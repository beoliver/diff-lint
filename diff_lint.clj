#!/usr/bin/env bb

(require '[babashka.pods :as pods])

(pods/load-pod 'clj-kondo/clj-kondo "2023.04.14")

(ns diff-lint
  (:require [pod.borkdude.clj-kondo :as clj-kondo]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn- shell-lines [args]
  (->> (apply sh args) :out str/split-lines))

(defn git-exec [proj args]
  (->> (into ["git" "--no-pager" "-C" proj] args)
       (shell-lines)
       (remove str/blank?)))

(defn- parse-hunk-header [header]
  (let [[[a-start a-lines] [b-start b-lines] :as pairs]
        (->> (str/split header #" ")
             (drop 1)
             (take 2)
             (mapv (fn [s]
                     (let [[start-line num-lines]
                           (->> (str/split s #",")
                                (mapv #(abs (Integer/parseInt %))))]
                       [start-line (or num-lines 1)]))))
        op (cond
             (zero? a-lines) :create
             (zero? b-lines) :delete
             :else :update)
        lookup (zipmap (range b-start (+ b-start b-lines))
                       (range a-start (+ a-start a-lines)))
        a-end (case op
                :create a-start
                (+ a-start (- a-lines 1)))
        b-end (case op
                :delete b-start
                (+ b-start (- b-lines 1)))
        b-next (+ b-end 1)
        a-next (+ a-end 1)]
    {:op      op
     :a-start a-start
     :b-start b-start
     :a-end   a-end
     :b-end   b-end
     :b-next  b-next
     :a-next  a-next
     :post-offset (- a-next b-next)
     :pairs   pairs
     :lookup lookup}))

;; Given a series of c(hunk) headers for example
;; @@ -0,0 +1 @@
;; @@ -108 +109 @@
;; @@ -119,4 +120,2 @@
;; @@ -127 +126 @@
;; @@ -144,0 +144 @@
;; and a strictly increasing sequence of line numbers [n_1, ..., n_k] 
;; where n_i < n_(i+1) for all i in [1, k]. For each input line number we wish to know 
;; the corresponding line number (if applicable) in the previous version of the file. 
;; The function should return a mapping of the input line numbers to the corresponding 
;; line numbers in the previous version.
;; The mapping should return `nil` if no line number exists in the previous version. 

;; For example, given the above headers and the sequence [1, 5, 109, 115, 144]
;; we could get back {5 4, 109 108, 115 114}

(defn linemap [hunks b-lines]
  (loop [offset 0
         mappings (transient {})
         [b & rest-b-lines :as b-lines] b-lines
         [h & rest-hunks :as hunks] hunks]
    (cond (nil? b)  (persistent! mappings)
          (nil? h)  (into (persistent! mappings) (map (fn [n] [n (+ n offset)]) b-lines))
          ;; delay the parsing of hunk header until we need it. Store the reslt at the head of the seq
          :else (let [{:keys [b-start b-end lookup post-offset] :as h} (if (map? h) h (parse-hunk-header h))]
                  (cond
                    (> b b-end)   (recur post-offset mappings b-lines rest-hunks)
                    (< b b-start) (recur offset (assoc! mappings b (+ offset b)) rest-b-lines (cons h hunks))
                    :else         (recur offset (assoc! mappings b (lookup b)) rest-b-lines (cons h hunks)))))))

(defn extract-headers-from-unified-patch [lines]
  (filterv #(str/starts-with? % "@@") lines))

(defn clj-kondo-lint-fn [project-to-check filename]
  (let [lint-result (clj-kondo/run! {:lint [filename]
                                     :cache false
                                     :config-dir (str project-to-check "/.clj-kondo")})
        findings-by-row (->> lint-result :findings (group-by :row))]
    (update-vals
     findings-by-row
     (fn [findings]
       (mapv (fn [{:keys [filename row col level message] :as _finding}]
               {:id [col message]
                :message (format "%s/%s:%s:%s %s %s" project-to-check filename row col (name level) message)})
             findings)))))

(defmacro run-with-clean-repo ^{:style/indent 1} [proj & body]
  `(let [inverse-ops# (atom nil)]
     (try
       (git-exec ~proj ["commit" "--allow-empty" "-m" "TEMP"])
       (swap! inverse-ops# conj ["reset" "--soft" "HEAD^"])
       (when (seq (git-exec ~proj ["diff" "--name-only"]))
         (git-exec ~proj ["stash"]) ;; "save" "--keep-index" "--include-untracked" "TEMP STASH"])
         (swap! inverse-ops# conj ["stash" "pop"]))
       (git-exec ~proj ["checkout" "HEAD^"])
       (swap! inverse-ops# conj ["checkout" "-"])
       ~@body
       (finally
         (run! (partial git-exec ~proj) @inverse-ops#)))))

(defn main [proj]
  (let [staged (git-exec proj ["diff" "--name-only" "--cached"])
        unstaged (git-exec proj ["diff" "--name-only"])
        staged-and-unstaged (into (set staged) unstaged)
        current-lint-errors (->> staged-and-unstaged
                                 (keep (fn [file]
                                         (let [findings (clj-kondo-lint-fn proj file)]
                                           (when (seq findings)
                                             [file {:findings findings
                                                    :full-path (str proj "/" file)
                                                    :patch-headers (-> (git-exec proj ["diff" "--unified=0" "HEAD" "--" file])
                                                                       (extract-headers-from-unified-patch))}]))))
                                 (into {}))]
    (when (seq current-lint-errors)
      (run-with-clean-repo
       proj
       (doseq [[path {:keys [full-path findings patch-headers]}] current-lint-errors]
         (println "\n-" path "\n")
         (let [old-findings (when (.exists (io/file full-path)) (clj-kondo-lint-fn proj path))]
           (if (seq old-findings)
             (let [lines-of-interest (sort (keys findings))
                   new-to-old-mappings (linemap patch-headers lines-of-interest)]
               (doseq [line lines-of-interest]
                 (let [old-line (get new-to-old-mappings line)
                       old-line-findings-ids (set (map :id (get old-findings old-line [])))]
                   (doseq [{:keys [message id]} (get findings line)]
                     (when-not (old-line-findings-ids id)
                       (println message))))))
             (doseq [[line-findings] (sort-by key findings)]
               (doseq [message (map :message line-findings)]
                 (println message)) findings))))
       (println "")))))

(some-> (first *command-line-args*)
        (main))
