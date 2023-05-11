#!/usr/bin/env bb

(require '[babashka.pods :as pods])

(pods/load-pod 'clj-kondo/clj-kondo "2023.04.14")

(ns diff-lint
  (:require [pod.borkdude.clj-kondo :as clj-kondo]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [babashka.cli :as cli]
            [taoensso.timbre :as timbre]))

(defn clj-kondo-lint-fn [path-prefix filename]
  (let [abs-path (.getAbsolutePath (io/file path-prefix filename))
        config-dir (.getAbsolutePath (io/file path-prefix ".clj-kondo"))
        lint-result (clj-kondo/run! {:lint [abs-path] :cache false :config-dir config-dir})
        findings-by-row (->> lint-result :findings (group-by :row))]
    (update-vals findings-by-row
                 (fn [findings]
                   (mapv (fn [{:keys [row col level message] :as _finding}]
                           {:id [col message]
                            :message (format "%s:%s:%s %s %s" abs-path row col (name level) message)})
                         findings)))))

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
    {:op op
     :a-start a-start
     :b-start b-start
     :a-end a-end
     :b-end b-end
     :b-next b-next
     :a-next a-next
     :post-offset (- a-next b-next)
     :pairs pairs
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
    (cond (nil? b) (persistent! mappings)
          (nil? h) (into (persistent! mappings) (map (fn [n] [n (+ n offset)]) b-lines))
          ;; delay the parsing of hunk header until we need it. Store the reslt at the head of the seq
          :else (let [{:keys [b-start b-end lookup post-offset] :as h} (if (map? h) h (parse-hunk-header h))]
                  (cond
                    (> b b-end) (recur post-offset mappings b-lines rest-hunks)
                    (< b b-start) (recur offset (assoc! mappings b (+ offset b)) rest-b-lines (cons h hunks))
                    :else (recur offset (assoc! mappings b (lookup b)) rest-b-lines (cons h hunks)))))))

(defn parse-diff [proj lines]
  (let [[old new & hunks] (->> lines (drop-while #(not (str/starts-with? % "---"))))
        a (subs old 4)
        b (subs new 4)]
    {:root proj
     :a (when-not (= a "/dev/null") (subs a 2)) ;; consume a/ 
     :b (when-not (= b "/dev/null") (subs b 2)) ;; consume b/
     :hunk-headers (filterv #(str/starts-with? % "@@") hunks)}))

(defn diff-data [proj]
  (->> (git-exec proj ["diff" "--unified=0" "HEAD"])
       (partition-by #(str/starts-with? % "diff"))
       (partition-all 2)
       (map (comp (partial parse-diff proj) second))))

(defn lint-for [{:keys [root] :as diff-data} k]
  (when-some [path (k diff-data)]
    (let [findings (clj-kondo-lint-fn root path)]
      (when (seq findings)
        findings))))

(defmacro run-with-clean-repo
  [proj & body]
  `(let [stash# (when (seq (git-exec ~proj ["diff-index" "HEAD"]))
                  (git-exec ~proj ["stash"]))]
     (try ~@body
          (finally (when stash# (git-exec ~proj ["stash" "pop" "--index"]))))))

(defn display-lint [{:keys [a b hunk-headers] :as diff} a-lint b-lint]
  (println (format "\n- %s %s\n" b (if (and a (not= a b)) (format "(%s)" a) "")))
  (when (seq b-lint)
    (let [b-lines (sort (keys b-lint))
          b-line->a-line-data (if (seq a-lint)
                                (let [m (linemap hunk-headers b-lines)]
                                  #(get a-lint (get m %)))
                                (constantly nil))]
      (doseq [b-line b-lines]
        (let [a-line-data (b-line->a-line-data b-line)
              a-line-ids (set (map :id a-line-data))
              b-line-data (get b-lint b-line)]
          (doseq [{:keys [message id]} b-line-data]
            (when-not (a-line-ids id)
              (println " " message))))))))

(defn main [proj _opts]
  (let [diffs (diff-data proj)]
    (timbre/info (format "Found %s diffs" (count diffs)))
    (when (seq diffs)
      (let [b-lints (mapv (partial lint-for :b) diffs)
            non-nil-b-lints (remove nil? b-lints)]
        (timbre/info (format "Computed %d linting results for current state of which %d suggested issues"
                             (count b-lints) (count non-nil-b-lints)))
        (when (seq non-nil-b-lints)
          (let [a-lints (run-with-clean-repo proj (mapv (partial lint-for :a) diffs))]
            (timbre/info (format "Computed %d linting results for previous state of which %d suggested issues"
                                 (count a-lints) (count (remove nil? a-lints))))
            (doseq [[diff a-lint b-lint] (map vector diffs a-lints b-lints)]
              (timbre/info (format "Computing diff lint for %s" (:b diff)))
              (display-lint diff a-lint b-lint)))))
      (println ""))))

(defn wrap-cli [f command-line-args]
  (let [{:keys [args opts]}
        (cli/parse-args command-line-args
                        {:alias {:v :verbose :h :help}
                         :coerce {:verbose :boolean :help :boolean}})
        log-level (if (:verbose opts) :info :warn)]
    (timbre/set-level! log-level)
    (if (or (:help opts) (empty? args))
      (println "https://github.com/beoliver/diff-lint/")
      (apply f (conj args opts)))))

(some->> *command-line-args*
         (wrap-cli main))
