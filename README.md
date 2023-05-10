# diff-lint
Only display lint errors that you have caused.

## What does it do?
1. Runs `git diff` to get the files that have changed.
2. Runs `clj-kondo` on the changed files.
3. Stashes any changes.
5. Runs `clj-kondo` on the same files (if present) as step 2.
6. Uses the diff from step 1 to compute which linting errors are new.
7. Restores the original state of the repository.
8. Displays the linting errors.

You can then click on an error message to open the file in your editor.

## Why?
I want to quickly see the linting warnings/errors that I have caused. I don't want to see the linting errors that were already present in the codebase.

I want to know (best effort) if I have **indirectly** introduced a linting error. For example, if I change a function signature, I want to know if I have caused any linting errors in the code that calls that function. This approach is not perfect, but it works for local (ie file) changes and is quite fast.

## How to use

I just have an alias in my `~/.zshrc`

```sh
alias dl='time bb -f ~/dev/beoliver/diff-lint/diff_lint.clj -- $PWD'
```
