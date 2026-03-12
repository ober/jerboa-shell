#!chezscheme
;;; build-jsh.ss — Compile src/jsh/*.sls modules (jsh — no Gherkin/submodule needed)
;;;
;;; Usage: scheme -q --libdirs src:<jerboa-lib>:<gherkin-src> --compile-imported-libraries < build-jsh.ss

(import
  (except (chezscheme) void box box? unbox set-box!
          andmap ormap iota last-pair find
          1+ 1- fx/ fx1+ fx1-
          error error? raise with-exception-handler identifier?
          hash-table? make-hash-table))

(define (compile-jsh-module name)
  (let ((sls (string-append "src/jsh/" name ".sls")))
    (if (file-exists? sls)
      (begin
        (printf "  Compiling ~a...~n" sls)
        (compile-library sls))
      (printf "  SKIP (not found): ~a~n" sls))))

;; Compile in dependency order (tiers match gerbil-shell module graph)
(printf "=== Compiling jsh modules (optimize-level 2) ===~n")
(parameterize ([optimize-level 2]
               [generate-inspector-information #f])
  ;; Compat layer first
  (compile-jsh-module "../compat/gambit")
  ;; Tier 1: no deps
  (for-each compile-jsh-module '("ast" "registry"))
  ;; Tier 2
  (for-each compile-jsh-module '("macros" "util"))
  ;; Tier 3
  (for-each compile-jsh-module
    '("environment" "lexer" "arithmetic" "glob" "fuzzy" "history"
      "pregexp-compat" "static-compat" "stage"))
  ;; Tier 4
  (for-each compile-jsh-module '("parser" "functions" "signals" "expander"))
  ;; Tier 5
  (for-each compile-jsh-module '("redirect" "control" "jobs" "builtins"))
  ;; Tier 6
  (for-each compile-jsh-module '("pipeline" "executor" "completion" "prompt"))
  ;; Tier 7
  (for-each compile-jsh-module '("lineedit" "fzf" "script" "startup" "main")))

(printf "~n=== jsh module compilation complete ===~n")
