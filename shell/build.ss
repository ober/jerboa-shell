#!/usr/bin/env gxi
(import :std/build-script
        :std/misc/path
        :std/misc/string)

;; Build tier: tiny (shell only), small (+eval), medium (+compiler), large (+coreutils)
(def tier (or (getenv "GSH_TIER" #f) "tiny"))

;; Library-only mode: compile modules without linking the executable
(def lib-only? (and (getenv "GSH_LIB_ONLY" #f) #t))

;; Collect .o files from a vendor directory for linking
(def (collect-vendor-objs subdir)
  (let ((dir (path-expand subdir (current-directory))))
    (if (file-exists? dir)
      (let* ((files (directory-files dir))
             (ofiles (filter (lambda (f) (string-suffix? ".o" f)) files)))
        (if (pair? ofiles)
          (string-join (map (lambda (f) (path-expand f dir)) ofiles) " ")
          ""))
      "")))

;; Collect gambitgsc .o files for linking (enables in-process compile-file)
;; Only for medium+ tiers that include the compiler
(def gambitgsc-ld-opts
  (if (member tier '("medium" "large"))
    (collect-vendor-objs "_vendor/gambitgsc")
    ""))

;; Collect gerbil-runtime .o files for linking (static builds, small+ tiers)
;; These embed missing runtime modules + .ssi data so the static binary
;; doesn't need -:~~=/path/to/gerbil
(def gerbil-runtime-ld-opts
  (if (and (getenv "GSH_STATIC" #f)
           (not (string=? tier "tiny")))
    (collect-vendor-objs "_vendor/gerbil-runtime")
    ""))

;; Collect gsh-dlopen .o files for linking (static builds, small+ tiers)
;; Provides dlopen/dlsym/dlclose/dlerror that override musl's weak stubs,
;; enabling .o1 loading in the static binary
(def gsh-dlopen-ld-opts
  (if (and (getenv "GSH_STATIC" #f)
           (not (string=? tier "tiny")))
    (collect-vendor-objs "_vendor/gsh-dlopen")
    ""))

;; GSH_STATIC=1 adds -static for static binary builds
(def static-ld-opts
  (if (getenv "GSH_STATIC" #f) "-static " ""))

;; pcre2 is needed by coreutils (large tier)
(def pcre2-ld-opts
  (if (string=? tier "large") "-lpcre2-8 " ""))

;; Core modules — always included regardless of tier
(def core-modules
  '("ffi"
    "util"
    "ast"
    "pregexp-compat"
    "environment"
    "lexer"
    "glob"
    "arithmetic"
    "expander"
    "parser"
    "redirect"
    "pipeline"
    "signals"
    "jobs"
    "registry"
    "macros"
    "builtins"
    "functions"
    "control"
    "executor"
    "history"
    "prompt"
    "fuzzy"
    "lineedit"
    "fzf"
    "completion"
    "static-compat"
    "script"
    "startup"))

;; Tier-specific modules
(def tier-modules
  (cond
    ((string=? tier "tiny")   '())
    ((string=? tier "small")  '())
    ((string=? tier "medium") '("compiler"))
    ((string=? tier "large")  '("compiler" "coreutils"))
    (else '("compiler" "coreutils"))))

;; Full module list depends on build mode
(def all-modules
  (if lib-only?
    ;; Library mode: core modules + lib entry point, no exe/stage
    (append core-modules '("lib"))
    ;; Executable mode: core + tier-specific + stage glue + exe
    (append core-modules
            tier-modules
            `("stage"
              (exe: "main" bin: "gsh" optimize: #t debug: 'env
                    "-ld-options" ,(string-append static-ld-opts pcre2-ld-opts gsh-dlopen-ld-opts " " gambitgsc-ld-opts " " gerbil-runtime-ld-opts))))))

(defbuild-script
  all-modules
  parallelize: (max 1 (quotient (##cpu-count) 2)))
