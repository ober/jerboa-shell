#!chezscheme
;;; build-jerboa.ss — Compile gerbil-shell .ss modules using the Gherkin compiler
;;; with Jerboa imports (instead of Gherkin's compat/runtime modules)
;;;
;;; Usage: scheme -q --libdirs src:<jerboa-lib>:<gherkin-src> --compile-imported-libraries < build-jerboa.ss
;;;
;;; The Gherkin compiler runs against gherkin's runtime, but the OUTPUT .sls files
;;; import from Jerboa's module paths: (jerboa runtime), (std ...), etc.

(import
  (except (chezscheme) void box box? unbox set-box!
          andmap ormap iota last-pair find
          1+ 1- fx/ fx1+ fx1-
          error error? raise with-exception-handler identifier?
          hash-table? make-hash-table)
  (compiler compile))

;; --- Configuration ---
(define submodule-dir "gerbil-shell")
(define output-dir "src/jsh")

;; Find source file: check local override first, then submodule
(define (find-source name)
  (let ((local (string-append "./" name ".ss"))
        (sub   (string-append submodule-dir "/" name ".ss")))
    (cond
      ((file-exists? local) local)
      ((file-exists? sub)   sub)
      (else (error 'find-source "source file not found" name)))))

;; --- Import map: Gerbil module → Jerboa library ---
;; KEY DIFFERENCE from gherkin-shell: maps to (std ...) / (jerboa ...) paths
(define jsh-import-map
  '(;; Standard library → Jerboa stdlib
    (:std/sugar        . (std sugar))
    (:std/format       . (std format))
    (:std/sort         . (std sort))
    (:std/pregexp      . (std pregexp))
    (:std/misc/string  . (std misc string))
    (:std/misc/list    . (std misc list))
    (:std/misc/path    . (std os path))
    (:std/misc/hash    . (jerboa runtime))
    (:std/iter         . #f)  ;; stripped — Gherkin compiles for-loops natively
    (:std/error        . (std error))
    (:std/os/signal    . (std os signal))
    (:std/os/signal-handler . (std os signal))
    (:std/os/fdio      . (std os fdio))
    (:std/srfi/1       . (std misc list))
    (:std/foreign      . #f)  ;; stripped
    (:std/build-script . #f)  ;; stripped
    ;; Gerbil runtime
    (:gerbil/core      . #f)  ;; stripped
    (:gerbil/runtime   . #f)  ;; stripped
    (:gerbil/runtime/init . #f)
    (:gerbil/runtime/loader . #f)
    (:gerbil/expander   . #f)
    (:gerbil/compiler   . #f)
    ;; Relative imports
    ("./pregexp-compat" . (jsh pregexp-compat))
    ;; gsh module mappings → jsh
    (:gsh/arithmetic   . (jsh arithmetic))
    (:gsh/ast          . (jsh ast))
    (:gsh/builtins     . (jsh builtins))
    (:gsh/completion   . (jsh completion))
    (:gsh/control      . (jsh control))
    (:gsh/environment  . (jsh environment))
    (:gsh/executor     . (jsh executor))
    (:gsh/expander     . (jsh expander))
    (:gsh/ffi          . (jsh ffi))
    (:gsh/functions    . (jsh functions))
    (:gsh/fuzzy        . (jsh fuzzy))
    (:gsh/fzf          . (jsh fzf))
    (:gsh/glob         . (jsh glob))
    (:gsh/history      . (jsh history))
    (:gsh/jobs         . (jsh jobs))
    (:gsh/lexer        . (jsh lexer))
    (:gsh/lineedit     . (jsh lineedit))
    (:gsh/macros       . (jsh macros))
    (:gsh/main         . (jsh main))
    (:gsh/parser       . (jsh parser))
    (:gsh/pipeline     . (jsh pipeline))
    (:gsh/pregexp-compat . (jsh pregexp-compat))
    (:gsh/prompt       . (jsh prompt))
    (:gsh/redirect     . (jsh redirect))
    (:gsh/registry     . (jsh registry))
    (:gsh/script       . (jsh script))
    (:gsh/signals      . (jsh signals))
    (:gsh/stage        . (jsh stage))
    (:gsh/startup      . (jsh startup))
    (:gsh/static-compat . (jsh static-compat))
    (:gsh/util         . (jsh util))
    ))

;; --- Base imports for all compiled modules ---
;; KEY DIFFERENCE: uses (jerboa runtime) instead of (runtime util/table/mop/hash)
;; and (compat gambit) from local src/compat/ instead of gherkin's
(define jsh-base-imports
  '((except (chezscheme) box box? unbox set-box!
            andmap ormap iota last-pair find
            1+ 1- fx/ fx1+ fx1-
            error? raise with-exception-handler identifier?
            hash-table? make-hash-table
            sort sort! path-extension
            printf fprintf
            ;; Exclude Chez builtins that (compat gambit) replaces
            file-directory? file-exists? getenv close-port
            ;; Chez void takes 0 args; Gerbil's is variadic
            void
            ;; Gambit-compatible: handles /dev/fd/N and keyword args
            open-output-file open-input-file)
    ;; Jerboa runtime provides: hash tables, keywords, errors, utilities
    ;; Exclude method dispatch (bind-method!, call-method) — MOP's version
    ;; must be used since MOP's make-instance uses class-type-methods table
    ;; Exclude void — (jerboa runtime) re-exports Chez's 0-arg void,
    ;; but Gerbil's void is variadic.  Let (compat gambit)'s version win.
    (except (jerboa runtime) bind-method! call-method ~ void)
    ;; Gherkin MOP: make-class-type, object::t, slot-ref/set!, class predicates
    ;; The Gherkin compiler emits calls to these for every defstruct/defclass.
    ;; TODO: Port MOP into jerboa to eliminate this gherkin dependency
    (runtime mop)
    ;; Gherkin runtime util: last, iota, andmap, ormap, foldl, etc.
    ;; Not re-exported by (runtime mop) but used by compiled Gerbil code
    (runtime util)

    ;; Import most of (compat gambit) — u8vector, threading, etc.
    ;; Exclude names that conflict with (chezscheme) builtins we still need:
    (except (compat gambit) number->string make-mutex
            with-output-to-string)
    ;; Std error for Error type, error predicates
    (std error)
    ;; Std misc for string-split, string-join, string-prefix?, path-expand, etc.
    (std misc string)
    (std misc list)
    (std misc alist)
    (std os path)
    (std format)
    (std sort)
    (std pregexp)
    ))

;; --- Import conflict resolution ---
;; (identical to gherkin-shell's — this is compiler infrastructure, not runtime)
(define (fix-import-conflicts lib-form)
  (let* ((lib-name (cadr lib-form))
         (export-clause (caddr lib-form))
         (import-clause (cadddr lib-form))
         (body (cddddr lib-form))
         (imports (cdr import-clause))
         (local-defs
           (let lp ((forms body) (names '()))
             (if (null? forms)
               names
               (lp (cdr forms)
                   (append (extract-def-names (car forms)) names)))))
         (all-earlier-names
           (let lp ((imps imports) (seen '()) (result '()))
             (if (null? imps)
               (reverse result)
               (let* ((imp (car imps))
                      (lib (get-import-lib-name imp))
                      (exports (if lib
                                 (or (begin (ensure-library-loaded lib)
                                       (guard (e (#t #f))
                                         (library-exports lib)))
                                     (read-sls-exports lib)
                                     '())
                                 '()))
                      (provided (cond
                                  ((and (pair? imp) (eq? (car imp) 'except))
                                   (filter (lambda (s) (not (memq s (cddr imp))))
                                           exports))
                                  ((and (pair? imp) (eq? (car imp) 'only))
                                   (cddr imp))
                                  (else exports))))
                 (lp (cdr imps)
                     (append provided seen)
                     (cons seen result)))))))
    (let ((fixed-imports
            (map (lambda (imp earlier-names)
                   (fix-one-import imp
                     (append local-defs earlier-names)))
                 imports all-earlier-names)))
      (let ((fixed-body (fix-assigned-exports
                          (cdr export-clause)
                          (list (cons 'import fixed-imports))
                          body)))
        `(library ,lib-name ,export-clause
          (import ,@fixed-imports) ,@fixed-body)))))

;; Fix exported variables that are set!'d (R6RS forbids this)
(define (fix-assigned-exports exports import-forms body)
  (let ((assigned-names
          (let lp ((tree body) (names '()))
            (cond
              ((not (pair? tree)) names)
              ((and (eq? (car tree) 'set!)
                    (pair? (cdr tree))
                    (symbol? (cadr tree))
                    (memq (cadr tree) exports)
                    (not (memq (cadr tree) names)))
               (cons (cadr tree) names))
              (else
               (lp (cdr tree) (lp (car tree) names)))))))
    (if (null? assigned-names)
      body
      (let ((new-body
              (let lp ((forms body) (result '()))
                (if (null? forms)
                  (reverse result)
                  (let ((form (car forms)))
                    (cond
                      ((and (pair? form)
                            (eq? (car form) 'define)
                            (let ((def-name (if (pair? (cadr form)) (caadr form) (cadr form))))
                              (and (symbol? def-name) (memq def-name assigned-names))))
                       (let* ((def-name (if (pair? (cadr form)) (caadr form) (cadr form)))
                              (init (if (pair? (cadr form))
                                      `(lambda ,(cdadr form) ,@(cddr form))
                                      (if (pair? (cddr form)) (caddr form) '(void))))
                              (cell-name (string->symbol
                                           (string-append (symbol->string def-name) "-cell"))))
                         (lp (cdr forms)
                             (append
                               (list
                                 `(define-syntax ,def-name
                                    (identifier-syntax
                                      (id (vector-ref ,cell-name 0))
                                      ((set! id v) (vector-set! ,cell-name 0 v))))
                                 `(define ,cell-name (vector ,init)))
                               result))))
                      (else
                       (lp (cdr forms) (cons form result)))))))))
        new-body))))

(define (extract-def-names form)
  (cond
    ((not (pair? form)) '())
    ((eq? (car form) 'define)
     (cond
       ((symbol? (cadr form)) (list (cadr form)))
       ((pair? (cadr form)) (list (caadr form)))
       (else '())))
    ((eq? (car form) 'define-syntax)
     (if (symbol? (cadr form)) (list (cadr form)) '()))
    ((eq? (car form) 'begin)
     (let lp ((forms (cdr form)) (names '()))
       (if (null? forms) names
           (lp (cdr forms) (append (extract-def-names (car forms)) names)))))
    (else '())))

(define (ensure-library-loaded lib-name)
  (guard (e (#t #f))
    (eval `(import ,lib-name) (interaction-environment))
    #t))

(define (read-sls-exports lib-name)
  (let ((path (lib-name->sls-path lib-name)))
    (if (and path (file-exists? path))
      (guard (e (#t #f))
        (call-with-input-file path
          (lambda (port)
            (let ((first (read port)))
              (let ((lib-form (if (and (pair? first) (eq? (car first) 'library))
                                first
                                (read port))))
                (if (and (pair? lib-form) (eq? (car lib-form) 'library))
                  (let ((export-clause (caddr lib-form)))
                    (if (and (pair? export-clause) (eq? (car export-clause) 'export))
                      (cdr export-clause)
                      #f))
                  #f))))))
      #f)))

(define (lib-name->sls-path lib-name)
  (cond
    ((and (pair? lib-name) (= (length lib-name) 2)
          (eq? (car lib-name) 'jsh))
     (string-append output-dir "/" (symbol->string (cadr lib-name)) ".sls"))
    ((and (pair? lib-name) (= (length lib-name) 2)
          (eq? (car lib-name) 'compat))
     (string-append "src/compat/" (symbol->string (cadr lib-name)) ".sls"))
    (else #f)))

(define (fix-one-import imp local-defs)
  (let ((lib-name (get-import-lib-name imp)))
    (if (not lib-name)
      imp
      (let* ((_load (ensure-library-loaded lib-name))
             (lib-exports (or (guard (e (#t #f)) (library-exports lib-name))
                              (read-sls-exports lib-name)
                              '()))
             (conflicts (filter (lambda (d) (memq d lib-exports))
                                local-defs)))
            (if (null? conflicts)
              imp
              (cond
                ((and (pair? imp) (eq? (car imp) 'except))
                 (let ((existing (cddr imp)))
                   `(except ,(cadr imp)
                      ,@existing
                      ,@(filter (lambda (d) (not (memq d existing)))
                                conflicts))))
                ((and (pair? imp) (eq? (car imp) 'only))
                 (let ((kept (filter (lambda (s) (not (memq s conflicts)))
                                     (cddr imp))))
                   `(only ,(cadr imp) ,@kept)))
                ((pair? imp)
                 `(except ,imp ,@conflicts))
                (else imp)))))))

(define (get-import-lib-name spec)
  (cond
    ((and (pair? spec)
          (memq (car spec) '(except only rename prefix)))
     (get-import-lib-name (cadr spec)))
    ((and (pair? spec) (symbol? (car spec)))
     spec)
    (else #f)))

;; --- Module compilation ---
(define (compile-module name)
  (let* ((input-path (find-source name))
         (output-path (string-append output-dir "/" name ".sls"))
         (lib-name `(jsh ,(string->symbol name))))
    (display (string-append "  Compiling: " name ".ss → " name ".sls\n"))
    (guard (exn
             (#t (display (string-append "  ERROR: " name ".ss failed: "))
                 (display (condition-message exn))
                 (when (irritants-condition? exn)
                   (display " — ")
                   (display (condition-irritants exn)))
                 (newline)
                 #f))
      (let* ((lib-form (gerbil-compile-to-library
                         input-path lib-name
                         jsh-import-map jsh-base-imports))
             (lib-form (fix-import-conflicts lib-form)))
        (call-with-output-file output-path
          (lambda (port)
            (display "#!chezscheme\n" port)
            (parameterize ([print-gensym #f])
              (pretty-print lib-form port)))
          'replace)
        (display (string-append "  OK: " output-path "\n"))
        #t))))

;; --- Main ---
(display "=== Jerboa Shell Builder ===\n\n")

;; Tier 1: No dependencies on other jsh modules
(display "--- Tier 1: Foundation ---\n")
(compile-module "ast")
(compile-module "registry")

;; Tier 2: Depends on Tier 1
(display "\n--- Tier 2: Core ---\n")
(compile-module "macros")
(compile-module "util")

;; Tier 3: Depends on Tier 1-2
(display "\n--- Tier 3: Modules ---\n")
(compile-module "environment")
(compile-module "lexer")
(compile-module "arithmetic")
(compile-module "glob")
(compile-module "fuzzy")
(compile-module "history")

;; Tier 4: Depends on Tier 1-3
(display "\n--- Tier 4: Processing ---\n")
(compile-module "parser")
(compile-module "functions")
(compile-module "signals")
(compile-module "expander")

;; Tier 5: Depends on Tier 1-4
(display "\n--- Tier 5: Execution ---\n")
(compile-module "redirect")
(compile-module "control")
(compile-module "jobs")
(compile-module "builtins")

;; Tier 6: Depends on Tier 1-5
(display "\n--- Tier 6: UI ---\n")
(compile-module "pipeline")
(compile-module "executor")
(compile-module "completion")
(compile-module "prompt")

;; Tier 7: Depends on all
(display "\n--- Tier 7: Top-level ---\n")
(compile-module "lineedit")
(compile-module "fzf")
(compile-module "script")
(compile-module "startup")
(compile-module "main")

;; --- Post-build: Force library invocation for side-effecting modules ---
(display "\n--- Post-build: Patching for Chez lazy invocation ---\n")
(let ()
  (define (string-find haystack needle)
    (let ((hlen (string-length haystack))
          (nlen (string-length needle)))
      (let loop ((i 0))
        (cond
          ((> (+ i nlen) hlen) #f)
          ((string=? (substring haystack i (+ i nlen)) needle) i)
          (else (loop (+ i 1)))))))
  (let* ((path "src/jsh/main.sls")
         (content (call-with-input-file path
                    (lambda (p) (get-string-all p)))))
    (let ((needle (string #\newline #\space #\space #\( #\d #\e #\f #\i #\n #\e #\space)))
      (let ((idx (string-find content needle)))
        (if idx
          (begin
            (call-with-output-file path
              (lambda (p)
                (display (substring content 0 idx) p)
                (display "\n  ;; Force invocation of (jsh builtins) for defbuiltin registration\n" p)
                (display "  (define _force-builtins special-builtin?)" p)
                (display (substring content idx (string-length content)) p))
              'replace)
            (display "  Patched main.sls for lazy invocation\n"))
          (display "  WARNING: Could not find insertion point in main.sls\n"))))))

;; --- Post-build patches ---
(display "\n--- Post-build: Applying patches ---\n")
(let ()
  (define (string-find haystack needle)
    (let ((hlen (string-length haystack))
          (nlen (string-length needle)))
      (let loop ((i 0))
        (cond
          ((> (+ i nlen) hlen) #f)
          ((string=? (substring haystack i (+ i nlen)) needle) i)
          (else (loop (+ i 1)))))))
  (define (patch-file! path old new)
    (let* ((content (call-with-input-file path
                      (lambda (p) (get-string-all p))))
           (idx (string-find content old)))
      (if idx
        (begin
          (call-with-output-file path
            (lambda (p)
              (display (substring content 0 idx) p)
              (display new p)
              (display (substring content (+ idx (string-length old))
                                 (string-length content)) p))
            'replace)
          (printf "  Patched ~a~n" path)
          #t)
        (begin
          (printf "  (skip) ~a~n" path)
          #f))))

  ;; Fix env-push-scope: keyword-style args → positional
  (patch-file! "src/jsh/environment.sls"
    "(define (env-push-scope env)\n    (make-shell-environment\n      'parent:\n      env\n      'name:\n      (shell-environment-shell-name env)))"
    "(define (env-push-scope env)\n    (make-shell-environment env (shell-environment-shell-name env)))")

  ;; Fix env-clone: no-arg constructor + manual set
  (patch-file! "src/jsh/environment.sls"
    "(define (env-clone env)\n    (let ([clone (make-shell-environment\n                   'name:\n                   (shell-environment-shell-name env))])"
    "(define (env-clone env)\n    (let ([clone (let ([e (make-shell-environment)])\n                   (shell-environment-shell-name-set! e (shell-environment-shell-name env))\n                   e)])")

  ;; Fix exception-message: Chez condition-message returns raw format templates
  (patch-file! "src/jsh/util.sls"
    "(define (exception-message e)\n    (cond\n      [(Error? e) (Error-message e)]\n      [(error-exception? e) (error-exception-message e)]\n      [(string? e) e]\n      [(os-exception? e)\n       (call-with-output-string\n         (lambda (p) (display-exception e p)))]\n      [else\n       (call-with-output-string\n         (lambda (p) (display-exception e p)))]))"
    "(define (exception-message e)\n    (define (format-condition e)\n      (let ([msg (call-with-string-output-port\n                   (lambda (p) (display-condition e p)))])\n        (if (and (> (string-length msg) 11)\n                 (string=? (substring msg 0 11) \"Exception: \"))\n            (substring msg 11 (string-length msg))\n            (if (and (> (string-length msg) 13)\n                     (string=? (substring msg 0 13) \"Exception in \"))\n                (let loop ([i 13])\n                  (cond\n                    [(>= (+ i 1) (string-length msg)) msg]\n                    [(and (char=? (string-ref msg i) #\\:)\n                          (char=? (string-ref msg (+ i 1)) #\\space))\n                     (substring msg (+ i 2) (string-length msg))]\n                    [else (loop (+ i 1))]))\n                msg))))\n    (cond\n      [(string? e) e]\n      [(condition? e) (format-condition e)]\n      [else (call-with-string-output-port\n              (lambda (p) (display e p)))]))")

  ;; Fix make-mutex: Chez requires symbol or #f, not strings
  (patch-file! "src/jsh/pipeline.sls"
    "(make-mutex \"pipeline-fd\")"
    "(make-mutex 'pipeline-fd)")

  ;; --- Performance optimizations ---

  ;; Add PATH lookup cache to util.sls (which-cached)
  (patch-file! "src/jsh/util.sls"
    "string-last-index-of which find-file-in-path executable?"
    "string-last-index-of which which-cached which-cache-invalidate!\n   find-file-in-path executable?")

  (patch-file! "src/jsh/util.sls"
    "(define find-file-in-path"
    (string-append
      ";; PATH lookup cache — equivalent to bash's command_hash\n"
      "  (define *which-cache* (make-hashtable string-hash string=?))\n"
      "  (define *which-cache-path* #f)\n"
      "  (define (which-cache-invalidate!)\n"
      "    (hashtable-clear! *which-cache*)\n"
      "    (set! *which-cache-path* #f))\n"
      "  (define (which-cached name)\n"
      "    (if (string-contains? name \"/\")\n"
      "        (which name)\n"
      "        (let ([current-path (or (getenv \"PATH\" #f) \"/usr/bin:/bin\")])\n"
      "          (unless (equal? current-path *which-cache-path*)\n"
      "            (hashtable-clear! *which-cache*)\n"
      "            (set! *which-cache-path* current-path))\n"
      "          (let ([cached (hashtable-ref *which-cache* name #f)])\n"
      "            (or cached\n"
      "                (let ([found (which name)])\n"
      "                  (when found (hashtable-set! *which-cache* name found))\n"
      "                  found))))))\n"
      "  (define find-file-in-path"))

  ;; Replace (which cmd-name) with (which-cached cmd-name) in executor.sls
  ;; Use a helper to replace all occurrences
  (let ((path "src/jsh/executor.sls"))
    (let* ((content (call-with-input-file path
                      (lambda (p) (get-string-all p))))
           (old "(which cmd-name)")
           (new "(which-cached cmd-name)")
           (olen (string-length old))
           (nlen (string-length new)))
      (let loop ((i 0) (result ""))
        (cond
          ((> (+ i olen) (string-length content))
           (let ((final (string-append result (substring content i (string-length content)))))
             (call-with-output-file path
               (lambda (p) (display final p))
               'replace)
             (printf "  Patched executor.sls: which -> which-cached~n")))
          ((string=? (substring content i (+ i olen)) old)
           (loop (+ i olen) (string-append result new)))
          (else
           (loop (+ i 1) (string-append result (substring content i (+ i 1)))))))))

  ;; env-get single-pass: eliminate double hash-table scan in env-get
  (patch-file! "src/jsh/environment.sls"
    (string-append
      "      [else\n"
      "       (let ([resolved (resolve-nameref name env)])\n"
      "         (let ([var (find-var-in-chain env resolved)])\n"
      "           (if (and var (shell-var-nameref? var))\n"
      "               #f\n"
      "               (env-get-chain env resolved))))])")
    (string-append
      "      [else\n"
      "       (let ([resolved (resolve-nameref name env)])\n"
      "         (let ([var (find-var-in-chain env resolved)])\n"
      "           (cond\n"
      "             [(not var) (getenv resolved #f)]\n"
      "             [(shell-var-nameref? var) #f]\n"
      "             [else (shell-var-scalar-value var)])))])")))
  
  ;; Rename all *gsh-* parameters to *jsh-* throughout all generated files  
  (system "sed -i 's/\\*gsh-/\\*jsh-/g' src/jsh/*.sls")
  (display "  Patched all .sls files (gsh-* → jsh-*)\n"))

(display "\n=== Build complete ===\n")
