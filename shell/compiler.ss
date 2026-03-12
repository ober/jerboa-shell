;;; compiler.ss — Gerbil compiler integration for gsh
;;; Embeds the Gerbil compiler so users can compile and load .ss modules
;;; from within the shell, without needing a separate gxc installation.
;;;
;;; When gambitgsc is linked in, compile-file runs in-process (no external gsc).
;;; When gambitgsc is absent but external gsc exists, delegates to it.
;;; When neither is available, generates .scm/.ssi only (no native .o1).
;;;
;;; Self-registers at module init time:
;;;   - Sets *meta-command-handler* for ,compile / ,load / ,use / ,exports
;;;   - Registers gsc-compile, gsc-load, gsc-exports builtins

(export ensure-gerbil-compiler!
        gsc-available?
        embedded-compile-file?
        gsh-compile
        gsh-load
        gsh-use
        gsh-show-exports
        handle-meta-command)

(import :std/sugar
        :std/format
        :gerbil/runtime/init
        :gerbil/runtime/loader
        :gerbil/expander
        :gerbil/compiler
        :gsh/static-compat
        :gsh/util
        :gsh/registry
        :gsh/script)

;;; --- Lazy initialization ---

(def *gerbil-compiler-initialized* #f)

(def (ensure-gerbil-compiler!)
  "Initialize the Gerbil expander and compiler on first use.
   Called lazily to avoid startup cost for normal shell operations."
  (unless *gerbil-compiler-initialized*
    (set! *gerbil-compiler-initialized* #t)
    (ensure-static-compat!)
    (__load-gxi)
    (when (scm-only-load-module-active?)
      (patch-loader-post-gxi!))))

;;; --- Compiler detection ---

(def *embedded-compile-file-cached* 'unchecked)

(def (embedded-compile-file?)
  "Check if compile-file is available in-process (gambitgsc linked in)."
  (when (eq? *embedded-compile-file-cached* 'unchecked)
    (set! *embedded-compile-file-cached*
      (with-catch (lambda (e) #f)
        (lambda () (procedure? (eval 'compile-file))))))
  *embedded-compile-file-cached*)

(def (gsc-available?)
  "Check if native compilation is available (embedded or external gsc)."
  (or (embedded-compile-file?)
      (let ((gsc-env (getenv "GERBIL_GSC" #f)))
        (if gsc-env
          (file-exists? gsc-env)
          (file-exists?
           (path-expand "gsc" (path-expand "bin" (path-expand "~~"))))))))

;;; --- In-process .scm -> .o1 compilation ---

(def (compile-scm-files outdir modpath)
  "Compile generated .scm files to .o1 using in-process compile-file.
   modpath is the module path (e.g. \"gsh/bench\") — may include package prefix.
   Compiles modpath.scm and all modpath~N.scm parts."
  (let* ((cf (eval 'compile-file))
         ;; Module path uses / separator, maps to directory structure
         (scm-dir (path-expand (path-directory modpath) outdir))
         (basename (path-strip-directory modpath)))
    ;; Compile the main .scm
    (let ((main-scm (path-expand (string-append basename ".scm") scm-dir)))
      (when (file-exists? main-scm)
        (cf main-scm)))
    ;; Compile ~N parts
    (let loop ((n 0))
      (let ((part-scm (path-expand (string-append basename "~" (number->string n) ".scm") scm-dir)))
        (when (file-exists? part-scm)
          (cf part-scm)
          (loop (+ n 1)))))))

;;; --- Compile ---

(def (gsh-compile srcpath
                  output-dir: (output-dir #f)
                  invoke-gsc: (invoke-gsc 'auto)
                  verbose: (verbose #f))
  "Compile a Gerbil source file (.ss).
   Returns (values outdir native?)."
  (ensure-gerbil-compiler!)
  (let* ((srcpath (path-normalize srcpath))
         (outdir (or output-dir (path-directory srcpath)))
         (has-embedded? (embedded-compile-file?))
         (use-external-gsc?
           (if (eq? invoke-gsc 'auto)
             ;; If we have embedded compile-file, don't invoke external gsc
             (if has-embedded? #f
               (let ((gsc-env (getenv "GERBIL_GSC" #f)))
                 (if gsc-env
                   (file-exists? gsc-env)
                   (file-exists?
                    (path-expand "gsc" (path-expand "bin" (path-expand "~~")))))))
             invoke-gsc))
         (opts [output-dir: outdir
                invoke-gsc: use-external-gsc?
                keep-scm: #t
                verbose: verbose
                generate-ssxi: #f])
         (basename (path-strip-extension (path-strip-directory srcpath))))
    (compile-module srcpath opts)
    ;; If embedded compile-file, compile .scm -> .o1 for native code
    ;; On static binaries: set up a fake ~~ with embedded headers + gambuild-C,
    ;; then try compile-file. Even though this process can't dlopen .o1 (musl static),
    ;; the embedded compiler is exercised and produces real artifacts.
    (let* ((is-static? (static-binary?))
           (static-env-ok? (and is-static? has-embedded?
                                (ensure-static-compile-env!)))
           (try-native? (and has-embedded? (not use-external-gsc?)
                             (or (not is-static?) static-env-ok?)))
           (native-ok?
            (and try-native?
                 (let ((modpath (with-catch
                                 (lambda (e) basename)
                                 (lambda ()
                                   (let ((ctx (import-module srcpath)))
                                     (symbol->string (expander-context-id ctx)))))))
                   (with-catch
                    (lambda (e)
                      (fprintf (current-error-port)
                               "gsh: native compile failed, using .scm: ~a~n" e)
                      #f)
                    (lambda ()
                      (compile-scm-files outdir modpath)
                      #t))))))
      (fprintf (current-error-port) "compiled: ~a~a~n"
               srcpath (if native-ok? "" " (scm only)"))
      (values outdir native-ok?))))

;;; --- Load ---

(def (gsh-load modpath libdir: (libdir #f) native-preloaded: (native-preloaded #f))
  "Load a compiled Gerbil module and import into the eval context.
   modpath is like \"test-mod\" or \"mylib/foo\".
   When native-preloaded is #t, the module's .o1 was already loaded via
   load-module -- bind exports directly to avoid the expander re-evaluating
   the module source interpreted (which would overwrite native definitions)."
  (ensure-gerbil-compiler!)
  (when libdir
    (let ((dir (path-normalize libdir)))
      (unless (member dir (load-path))
        (add-load-path! dir))))
  (if native-preloaded
    ;; Module already loaded natively. Bind exports in the eval environment
    ;; without going through (eval '(import ...)) which would re-evaluate
    ;; the source interpreted via the expander.
    (let ((mod-sym (string->symbol (string-append ":" modpath))))
      (bind-module-exports! mod-sym)
      (fprintf (current-error-port) "loaded: ~a~n" modpath))
    ;; Fallback: standard import through expander
    (let ((mod-sym (string->symbol (string-append ":" modpath))))
      (eval `(import ,mod-sym))
      (fprintf (current-error-port) "loaded: ~a~n" modpath))))

(def (bind-module-exports! mod-sym)
  "Bind a module's runtime exports in the eval environment.
   Uses the expander's module context (from compile-module) to enumerate
   exports, then defines each name in the eval environment pointing to
   the native Gambit global set by the .o1 load."
  (let ((ctx (with-catch (lambda (e) #f)
               (lambda () (import-module mod-sym)))))
    (when ctx
      (for-each
       (lambda (exp)
         (when (and (module-export? exp) (eqv? (module-export-phi exp) 0))
           (let* ((name (module-export-name exp))
                  (binding (with-catch (lambda (e) #f)
                             (lambda () (core-resolve-module-export exp)))))
             (when (and binding (runtime-binding? binding))
               (let ((rid (runtime-binding-id binding)))
                 ;; Bind in eval env: (define name qualified-global)
                 (with-catch
                  (lambda (e) #f) ;; skip if binding fails
                  (lambda ()
                    (eval `(define ,name ,rid)))))))))
       (module-context-export ctx)))))

;;; --- Use (compile + load) ---

(def (gsh-use srcpath
              output-dir: (output-dir #f)
              verbose: (verbose #f))
  "Compile and load a Gerbil source file in one step.
   Returns the module path string."
  (ensure-gerbil-compiler!)
  (let* ((srcpath (path-normalize srcpath)))
    (let-values (((outdir native?) (gsh-compile srcpath
                                                  output-dir: output-dir
                                                  verbose: verbose)))
      (unless (member outdir (load-path))
        (add-load-path! outdir))
      (let ((modpath
             (with-catch
              (lambda (e)
                (path-strip-extension (path-strip-directory srcpath)))
              (lambda ()
                (let ((ctx (import-module srcpath)))
                  (symbol->string (expander-context-id ctx)))))))
        ;; Pre-load via load-module which uses load-path (finds .o1 files)
        ;; rather than gerbil.pkg source resolution. Then bind exports
        ;; directly in the eval env to avoid the expander re-evaluating
        ;; the module source interpreted (which would overwrite native defs).
        (let ((preloaded
               (and native?
                    (with-catch
                     (lambda (e) #f)
                     (lambda () (load-module modpath) #t)))))
          (gsh-load modpath native-preloaded: preloaded)
          modpath)))))

;;; --- Show exports ---

(def (gsh-show-exports modpath)
  "List the exports of a module."
  (ensure-gerbil-compiler!)
  (let ((ctx (with-catch (lambda (e) #f)
               (lambda ()
                 (import-module (string->symbol (string-append ":" modpath)))))))
    (if (not ctx)
      (begin
        (fprintf (current-error-port) "gsh: cannot resolve module ~a~n" modpath)
        1)
      (let ((exports (module-context-export ctx)))
        (for-each
         (lambda (exp)
           (when (and (module-export? exp) (eqv? (module-export-phi exp) 0))
             (let* ((name (module-export-name exp))
                    (binding (with-catch (lambda (e) #f)
                               (lambda () (core-resolve-module-export exp)))))
               (cond
                 ((and binding (runtime-binding? binding))
                  (let ((val (with-catch (lambda (e) #f)
                               (lambda () (eval name)))))
                    (if (procedure? val)
                      (fprintf (current-output-port) "  ~a\tprocedure~n" name)
                      (fprintf (current-output-port) "  ~a\tvalue~n" name))))
                 ((and binding (syntax-binding? binding))
                  (fprintf (current-output-port) "  ~a\tsyntax~n" name))
                 (else
                  (fprintf (current-output-port) "  ~a~n" name))))))
         exports)
        0))))

;;; --- Meta-command dispatch for ,compile / ,load / ,use / ,exports ---

(def (handle-meta-command expr-str)
  "Handle special meta-commands. Returns (cons result-string status) or #f."
  (cond
    ((meta-cmd-match expr-str "compile ")
     => (lambda (arg)
          (meta-cmd-wrap
           (lambda ()
             (let-values (((path verbose no-gsc) (parse-compile-args arg)))
               (gsh-compile path
                            invoke-gsc: (if no-gsc #f 'auto)
                            verbose: verbose))))))
    ((meta-cmd-match expr-str "load ")
     => (lambda (arg)
          (meta-cmd-wrap
           (lambda ()
             (let-values (((modpath libdir) (parse-load-args arg)))
               (gsh-load modpath libdir: libdir))))))
    ((meta-cmd-match expr-str "use ")
     => (lambda (arg)
          (meta-cmd-wrap
           (lambda ()
             (gsh-use (parse-path-arg arg))))))
    ((meta-cmd-match expr-str "exports ")
     => (lambda (arg)
          (meta-cmd-wrap
           (lambda ()
             (gsh-show-exports (string-trim-whitespace arg))))))
    (else #f)))

(def (meta-cmd-match str prefix)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)
       (substring str (string-length prefix) (string-length str))))

(def (meta-cmd-wrap thunk)
  (with-catch
   (lambda (e)
     (cons (call-with-output-string
            (lambda (port) (display-exception e port)))
           1))
   (lambda ()
     (thunk)
     (cons "" 0))))

(def (parse-path-arg arg)
  "Parse a path argument, stripping surrounding quotes if present."
  (let ((trimmed (string-trim-whitespace arg)))
    (if (and (> (string-length trimmed) 1)
             (char=? (string-ref trimmed 0) #\"))
      (with-catch
       (lambda (e) trimmed)
       (lambda () (call-with-input-string trimmed read)))
      trimmed)))

(def (parse-compile-args arg)
  "Parse: [-v] [-n] <path>.  Returns (values path verbose no-gsc)."
  (let loop ((parts (string-split arg #\space))
             (verbose #f) (no-gsc #f))
    (cond
      ((null? parts) (values "" verbose no-gsc))
      ((string=? (car parts) "") (loop (cdr parts) verbose no-gsc))
      ((string=? (car parts) "-v") (loop (cdr parts) #t no-gsc))
      ((string=? (car parts) "-n") (loop (cdr parts) verbose #t))
      (else
       (values (parse-path-arg (string-join parts " "))
               verbose no-gsc)))))

(def (parse-load-args arg)
  "Parse: <modpath> [-L dir].  Returns (values modpath libdir)."
  (let loop ((parts (string-split arg #\space))
             (modpath #f) (libdir #f))
    (cond
      ((null? parts) (values (or modpath "") libdir))
      ((string=? (car parts) "") (loop (cdr parts) modpath libdir))
      ((string=? (car parts) "-L")
       (if (pair? (cdr parts))
         (loop (cddr parts) modpath (cadr parts))
         (values (or modpath "") libdir)))
      (else
       (loop (cdr parts) (or modpath (car parts)) libdir)))))

;;; --- Self-registration (runs at module init time) ---

;; Set meta-command handler so ,compile / ,load / ,use / ,exports work
(*meta-command-handler* handle-meta-command)

;; Register compiler-related builtins
(builtin-register! "gsc-compile"
  (lambda (args env)
    (if (null? args)
      (begin
        (fprintf (current-error-port)
                 "usage: gsc-compile [-v] [-n] <file.ss>~n")
        2)
      (let loop ((args args) (verbose #f) (no-gsc #f))
        (cond
          ((null? args)
           (fprintf (current-error-port)
                    "gsh: gsc-compile: missing file argument~n")
           2)
          ((string=? (car args) "-v") (loop (cdr args) #t no-gsc))
          ((string=? (car args) "-n") (loop (cdr args) verbose #t))
          (else
           (with-catch
            (lambda (e)
              (fprintf (current-error-port) "gsh: gsc-compile: ~a~n"
                       (call-with-output-string
                        (lambda (p) (display-exception e p))))
              1)
            (lambda ()
              (gsh-compile (car args)
                           invoke-gsc: (if no-gsc #f 'auto)
                           verbose: verbose)
              0))))))))

(builtin-register! "gsc-load"
  (lambda (args env)
    (if (null? args)
      (begin
        (fprintf (current-error-port)
                 "usage: gsc-load <modpath> [-L dir]~n")
        2)
      (let loop ((args args) (modpath #f) (libdir #f))
        (cond
          ((null? args)
           (if modpath
             (with-catch
              (lambda (e)
                (fprintf (current-error-port) "gsh: gsc-load: ~a~n"
                         (call-with-output-string
                          (lambda (p) (display-exception e p))))
                1)
              (lambda ()
                (gsh-load modpath libdir: libdir)
                0))
             (begin
               (fprintf (current-error-port)
                        "gsh: gsc-load: missing module path~n")
               2)))
          ((string=? (car args) "-L")
           (if (pair? (cdr args))
             (loop (cddr args) modpath (cadr args))
             (begin
               (fprintf (current-error-port)
                        "gsh: gsc-load: -L requires directory argument~n")
               2)))
          (else
           (loop (cdr args) (or modpath (car args)) libdir)))))))

(builtin-register! "gsc-exports"
  (lambda (args env)
    (if (null? args)
      (begin
        (fprintf (current-error-port)
                 "usage: gsc-exports <modpath>~n")
        2)
      (with-catch
       (lambda (e)
         (fprintf (current-error-port) "gsh: gsc-exports: ~a~n"
                  (call-with-output-string
                   (lambda (p) (display-exception e p))))
         1)
       (lambda ()
         (gsh-show-exports (car args)))))))
