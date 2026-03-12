;;; lib.ss — Library API for embedding jsh in Gerbil projects
;;;
;;; Quick start:
;;;   (import :jsh/lib)
;;;   (def env (jsh-init!))
;;;   (jsh-run "echo hello" env)           ;; prints "hello", returns 0
;;;   (jsh-capture "echo hello" env)       ;; returns (values "hello\n" 0)
;;;
;;; For advanced use, import individual modules directly:
;;;   (import :jsh/parser :jsh/executor :jsh/environment)

(export jsh-init!
        jsh-run
        jsh-capture
        jsh-execute-input
        jsh-process-traps!)

(import :std/sugar
        :std/format
        :jsh/util
        :jsh/ast
        :jsh/environment
        :jsh/lexer
        :jsh/parser
        :jsh/executor
        :jsh/expander
        :jsh/functions
        :jsh/registry
        :jsh/builtins
        :jsh/signals
        :jsh/jobs
        :jsh/script
        :jsh/arithmetic
        :jsh/ffi)

;;; --- Initialization ---

(def (jsh-init! (interactive? #f) (manage-fds? #f) (manage-signals? #f))
  "Initialize the jsh shell engine and return a shell environment.
   Must be called before using any jsh functions.

   Options:
     interactive?    — #t for job control and terminal handling (default #f)
     manage-fds?     — #t to move Gambit internal fds high, freeing fds 3-9
                       for shell redirects. WARNING: breaks gxi REPL.
                       Only use in standalone executables. (default #f)
     manage-signals? — #t to install shell signal handlers (SIGINT, SIGCHLD, etc.).
                       May conflict with the host application's signal handling.
                       (default #f)"
  ;; Optionally move Gambit internal fds high (standalone exe only)
  (when manage-fds?
    (ffi-move-gambit-fds 255))
  ;; Cache scheduler pipe fds for ffi-fork-exec
  (*gambit-scheduler-rfd* (ffi-gambit-scheduler-rfd))
  (*gambit-scheduler-wfd* (ffi-gambit-scheduler-wfd))
  ;; Create environment
  (let ((env (make-shell-environment)))
    (env-init! env)
    (env-set! env "GSH_VERSION" "0.1.0")
    ;; Wire up circular dependency parameters
    (*execute-input* (lambda (input env) (jsh-execute-input input env)))
    (*arith-eval-fn* arith-eval)
    (*execute-external-fn* execute-external)
    (*process-traps-fn* (lambda (env) (jsh-process-traps! env)))
    ;; Register source/. builtins (normally done in main.ss)
    (register-source-builtins! env)
    ;; Optionally set up signal handlers
    (when manage-signals?
      (if interactive?
        (setup-default-signal-handlers!)
        (setup-noninteractive-signal-handlers!)))
    (when interactive?
      (*interactive-shell* #t)
      (env-shopt-set! env "expand_aliases" #t))
    env))

;;; --- Core execution ---

(def (jsh-execute-input input env)
  "Parse and execute a shell command string. Returns exit status."
  (with-catch
   (lambda (e)
     (cond
       ((nounset-exception? e) (raise e))
       ((errexit-exception? e) (errexit-exception-status e))
       ((subshell-exit-exception? e) (raise e))
       ((break-exception? e) (raise e))
       ((continue-exception? e) (raise e))
       ((return-exception? e) (raise e))
       (else
        (let ((msg (exception-message e)))
          (fprintf (current-error-port) "jsh: ~a~n" msg)
          (if (and (string? msg)
                   (or (string-prefix? "parse error" msg)
                       (string-prefix? "bad substitution: unclosed" msg)))
            2 1)))))
   (lambda ()
     (let ((cmd (with-catch
                 (lambda (e)
                   (fprintf (current-error-port) "jsh: syntax error: ~a~n"
                            (exception-message e))
                   'error)
                 (lambda ()
                   (let ((alias-fn (and (env-shopt? env "expand_aliases")
                                        (lambda (word) (alias-get env word)))))
                     (parse-complete-command input (env-shopt? env "extglob") alias-fn))))))
       (cond
         ((eq? cmd 'error) 2)
         ((not cmd) 0)
         (else (execute-command cmd env)))))))

;;; --- Trap processing ---

(def (jsh-process-traps! env)
  "Process pending signals and execute trap commands."
  (let ((signals (pending-signals!)))
    (for-each
     (lambda (sig-name)
       (cond
         ((string=? sig-name "CHLD")
          (job-update-status!)
          (job-notify!))
         (else #!void))
       (let ((action (trap-get sig-name)))
         (when (and action (string? action))
           (let ((saved-status (shell-environment-last-status env)))
             (jsh-execute-input action env)
             (env-set-last-status! env saved-status)))))
     signals)))

;;; --- Convenience API ---

(def (jsh-run input env)
  "Execute a shell command string (may span multiple lines).
   Returns exit status (integer)."
  (let ((status (execute-string input env)))
    (env-set-last-status! env status)
    status))

(def (jsh-capture input env)
  "Execute a shell command and capture stdout.
   Returns (values output-string exit-status)."
  (let ((output (command-substitute input env)))
    (values output (shell-environment-last-status env))))

;;; --- Internal: source builtin registration ---

(def (register-source-builtins! env)
  (let ((source-handler
         (lambda (args env)
           (let ((args (if (and (pair? args) (string=? (car args) "--"))
                         (cdr args)
                         args)))
             (if (null? args)
               (begin
                 (fprintf (current-error-port) "jsh: source: filename argument required~n")
                 2)
               (let* ((filename (car args))
                      (filepath (if (string-contains? filename "/")
                                  filename
                                  (or (find-file-in-path filename (env-get env "PATH"))
                                      filename))))
                 (if (pair? (cdr args))
                   (let ((saved-pos (shell-environment-positional env)))
                     (env-set-positional! env (cdr args))
                     (let ((result (source-file! filepath env)))
                       (set! (shell-environment-positional env) saved-pos)
                       result))
                   (source-file! filepath env))))))))
    (builtin-register! "source" source-handler)
    (builtin-register! "." source-handler)))
