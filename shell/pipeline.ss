;;; pipeline.ss — Pipe management for gsh
;;; Connects N commands in a pipeline using OS pipes.
;;;
;;; Strategy: for each command, dup2 the appropriate pipe fds onto real fds 0/1,
;;; launch the command with stdin/stdout-redirection: #f (inherit real fds),
;;; then immediately close the pipe fd in the parent so downstream commands
;;; see EOF when the producer finishes.

(export #t)
(import :std/sugar
        :std/format
        :gsh/ast
        :gsh/ffi
        :gsh/environment
        :gsh/expander
        :gsh/redirect
        :gsh/registry
        :gsh/builtins
        :gsh/functions
        :gsh/util)

;;; --- Helpers ---

;; Build a temp env with prefix assignments exported for pipeline external commands.
;; Simpler version of executor.ss apply-temp-assignments for use in pipeline context.
(def (pipeline-temp-env assignments env)
  (let ((child (env-push-scope env)))
    (for-each
     (lambda (asgn)
       (let* ((raw-name (assignment-name asgn))
              (name (resolve-nameref raw-name env))
              (raw-value (assignment-value asgn))
              (op (assignment-op asgn))
              (val (expand-assignment-value raw-value child))
              (final-val (if (eq? op '+=)
                           (string-append (or (env-get child name) "") val)
                           val)))
         (hash-put! (shell-environment-vars child) name
                    (make-shell-var final-val #t #f #f #f #f #f #f #f #f))))
     assignments)
    child))

;;; --- Public interface ---

(def (execute-pipeline commands env execute-fn (pipe-types #f))
  (if (= (length commands) 1)
    (let ((status (execute-fn (car commands) env)))
      ;; Return list of exit codes for PIPESTATUS
      [status])
    (execute-piped-commands commands env execute-fn
                            (or pipe-types (make-list (- (length commands) 1) 'PIPE)))))

;;; --- Multi-command pipeline ---

;; Mutex to serialize fd-manipulation sections of pipeline setup.
;; Nested pipelines (e.g. { cmd | cmd; } | cmd) run execute-piped-commands
;; in threads that share real fds 0/1, so we must serialize the dup2/launch/restore.
(def *pipeline-fd-mutex* (make-mutex "pipeline-fd"))

(def (execute-piped-commands commands env execute-fn (pipe-types #f))
  (let ((ptypes (or pipe-types (make-list (- (length commands) 1) 'PIPE))))
  (parameterize ((*procsub-cleanups* []))
  ;; When running inside a pipeline thread (nested pipeline in brace group or eval),
  ;; the outer pipe fds are only in character ports, not on real fds 0/1.
  ;; We need inner pipeline commands to inherit the outer pipe fds.
  ;; Save the true originals first, then dup2 outer pipe onto 0/1 for the inner pipeline,
  ;; and restore originals when done (so the outer pipe write-end reference is removed,
  ;; allowing downstream readers to see EOF).
  (let* ((outer-in (*pipeline-stdin-fd*))
         (outer-out (*pipeline-stdout-fd*))
         (true-stdin (and outer-in (ffi-dup 0)))
         (true-stdout (and outer-out (ffi-dup 1))))
    ;; Lock mutex to serialize fd manipulation across pipeline threads.
    ;; Nested pipelines (e.g. { cmd | cmd; } | cmd) run in threads that
    ;; share real fds 0/1, so we must serialize dup2/launch/restore.
    (mutex-lock! *pipeline-fd-mutex*)
    (when outer-in (ffi-dup2 outer-in 0))
    (when outer-out (ffi-dup2 outer-out 1))
  (let* ((n (length commands))
         ;; Create n-1 pipes: each is [read-fd write-fd]
         (pipes (make-pipes (- n 1)))
         ;; Save fds (now pointing to outer pipe if nested)
         (saved-stdin-fd (ffi-dup 0))
         (saved-stdout-fd (ffi-dup 1))
         (saved-stderr-fd (ffi-dup 2))
         (saved-stdin-port (current-input-port))
         (saved-stdout-port (current-output-port))
         (saved-stderr-port (current-error-port)))
    (let ((procs
           (let loop ((cmds commands) (idx 0) (procs []))
             (if (null? cmds)
               (reverse procs)
               (let* ((is-first? (= idx 0))
                      (is-last? (null? (cdr cmds)))
                      ;; This command reads from the pipe connecting it to the previous
                      (in-pipe (and (not is-first?) (list-ref pipes (- idx 1))))
                      ;; This command writes to the pipe connecting it to the next
                      (out-pipe (and (not is-last?) (list-ref pipes idx)))
                      ;; Check if this connection uses |& (stderr also goes to pipe)
                      (pipeamp? (and (not is-last?)
                                     (< idx (length ptypes))
                                     (eq? (list-ref ptypes idx) 'PIPEAMP))))

                 ;; Redirect real fd 0 to pipe read-end (if not first command)
                 (when in-pipe
                   (ffi-dup2 (car in-pipe) 0))

                 ;; Redirect real fd 1 to pipe write-end (if not last command)
                 (when out-pipe
                   (ffi-dup2 (cadr out-pipe) 1))

                 ;; For |&, also redirect fd 2 to the pipe write-end
                 (when pipeamp?
                   (ffi-dup2 (cadr out-pipe) 2))

                 ;; Launch the command (it inherits current real fds 0/1)
                 ;; For lastpipe: run last command in current shell (not subshell)
                 (let ((proc (if (and is-last? (not is-first?)
                                     (env-shopt? env "lastpipe")
                                     (not (*in-subshell*)))
                               ;; lastpipe: execute last command directly in current shell
                               (let* ((pipe-in-fd (ffi-dup 0))
                                      (in-port (open-input-file
                                                (string-append "/dev/fd/"
                                                               (number->string pipe-in-fd))))
                                      (old-input (current-input-port))
                                      (status (begin
                                                (current-input-port in-port)
                                                (with-catch
                                                 (lambda (e)
                                                   (cond
                                                     ((subshell-exit-exception? e)
                                                      (subshell-exit-exception-status e))
                                                     ((errexit-exception? e)
                                                      (errexit-exception-status e))
                                                     (else (raise e))))
                                                 (lambda ()
                                                   (execute-fn (car cmds) env))))))
                                 (current-input-port old-input)
                                 (close-port in-port)
                                 (ffi-close-fd pipe-in-fd)
                                 (list 'direct status))
                               ;; Normal: launch in thread/process
                               (launch-pipeline-command
                                (car cmds) env execute-fn
                                (not is-first?) (not is-last?)))))

                   ;; Restore real fd 0, 1, and 2 for the parent
                   (when in-pipe
                     (ffi-dup2 saved-stdin-fd 0))
                   (when out-pipe
                     (ffi-dup2 saved-stdout-fd 1))
                   (when pipeamp?
                     (ffi-dup2 saved-stderr-fd 2))

                   ;; CRITICAL: Close pipe ends in the parent after launching
                   ;; the command that uses them. This ensures:
                   ;; - The write-end is only held by the producing process
                   ;; - The read-end is only held by the consuming process
                   ;; Without this, readers never see EOF.
                   (when in-pipe
                     (ffi-close-fd (car in-pipe))   ;; close read-end
                     (set-car! in-pipe -1))          ;; mark as closed
                   (when out-pipe
                     (ffi-close-fd (cadr out-pipe))  ;; close write-end
                     (set-car! (cdr out-pipe) -1))   ;; mark as closed

                   (loop (cdr cmds) (+ idx 1) (cons proc procs))))))))
      ;; Close any remaining pipe fds (shouldn't be any, but be safe)
      (for-each
       (lambda (p)
         (when (>= (car p) 0)
           (with-catch void (lambda () (ffi-close-fd (car p)))))
         (when (>= (cadr p) 0)
           (with-catch void (lambda () (ffi-close-fd (cadr p))))))
       pipes)
      ;; Restore fds and ports
      (ffi-dup2 saved-stdin-fd 0)
      (ffi-dup2 saved-stdout-fd 1)
      (ffi-dup2 saved-stderr-fd 2)
      (ffi-close-fd saved-stdin-fd)
      (ffi-close-fd saved-stdout-fd)
      (ffi-close-fd saved-stderr-fd)
      ;; Restore true original fds BEFORE wait-for-all.
      ;; Without this, fd 1 holds an extra reference to the outer pipe
      ;; write-end during the wait, preventing downstream readers from
      ;; seeing EOF (causes hangs in nested pipelines).
      (when true-stdin (ffi-dup2 true-stdin 0) (ffi-close-fd true-stdin))
      (when true-stdout (ffi-dup2 true-stdout 1) (ffi-close-fd true-stdout))
      ;; CRITICAL: Also close the outer-in/outer-out fds passed from command substitution
      ;; to prevent dangling references to the capture pipe write-end
      (when outer-in (ffi-close-fd outer-in))
      (when outer-out (ffi-close-fd outer-out))
      (current-input-port saved-stdin-port)
      (current-output-port saved-stdout-port)
      (current-error-port saved-stderr-port)
      ;; Unlock mutex — all commands launched, fds fully restored.
      (mutex-unlock! *pipeline-fd-mutex*)
      ;; Wait for all processes/threads
      (let ((exit-codes (wait-for-all procs)))
        ;; Unblock SIGCHLD now that all children are reaped
        (ffi-sigchld-unblock)
        ;; Clean up any process substitution FIFOs
        (run-procsub-cleanups!)
        ;; Return the full list of exit codes (caller sets PIPESTATUS)
        exit-codes)))))))

(def (make-pipes n)
  (let loop ((i 0) (pipes []))
    (if (>= i n)
      (reverse pipes)
      (let-values (((read-fd write-fd) (ffi-pipe-raw)))
        (loop (+ i 1) (cons [read-fd write-fd] pipes))))))

;;; --- Launch helpers ---

;; Launch a single command in the pipeline.
;; Real fds 0/1 are already set to pipe ends (or original fds).
(def (launch-pipeline-command cmd env execute-fn has-pipe-in? has-pipe-out?)
  (cond
    ((simple-command? cmd)
     ;; Clone env so word expansion side effects (${var=value}) don't leak to parent
     (let* ((child-env (env-clone env))
            (words (expand-words (simple-command-words cmd) child-env))
            (cmd-name (if (pair? words) (car words) #f))
            (redirections (simple-command-redirections cmd)))
       (cond
         ;; Check builtins FIRST — before external commands
         ((and cmd-name (builtin-lookup cmd-name))
          (launch-thread-piped cmd child-env execute-fn has-pipe-in? has-pipe-out?))
         ;; External command — inherits real fds 0/1 directly
         ((and cmd-name (which cmd-name))
          (let* ((path (which cmd-name))
                 ;; Resolve relative paths to absolute
                 (exec-path (if (string-contains? path "/")
                              (path-expand path) path))
                 (args (if (pair? words) (cdr words) []))
                 ;; Apply temp assignments so they appear in the environment
                 (assignments (simple-command-assignments cmd))
                 (cmd-env (if (pair? assignments)
                            (pipeline-temp-env assignments child-env)
                            child-env))
                 (redir-saved (if (pair? redirections)
                                (with-catch
                                 (lambda (e) #f)
                                 (lambda () (apply-redirections redirections cmd-env)))
                                []))
                 (packed-argv (pack-with-soh
                               (map string->c-safe (cons cmd-name args))))
                 (packed-env (pack-with-soh
                              (map string->c-safe (env-exported-alist cmd-env))))
                 (keep-fds (pack-fds-with-soh (*active-redirect-fds*)))
                 (pid (ffi-fork-exec (string->c-safe exec-path)
                                     packed-argv packed-env
                                     0  ;; foreground pipeline component
                                     (*gambit-scheduler-rfd*)
                                     (*gambit-scheduler-wfd*)
                                     keep-fds
                                     (current-directory))))
            ;; Restore redirections in parent (child already inherited the fds)
            (when (pair? redir-saved)
              (restore-redirections redir-saved))
            (if (< pid 0)
              ;; Fork failed — fall back to thread
              (launch-thread-piped cmd child-env execute-fn has-pipe-in? has-pipe-out?)
              (list 'process pid))))
         ;; Shell function or unknown — run in thread
         (else
          (launch-thread-piped cmd child-env execute-fn has-pipe-in? has-pipe-out?)))))
    (else
     (launch-thread-piped cmd env execute-fn has-pipe-in? has-pipe-out?))))

;; Launch a builtin/function in a thread.
;; Create Gambit character ports wrapping current real fds 0/1.
;; Each pipeline component runs as a subshell (exit only exits the component).
(def (launch-thread-piped cmd env execute-fn has-pipe-in? has-pipe-out?)
  (let* ((exit-box (box 0))
         ;; Dup pipe fds so the thread has its own copy
         ;; (parent will restore 0/1 after this returns)
         (thread-in-fd (if has-pipe-in? (ffi-dup 0) #f))
         (thread-out-fd (if has-pipe-out? (ffi-dup 1) #f))
         ;; Capture current Gambit ports for non-pipe ends (thread-safe)
         (saved-in-port (current-input-port))
         (saved-out-port (current-output-port)))
    (let ((t (spawn
              (lambda ()
                (let ((in-port (if thread-in-fd
                                 (open-input-file
                                  (string-append "/dev/fd/" (number->string thread-in-fd)))
                                 saved-in-port))
                      (out-port (if thread-out-fd
                                 (open-output-file
                                  (string-append "/dev/fd/" (number->string thread-out-fd)))
                                 saved-out-port)))
                  (parameterize ((current-input-port in-port)
                                 (current-output-port out-port)
                                 (*in-subshell* #t)
                                 (*pipeline-stdin-fd* thread-in-fd)
                                 (*pipeline-stdout-fd* thread-out-fd))
                    (let ((status (with-catch
                                   (lambda (e)
                                     (cond
                                       ((subshell-exit-exception? e)
                                        (subshell-exit-exception-status e))
                                       ((errexit-exception? e)
                                        (errexit-exception-status e))
                                       (else (raise e))))
                                   (lambda ()
                                     (execute-fn cmd env)))))
                      (set-box! exit-box status)
                      (force-output out-port)
                      (when thread-out-fd
                        (close-port out-port)
                        (ffi-close-fd thread-out-fd))
                      (when thread-in-fd
                        (close-port in-port)
                        (ffi-close-fd thread-in-fd)))))))))
      (list 'thread t exit-box))))

;; Wait for all processes/threads to complete
(def (wait-for-all procs)
  (map
   (lambda (proc)
     (cond
       ((port? proc)
        ;; Use ffi-waitpid polling instead of Gambit's process-status.
        ;; process-status hangs when the child exits before the event condvar
        ;; is polled (lost SIGCHLD wakeup in Gambit's I/O system).
        (let ((pid (process-pid proc)))
          (let loop ((delay 0.001))
            (let ((result (ffi-waitpid-pid pid WNOHANG)))
              (cond
                ((> result 0)
                 ;; Process exited — get status
                 (let ((raw (ffi-waitpid-status)))
                   (close-port proc)
                   (status->exit-code raw)))
                ((= result 0)
                 ;; Still running — sleep and poll again
                 (thread-sleep! delay)
                 (loop (min 0.05 (* delay 1.5))))
                (else
                 ;; ECHILD — Gambit already reaped it, fall back to process-status
                 (let ((raw-status (process-status proc)))
                   (close-port proc)
                   (status->exit-code raw-status))))))))
       ((and (list? proc) (eq? (car proc) 'process))
        ;; ffi-fork-exec'd child: poll with waitpid (no Gambit port)
        (let ((pid (cadr proc)))
          (let loop ((delay 0.001))
            (let ((result (ffi-waitpid-pid pid WNOHANG)))
              (cond
                ((> result 0)
                 (status->exit-code (ffi-waitpid-status)))
                ((= result 0)
                 (thread-sleep! delay)
                 (loop (min 0.05 (* delay 1.5))))
                (else
                 ;; ECHILD — shouldn't happen for ffi-fork-exec children
                 0))))))
       ((and (list? proc) (eq? (car proc) 'direct))
        ;; lastpipe: already executed directly, return stored status
        (cadr proc))
       ((and (list? proc) (eq? (car proc) 'thread))
        ;; Catch exceptions from pipeline threads (e.g. subshell-exit-exception
        ;; from 'exit N' in a brace group) and use the exit code from the box
        (with-catch
         (lambda (e)
           (cond
             ((subshell-exit-exception? e) (subshell-exit-exception-status e))
             ((errexit-exception? e) (errexit-exception-status e))
             (else (unbox (caddr proc)))))
         (lambda ()
           (thread-join! (cadr proc))
           (unbox (caddr proc)))))
       (else 0)))
   procs))

;;; --- Helpers ---

(def (last-elem lst)
  (if (null? (cdr lst)) (car lst) (last-elem (cdr lst))))

(def (void) #!void)
