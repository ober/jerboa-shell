#!chezscheme
(library (jsh pipeline)
  (export pipeline-temp-env execute-pipeline *pipeline-fd-mutex*
    execute-piped-commands make-pipes launch-pipeline-command
    launch-thread-piped wait-for-all last-elem void)
  (import
   (except (chezscheme) box box? unbox set-box! andmap ormap
    iota last-pair find \x31;+ \x31;- fx/ fx1+ fx1- error? raise
    with-exception-handler identifier? hash-table?
    make-hash-table sort sort! path-extension printf fprintf
    file-directory? file-exists? getenv close-port void
    open-output-file open-input-file)
   (except (jerboa runtime) bind-method! call-method ~ void
     cons* make-list)
   (runtime mop)
   (except (runtime util) last-pair iota \x31;- \x31;+
     displayln make-keyword)
   (except (compat gambit) number->string make-mutex
     with-output-to-string void string->bytes bytes->string
     thread?)
   (except (std error) with-exception-handler error-trace
     error-irritants error-message)
   (except (std misc string) string-join string-split
     string-index string-empty?)
   (except (std misc list) take drop filter-map)
   (except (std misc alist) pget pgetv pgetq aget agetv agetq)
   (except
     (std os path)
     path-expand
     path-normalize
     path-absolute?)
   (except (std format) format) (std sort) (std pregexp)
   ;; Structured logging (component=pipeline)
   (only (std log) make-logger log-info log-debug)
   (std sugar) (jsh ast) (jsh ffi) (jsh environment)
   (jsh expander) (jsh redirect) (jsh registry)
   (except (jsh builtins) list-head) (jsh functions)
   (except (jsh util) string-index string-join file-directory?
     string-join string-index string-downcase file-regular?
     string-upcase))
  ;; Debug logger: activated by JSH_DEBUG=1/all or including "pipeline"
  (define *jsh-debug-logger*
    (let ((val (getenv "JSH_DEBUG" #f)))
      (and val
           (or (string=? val "1")
               (string=? val "all")
               (string-contains? val "pipeline"))
           (make-logger 'debug 'component 'pipeline))))
  (define (pipeline-debug-log msg . args)
    (when *jsh-debug-logger*
      (apply log-debug *jsh-debug-logger* msg args)))

  (define (pipeline-temp-env assignments env)
    (let ([child (env-push-scope env)])
      (for-each
        (lambda (asgn)
          (let* ([raw-name (assignment-name asgn)])
            (let* ([name (resolve-nameref raw-name env)])
              (let* ([raw-value (assignment-value asgn)])
                (let* ([op (assignment-op asgn)])
                  (let* ([val (expand-assignment-value raw-value child)])
                    (let* ([final-val (if (eq? op '\x2B;=)
                                          (string-append
                                            (or (env-get child name) "")
                                            val)
                                          val)])
                      (hash-put!
                        (shell-environment-vars child)
                        name
                        (make-shell-var final-val #t #f #f #f #f #f #f #f
                          #f)))))))))
        assignments)
      child))
  (define execute-pipeline
    (case-lambda
      [(commands env execute-fn)
       (let* ([pipe-types #f])
         (if (= (length commands) 1)
             (let ([status (execute-fn (car commands) env)])
               (list status))
             (execute-piped-commands
               commands
               env
               execute-fn
               (or pipe-types
                   (make-list (- (length commands) 1) 'PIPE)))))]
      [(commands env execute-fn pipe-types)
       (if (= (length commands) 1)
           (let ([status (execute-fn (car commands) env)])
             (list status))
           (execute-piped-commands
             commands
             env
             execute-fn
             (or pipe-types
                 (make-list (- (length commands) 1) 'PIPE))))]))
  (define *pipeline-fd-mutex* (make-mutex 'pipeline-fd))
  (define execute-piped-commands
    (case-lambda
      [(commands env execute-fn)
       (let* ([pipe-types #f])
         (let ([ptypes (or pipe-types
                           (make-list (- (length commands) 1) 'PIPE))])
           (parameterize ([*procsub-cleanups* (list)])
             (let* ([outer-in (*pipeline-stdin-fd*)])
               (let* ([outer-out (*pipeline-stdout-fd*)])
                 (let* ([true-stdin (and outer-in (ffi-dup 0))])
                   (let* ([true-stdout (and outer-out (ffi-dup 1))])
                     (mutex-lock! *pipeline-fd-mutex*)
                     (when outer-in (ffi-dup2 outer-in 0))
                     (when outer-out (ffi-dup2 outer-out 1))
                     (let* ([n (length commands)])
                       (pipeline-debug-log "pipeline: ~a stages" n)
                       (let* ([pipes (make-pipes (- n 1))])
                         (let* ([saved-stdin-fd (ffi-dup 0)])
                           (let* ([saved-stdout-fd (ffi-dup 1)])
                             (let* ([saved-stderr-fd (ffi-dup 2)])
                               (let* ([saved-stdin-port (current-input-port)])
                                 (let* ([saved-stdout-port (current-output-port)])
                                   (let* ([saved-stderr-port (current-error-port)])
                                     (let ([procs (let loop ([cmds commands]
                                                             [idx 0]
                                                             [procs (list)])
                                                    (if (null? cmds)
                                                        (reverse procs)
                                                        (let* ([is-first? (= idx
                                                                             0)])
                                                          (let* ([is-last? (null?
                                                                             (cdr cmds))])
                                                            (let* ([in-pipe (and (not is-first?)
                                                                                 (list-ref
                                                                                   pipes
                                                                                   (- idx
                                                                                      1)))])
                                                              (let* ([out-pipe (and (not is-last?)
                                                                                    (list-ref
                                                                                      pipes
                                                                                      idx))])
                                                                (let* ([pipeamp? (and (not is-last?)
                                                                                      (< idx
                                                                                         (length
                                                                                           ptypes))
                                                                                      (eq? (list-ref
                                                                                             ptypes
                                                                                             idx)
                                                                                           'PIPEAMP))])
                                                                  (when in-pipe
                                                                    (ffi-dup2
                                                                      (car in-pipe)
                                                                      0))
                                                                  (when out-pipe
                                                                    (ffi-dup2
                                                                      (cadr
                                                                        out-pipe)
                                                                      1))
                                                                  (when pipeamp?
                                                                    (ffi-dup2
                                                                      (cadr
                                                                        out-pipe)
                                                                      2))
                                                                  (let ([proc (if (and is-last?
                                                                                       (not is-first?)
                                                                                       (env-shopt?
                                                                                         env
                                                                                         "lastpipe")
                                                                                       (not (*in-subshell*)))
                                                                                  (let* ([pipe-in-fd (ffi-dup
                                                                                                       0)])
                                                                                    (let* ([in-port (open-input-file
                                                                                                      (string-append
                                                                                                        "/dev/fd/"
                                                                                                        (number->string
                                                                                                          pipe-in-fd)))])
                                                                                      (let* ([old-input (current-input-port)])
                                                                                        (let* ([status (begin
                                                                                                         (current-input-port
                                                                                                           in-port)
                                                                                                         (guard (__exn
                                                                                                                  [#t
                                                                                                                   ((lambda (e)
                                                                                                                      (cond
                                                                                                                        [(subshell-exit-exception?
                                                                                                                           e)
                                                                                                                         (subshell-exit-exception-status
                                                                                                                           e)]
                                                                                                                        [(errexit-exception?
                                                                                                                           e)
                                                                                                                         (errexit-exception-status
                                                                                                                           e)]
                                                                                                                        [else
                                                                                                                         (raise
                                                                                                                           e)]))
                                                                                                                     __exn)])
                                                                                                           (execute-fn
                                                                                                             (car cmds)
                                                                                                             env)))])
                                                                                          (current-input-port
                                                                                            old-input)
                                                                                          (close-port
                                                                                            in-port)
                                                                                          (ffi-close-fd
                                                                                            pipe-in-fd)
                                                                                          (list
                                                                                            'direct
                                                                                            status)))))
                                                                                  (launch-pipeline-command
                                                                                    (car cmds)
                                                                                    env
                                                                                    execute-fn
                                                                                    (not is-first?)
                                                                                    (not is-last?)))])
                                                                    (when in-pipe
                                                                      (ffi-dup2
                                                                        saved-stdin-fd
                                                                        0))
                                                                    (when out-pipe
                                                                      (ffi-dup2
                                                                        saved-stdout-fd
                                                                        1))
                                                                    (when pipeamp?
                                                                      (ffi-dup2
                                                                        saved-stderr-fd
                                                                        2))
                                                                    (when in-pipe
                                                                      (ffi-close-fd
                                                                        (car in-pipe))
                                                                      (set-car!
                                                                        in-pipe
                                                                        -1))
                                                                    (when out-pipe
                                                                      (ffi-close-fd
                                                                        (cadr
                                                                          out-pipe))
                                                                      (set-car!
                                                                        (cdr out-pipe)
                                                                        -1))
                                                                    (loop
                                                                      (cdr cmds)
                                                                      (+ idx
                                                                         1)
                                                                      (cons
                                                                        proc
                                                                        procs))))))))))])
                                       (for-each
                                         (lambda (p)
                                           (when (>= (car p) 0)
                                             (guard (__exn
                                                      [#t (void __exn)])
                                               (ffi-close-fd (car p))))
                                           (when (>= (cadr p) 0)
                                             (guard (__exn
                                                      [#t (void __exn)])
                                               (ffi-close-fd (cadr p)))))
                                         pipes)
                                       (ffi-dup2 saved-stdin-fd 0)
                                       (ffi-dup2 saved-stdout-fd 1)
                                       (ffi-dup2 saved-stderr-fd 2)
                                       (ffi-close-fd saved-stdin-fd)
                                       (ffi-close-fd saved-stdout-fd)
                                       (ffi-close-fd saved-stderr-fd)
                                       (when true-stdin
                                         (ffi-dup2 true-stdin 0)
                                         (ffi-close-fd true-stdin))
                                       (when true-stdout
                                         (ffi-dup2 true-stdout 1)
                                         (ffi-close-fd true-stdout))
                                       (when outer-in
                                         (ffi-close-fd outer-in))
                                       (when outer-out
                                         (ffi-close-fd outer-out))
                                       (current-input-port
                                         saved-stdin-port)
                                       (current-output-port
                                         saved-stdout-port)
                                       (current-error-port
                                         saved-stderr-port)
                                       (mutex-unlock! *pipeline-fd-mutex*)
                                       (let ([exit-codes (wait-for-all
                                                           procs)])
                                         (ffi-sigchld-unblock)
                                         (run-procsub-cleanups!)
                                         exit-codes)))))))))))))))))]
      [(commands env execute-fn pipe-types)
       (let ([ptypes (or pipe-types
                         (make-list (- (length commands) 1) 'PIPE))])
         (parameterize ([*procsub-cleanups* (list)])
           (let* ([outer-in (*pipeline-stdin-fd*)])
             (let* ([outer-out (*pipeline-stdout-fd*)])
               (let* ([true-stdin (and outer-in (ffi-dup 0))])
                 (let* ([true-stdout (and outer-out (ffi-dup 1))])
                   (mutex-lock! *pipeline-fd-mutex*)
                   (when outer-in (ffi-dup2 outer-in 0))
                   (when outer-out (ffi-dup2 outer-out 1))
                   (let* ([n (length commands)])
                     (let* ([pipes (make-pipes (- n 1))])
                       (let* ([saved-stdin-fd (ffi-dup 0)])
                         (let* ([saved-stdout-fd (ffi-dup 1)])
                           (let* ([saved-stderr-fd (ffi-dup 2)])
                             (let* ([saved-stdin-port (current-input-port)])
                               (let* ([saved-stdout-port (current-output-port)])
                                 (let* ([saved-stderr-port (current-error-port)])
                                   (let ([procs (let loop ([cmds commands]
                                                           [idx 0]
                                                           [procs (list)])
                                                  (if (null? cmds)
                                                      (reverse procs)
                                                      (let* ([is-first? (= idx
                                                                           0)])
                                                        (let* ([is-last? (null?
                                                                           (cdr cmds))])
                                                          (let* ([in-pipe (and (not is-first?)
                                                                               (list-ref
                                                                                 pipes
                                                                                 (- idx
                                                                                    1)))])
                                                            (let* ([out-pipe (and (not is-last?)
                                                                                  (list-ref
                                                                                    pipes
                                                                                    idx))])
                                                              (let* ([pipeamp? (and (not is-last?)
                                                                                    (< idx
                                                                                       (length
                                                                                         ptypes))
                                                                                    (eq? (list-ref
                                                                                           ptypes
                                                                                           idx)
                                                                                         'PIPEAMP))])
                                                                (when in-pipe
                                                                  (ffi-dup2
                                                                    (car in-pipe)
                                                                    0))
                                                                (when out-pipe
                                                                  (ffi-dup2
                                                                    (cadr
                                                                      out-pipe)
                                                                    1))
                                                                (when pipeamp?
                                                                  (ffi-dup2
                                                                    (cadr
                                                                      out-pipe)
                                                                    2))
                                                                (let ([proc (if (and is-last?
                                                                                     (not is-first?)
                                                                                     (env-shopt?
                                                                                       env
                                                                                       "lastpipe")
                                                                                     (not (*in-subshell*)))
                                                                                (let* ([pipe-in-fd (ffi-dup
                                                                                                     0)])
                                                                                  (let* ([in-port (open-input-file
                                                                                                    (string-append
                                                                                                      "/dev/fd/"
                                                                                                      (number->string
                                                                                                        pipe-in-fd)))])
                                                                                    (let* ([old-input (current-input-port)])
                                                                                      (let* ([status (begin
                                                                                                       (current-input-port
                                                                                                         in-port)
                                                                                                       (guard (__exn
                                                                                                                [#t
                                                                                                                 ((lambda (e)
                                                                                                                    (cond
                                                                                                                      [(subshell-exit-exception?
                                                                                                                         e)
                                                                                                                       (subshell-exit-exception-status
                                                                                                                         e)]
                                                                                                                      [(errexit-exception?
                                                                                                                         e)
                                                                                                                       (errexit-exception-status
                                                                                                                         e)]
                                                                                                                      [else
                                                                                                                       (raise
                                                                                                                         e)]))
                                                                                                                   __exn)])
                                                                                                         (execute-fn
                                                                                                           (car cmds)
                                                                                                           env)))])
                                                                                        (current-input-port
                                                                                          old-input)
                                                                                        (close-port
                                                                                          in-port)
                                                                                        (ffi-close-fd
                                                                                          pipe-in-fd)
                                                                                        (list
                                                                                          'direct
                                                                                          status)))))
                                                                                (launch-pipeline-command
                                                                                  (car cmds)
                                                                                  env
                                                                                  execute-fn
                                                                                  (not is-first?)
                                                                                  (not is-last?)))])
                                                                  (when in-pipe
                                                                    (ffi-dup2
                                                                      saved-stdin-fd
                                                                      0))
                                                                  (when out-pipe
                                                                    (ffi-dup2
                                                                      saved-stdout-fd
                                                                      1))
                                                                  (when pipeamp?
                                                                    (ffi-dup2
                                                                      saved-stderr-fd
                                                                      2))
                                                                  (when in-pipe
                                                                    (ffi-close-fd
                                                                      (car in-pipe))
                                                                    (set-car!
                                                                      in-pipe
                                                                      -1))
                                                                  (when out-pipe
                                                                    (ffi-close-fd
                                                                      (cadr
                                                                        out-pipe))
                                                                    (set-car!
                                                                      (cdr out-pipe)
                                                                      -1))
                                                                  (loop
                                                                    (cdr cmds)
                                                                    (+ idx
                                                                       1)
                                                                    (cons
                                                                      proc
                                                                      procs))))))))))])
                                     (for-each
                                       (lambda (p)
                                         (when (>= (car p) 0)
                                           (guard (__exn [#t (void __exn)])
                                             (ffi-close-fd (car p))))
                                         (when (>= (cadr p) 0)
                                           (guard (__exn [#t (void __exn)])
                                             (ffi-close-fd (cadr p)))))
                                       pipes)
                                     (ffi-dup2 saved-stdin-fd 0)
                                     (ffi-dup2 saved-stdout-fd 1)
                                     (ffi-dup2 saved-stderr-fd 2)
                                     (ffi-close-fd saved-stdin-fd)
                                     (ffi-close-fd saved-stdout-fd)
                                     (ffi-close-fd saved-stderr-fd)
                                     (when true-stdin
                                       (ffi-dup2 true-stdin 0)
                                       (ffi-close-fd true-stdin))
                                     (when true-stdout
                                       (ffi-dup2 true-stdout 1)
                                       (ffi-close-fd true-stdout))
                                     (when outer-in
                                       (ffi-close-fd outer-in))
                                     (when outer-out
                                       (ffi-close-fd outer-out))
                                     (current-input-port saved-stdin-port)
                                     (current-output-port
                                       saved-stdout-port)
                                     (current-error-port saved-stderr-port)
                                     (mutex-unlock! *pipeline-fd-mutex*)
                                     (let ([exit-codes (wait-for-all
                                                         procs)])
                                       (ffi-sigchld-unblock)
                                       (run-procsub-cleanups!)
                                       exit-codes))))))))))))))))]))
  (define (make-pipes n)
    (let loop ([i 0] [pipes (list)])
      (if (>= i n)
          (reverse pipes)
          (let-values ([(read-fd write-fd) (ffi-pipe-raw)])
            (loop (+ i 1) (cons (list read-fd write-fd) pipes))))))
  (define (launch-pipeline-command cmd env execute-fn
           has-pipe-in? has-pipe-out?)
    (cond
      [(simple-command? cmd)
       (let* ([child-env (env-clone env)])
         (let* ([words (expand-words
                         (simple-command-words cmd)
                         child-env)])
           (let* ([cmd-name (if (pair? words) (car words) #f)])
             (let* ([redirections (simple-command-redirections cmd)])
               (cond
                 [(and cmd-name (builtin-lookup cmd-name))
                  (launch-thread-piped cmd child-env execute-fn
                    has-pipe-in? has-pipe-out?)]
                 [(and cmd-name (which cmd-name))
                  (let* ([path (which cmd-name)])
                    (let* ([exec-path (if (string-contains? path "/")
                                          (gambit-path-expand path)
                                          path)])
                      (let* ([args (if (pair? words) (cdr words) (list))])
                        (let* ([assignments (simple-command-assignments
                                              cmd)])
                          (let* ([cmd-env (if (pair? assignments)
                                              (pipeline-temp-env
                                                assignments
                                                child-env)
                                              child-env)])
                            (let* ([redir-saved (if (pair? redirections)
                                                    (guard (__exn
                                                             [#t
                                                              ((lambda (e)
                                                                 #f)
                                                                __exn)])
                                                      (apply-redirections
                                                        redirections
                                                        cmd-env))
                                                    (list))])
                              (let* ([packed-argv (pack-with-soh
                                                    (map string->c-safe
                                                         (cons
                                                           cmd-name
                                                           args)))])
                                (let* ([packed-env (pack-with-soh
                                                     (map string->c-safe
                                                          (env-exported-alist
                                                            cmd-env)))])
                                  (let* ([keep-fds (pack-fds-with-soh
                                                     (*active-redirect-fds*))])
                                    (let* ([pid (ffi-fork-exec
                                                  (string->c-safe
                                                    exec-path)
                                                  packed-argv packed-env 0
                                                  (*gambit-scheduler-rfd*)
                                                  (*gambit-scheduler-wfd*)
                                                  keep-fds
                                                  (current-directory))])
                                      (when (pair? redir-saved)
                                        (restore-redirections redir-saved))
                                      (if (< pid 0)
                                          (launch-thread-piped cmd child-env execute-fn
                                            has-pipe-in? has-pipe-out?)
                                          (list 'process pid))))))))))))]
                 [else
                  (launch-thread-piped cmd child-env execute-fn
                    has-pipe-in? has-pipe-out?)])))))]
      [else
       (launch-thread-piped cmd env execute-fn has-pipe-in?
         has-pipe-out?)]))
  (define (launch-thread-piped cmd env execute-fn has-pipe-in?
           has-pipe-out?)
    (let* ([exit-box (box 0)])
      (let* ([thread-in-fd (if has-pipe-in? (ffi-dup 0) #f)])
        (let* ([thread-out-fd (if has-pipe-out? (ffi-dup 1) #f)])
          (let* ([saved-in-port (current-input-port)])
            (let* ([saved-out-port (current-output-port)])
              (let ([t (spawn
                         (lambda ()
                           (let ([in-port (if thread-in-fd
                                              (open-input-file
                                                (string-append
                                                  "/dev/fd/"
                                                  (number->string
                                                    thread-in-fd)))
                                              saved-in-port)]
                                 [out-port (if thread-out-fd
                                               (open-output-file
                                                 (string-append
                                                   "/dev/fd/"
                                                   (number->string
                                                     thread-out-fd)))
                                               saved-out-port)])
                             (parameterize ([current-input-port in-port]
                                            [current-output-port out-port]
                                            [*in-subshell* #t]
                                            [*pipeline-stdin-fd*
                                             thread-in-fd]
                                            [*pipeline-stdout-fd*
                                             thread-out-fd])
                               (let ([status (guard (__exn
                                                      [#t
                                                       ((lambda (e)
                                                          (cond
                                                            [(subshell-exit-exception?
                                                               e)
                                                             (subshell-exit-exception-status
                                                               e)]
                                                            [(errexit-exception?
                                                               e)
                                                             (errexit-exception-status
                                                               e)]
                                                            [else
                                                             (raise e)]))
                                                         __exn)])
                                               (execute-fn cmd env))])
                                 (set-box! exit-box status)
                                 (flush-output-port out-port)
                                 (when thread-out-fd
                                   (close-port out-port)
                                   (ffi-close-fd thread-out-fd))
                                 (when thread-in-fd
                                   (close-port in-port)
                                   (ffi-close-fd thread-in-fd)))))))])
                (list 'thread t exit-box))))))))
  (define (wait-for-all procs)
    (map (lambda (proc)
           (cond
             [(port? proc)
              (let ([pid (process-pid proc)])
                (let loop ([delay 0.001])
                  (let ([result (ffi-waitpid-pid pid WNOHANG)])
                    (cond
                      [(> result 0)
                       (let ([raw (ffi-waitpid-status)])
                         (close-port proc)
                         (status->exit-code raw))]
                      [(= result 0)
                       (thread-sleep! delay)
                       (loop (min 0.05 (* delay 1.5)))]
                      [else
                       (let ([raw-status (process-status proc)])
                         (close-port proc)
                         (status->exit-code raw-status))]))))]
             [(and (list? proc) (eq? (car proc) 'process))
              (let ([pid (cadr proc)])
                (let loop ([delay 0.001])
                  (let ([result (ffi-waitpid-pid pid WNOHANG)])
                    (cond
                      [(> result 0)
                       (status->exit-code (ffi-waitpid-status))]
                      [(= result 0)
                       (thread-sleep! delay)
                       (loop (min 0.05 (* delay 1.5)))]
                      [else 0]))))]
             [(and (list? proc) (eq? (car proc) 'direct)) (cadr proc)]
             [(and (list? proc) (eq? (car proc) 'thread))
              (guard (__exn
                       [#t
                        ((lambda (e)
                           (cond
                             [(subshell-exit-exception? e)
                              (subshell-exit-exception-status e)]
                             [(errexit-exception? e)
                              (errexit-exception-status e)]
                             [else (unbox (caddr proc))]))
                          __exn)])
                (thread-join! (cadr proc))
                (unbox (caddr proc)))]
             [else 0]))
         procs))
  (define (last-elem lst)
    (if (null? (cdr lst)) (car lst) (last-elem (cdr lst))))
  (define (void . _) (if #f #f)))
