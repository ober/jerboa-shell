#!chezscheme
(library (jsh executor)
  (export execute-command apply-assignment!
   apply-temp-assignments execute-simple-command
   has-equals-sign? declaration-builtin?
   literal-declaration-builtin? expand-declaration-args
   word-is-literal-empty? execute-external execute-exec
   exec-command execute-ast-pipeline execute-and-or
   execute-command-list execute-subshell
   execute-redirected-command execute-brace-group
   execute-function-def execute-cond-command eval-cond-expr
   eval-cond-unary cond-file-access? eval-cond-binary-test
   arith-string-to-int cond-int-cmp cond-same-file?
   cond-newer-than? execute-arith-command execute-arith-for
   check-errexit! execute-time-command format-time-bash
   format-time-posix return-status string-join-words
   execute-coproc *next-fake-pid* next-fake-pid!
   launch-background launch-background-simple
   ast->command-text)
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
     with-output-to-string string->bytes bytes->string thread?)
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
   (std sugar)
   (except (jsh pregexp-compat) pregexp-quote pregexp-replace*
     pregexp-replace pregexp-split pregexp-match
     pregexp-match-positions pregexp)
   (gsh ast) (gsh environment) (gsh expander) (gsh registry)
   (gsh builtins) (gsh functions) (gsh pipeline) (gsh redirect)
   (gsh control) (gsh jobs) (gsh util) (gsh arithmetic)
   (gsh ffi) (gsh glob) (gsh signals))
  (define (execute-command cmd env)
    (cond
      [(not cmd) 0]
      [(redirected-command? cmd)
       (let ([s (execute-redirected-command cmd env)])
         (env-set-last-status! env s)
         (check-errexit! env s)
         s)]
      [(simple-command? cmd) (execute-simple-command cmd env)]
      [(ast-pipeline? cmd)
       (let ([s (execute-ast-pipeline cmd env)])
         (unless (ast-pipeline-bang? cmd) (check-errexit! env s))
         s)]
      [(and-or-list? cmd) (execute-and-or cmd env)]
      [(command-list? cmd) (execute-command-list cmd env)]
      [(subshell? cmd)
       (let ([s (execute-subshell cmd env)])
         (check-errexit! env s)
         s)]
      [(brace-group? cmd) (execute-brace-group cmd env)]
      [(if-command? cmd) (execute-if cmd env execute-command)]
      [(for-command? cmd) (execute-for cmd env execute-command)]
      [(while-command? cmd)
       (execute-while cmd env execute-command)]
      [(until-command? cmd)
       (execute-until cmd env execute-command)]
      [(case-command? cmd) (execute-case cmd env execute-command)]
      [(select-command? cmd)
       (execute-select cmd env execute-command)]
      [(cond-command? cmd)
       (let ([s (execute-cond-command cmd env)])
         (env-set-last-status! env s)
         (check-errexit! env s)
         s)]
      [(arith-command? cmd)
       (let ([s (execute-arith-command cmd env)])
         (env-set-last-status! env s)
         (check-errexit! env s)
         s)]
      [(arith-for-command? cmd)
       (let ([s (execute-arith-for cmd env execute-command)])
         (env-set-last-status! env s)
         s)]
      [(coproc-command? cmd)
       (let ([s (execute-coproc cmd env)])
         (env-set-last-status! env s)
         s)]
      [(function-def? cmd) (execute-function-def cmd env)]
      [(time-command? cmd) (execute-time-command cmd env)]
      [else
       (fprintf (current-error-port) "gsh: unknown command type~n")
       1]))
  (define (apply-assignment! asgn env)
    (let* ([raw-name (assignment-name asgn)])
      (let* ([name (resolve-nameref raw-name env)])
        (let* ([index (assignment-index asgn)])
          (let* ([raw-value (assignment-value asgn)])
            (let* ([op (assignment-op asgn)])
              (cond
                [(list? raw-value)
                 (let ([expanded (append-map
                                   (lambda (v) (expand-word v env))
                                   raw-value)])
                   (if (eq? op '\x2B;=)
                       (env-array-append-compound! env name expanded)
                       (env-array-set-compound!
                         env
                         name
                         expanded
                         (let ([var (env-get-var env name)])
                           (and var (shell-var-assoc? var))))))]
                [index
                 (let ([expanded-index (expand-word-nosplit index env)]
                       [val (expand-assignment-value raw-value env)])
                   (if (eq? op '\x2B;=)
                       (let ([old (env-array-get env name expanded-index)])
                         (env-array-set!
                           env
                           name
                           expanded-index
                           (string-append old val)))
                       (env-array-set! env name expanded-index val)))]
                [else
                 (let ([val (expand-assignment-value raw-value env)])
                   (if (eq? op '\x2B;=)
                       (let ([old (or (env-get env name) "")])
                         (env-set! env name (string-append old val)))
                       (env-set! env name val)))])))))))
  (define (apply-temp-assignments assignments env)
    (let* ([filtered (filter
                       (lambda (asgn)
                         (and (not (assignment-index asgn))
                              (not (list? (assignment-value asgn)))))
                       assignments)])
      (let* ([child (env-push-scope env)])
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
          filtered)
        child)))
  (define (execute-simple-command cmd env)
    (let* ([assignments (simple-command-assignments cmd)])
      (let* ([raw-words (simple-command-words cmd)])
        (let* ([redirections (simple-command-redirections cmd)])
          (if (null? raw-words)
              (begin
                (parameterize ([*command-sub-ran* #f])
                  (for-each
                    (lambda (asgn) (apply-assignment! asgn env))
                    assignments)
                  (let ([status (if (pair? redirections)
                                    (guard (__exn
                                             [#t
                                              ((lambda (e)
                                                 (fprintf
                                                   (current-error-port)
                                                   "gsh: ~a~n"
                                                   (exception-message e))
                                                 1)
                                                __exn)])
                                      (let ([saved (apply-redirections
                                                     redirections
                                                     env)])
                                        (restore-redirections saved)
                                        (if (*command-sub-ran*)
                                            (shell-environment-last-status
                                              env)
                                            0)))
                                    (if (*command-sub-ran*)
                                        (shell-environment-last-status env)
                                        0))])
                    (env-set-last-status! env status)
                    (check-errexit! env status)
                    status)))
              (parameterize ([*procsub-cleanups* (list)])
                (unwind-protect
                  (let* ([expanded (guard (__exn
                                            [#t
                                             ((lambda (e)
                                                (if (and (pair? e)
                                                         (eq? (car e)
                                                              'failglob))
                                                    #f
                                                    (raise e)))
                                               __exn)])
                                     (if (and (pair? raw-words)
                                              (literal-declaration-builtin?
                                                (car raw-words)))
                                         (let ([first-word (expand-word-nosplit
                                                             (car raw-words)
                                                             env)])
                                           (cons
                                             first-word
                                             (expand-declaration-args
                                               (cdr raw-words)
                                               env)))
                                         (expand-words raw-words env)))])
                    (let* ([cmd-name (if (and expanded (pair? expanded))
                                         (car expanded)
                                         #f)])
                      (let* ([args (if (and expanded (pair? expanded))
                                       (cdr expanded)
                                       (list))])
                        (if (not expanded)
                            (begin (env-set-last-status! env 1) 1)
                            (if (or (not cmd-name) (string=? cmd-name ""))
                                (if (and cmd-name
                                         (string=? cmd-name "")
                                         (word-is-literal-empty?
                                           raw-words))
                                    (begin
                                      (fprintf
                                        (current-error-port)
                                        "gsh: : command not found~n")
                                      127)
                                    (shell-environment-last-status env))
                                (begin
                                  (when (env-option? env "xtrace")
                                    (let ([ps4 (or (env-get env "PS4")
                                                   "+ ")])
                                      (fprintf
                                        (current-error-port)
                                        "~a~a~n"
                                        ps4
                                        (string-join-words expanded))))
                                  (let* ([temp-env (if (and (pair?
                                                              assignments)
                                                            (not (declaration-builtin?
                                                                   cmd-name)))
                                                       (apply-temp-assignments
                                                         assignments
                                                         env)
                                                       env)])
                                    (let* ([status (guard (__exn
                                                            [#t
                                                             ((lambda (e)
                                                                (cond
                                                                  [(return-exception?
                                                                     e)
                                                                   (raise
                                                                     e)]
                                                                  [(break-exception?
                                                                     e)
                                                                   (raise
                                                                     e)]
                                                                  [(continue-exception?
                                                                     e)
                                                                   (raise
                                                                     e)]
                                                                  [(errexit-exception?
                                                                     e)
                                                                   (raise
                                                                     e)]
                                                                  [(nounset-exception?
                                                                     e)
                                                                   (raise
                                                                     e)]
                                                                  [(subshell-exit-exception?
                                                                     e)
                                                                   (raise
                                                                     e)]
                                                                  [else
                                                                   (fprintf
                                                                     (current-error-port)
                                                                     "gsh: ~a~n"
                                                                     (exception-message
                                                                       e))
                                                                   1]))
                                                               __exn)])
                                                     (if (string=?
                                                           cmd-name
                                                           "exec")
                                                         (execute-exec
                                                           args
                                                           redirections
                                                           temp-env)
                                                         (let ([saved (apply-redirections
                                                                        redirections
                                                                        temp-env)])
                                                           (unwind-protect
                                                             (cond
                                                               [(function-lookup
                                                                  env
                                                                  cmd-name) =>
                                                                (lambda (func)
                                                                  (let* ([func-redirs (shell-function-redirections
                                                                                        func)])
                                                                    (let* ([func-saved (if (pair?
                                                                                             func-redirs)
                                                                                           (apply-redirections
                                                                                             func-redirs
                                                                                             temp-env)
                                                                                           (list))])
                                                                      (let* ([result (function-call
                                                                                       func
                                                                                       args
                                                                                       temp-env
                                                                                       execute-command)])
                                                                        (when (pair?
                                                                                func-saved)
                                                                          (restore-redirections
                                                                            func-saved))
                                                                        result))))]
                                                               [(builtin-lookup
                                                                  cmd-name) =>
                                                                (lambda (handler)
                                                                  (handler
                                                                    args
                                                                    temp-env))]
                                                               [else
                                                                (execute-external
                                                                  cmd-name
                                                                  args
                                                                  temp-env)])
                                                             (restore-redirections
                                                               saved)))))])
                                      (env-set-last-status! env status)
                                      (env-array-set-compound!
                                        env
                                        "PIPESTATUS"
                                        (list (number->string status))
                                        #f)
                                      (check-errexit! env status)
                                      status))))))))
                  (run-procsub-cleanups!))))))))
  (define (has-equals-sign? str)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (cond
          [(>= i len) #f]
          [(char=? (string-ref str i) #\=) #t]
          [else (loop (+ i 1))]))))
  (define (declaration-builtin? name)
    (member
      name
      '("export" "declare" "typeset" "local" "readonly")))
  (define (literal-declaration-builtin? raw-word)
    (and (string? raw-word)
         (member
           raw-word
           '("export" "declare" "typeset" "local" "readonly"))
         #t))
  (define (expand-declaration-args words env)
    (append-map
      (lambda (w)
        (let ([s (if (string? w) w (expand-word-nosplit w env))])
          (cond
            [(and (string? s)
                  (> (string-length s) 0)
                  (or (char=? (string-ref s 0) #\-)
                      (char=? (string-ref s 0) #\+)))
             (let ([multi (expand-words (list w) env)])
               (if (> (length multi) 1) multi (list s)))]
            [(and (string? w) (has-equals-sign? w))
             (let* ([eq-pos (string-find-char* w #\=)])
               (let* ([raw-val (and eq-pos
                                    (substring
                                      w
                                      (+ eq-pos 1)
                                      (string-length w)))])
                 (if (and raw-val
                          (> (string-length raw-val) 0)
                          (char=? (string-ref raw-val 0) #\())
                     (let* ([name-part (expand-word-nosplit
                                         (substring w 0 (+ eq-pos 1))
                                         env)])
                       (let* ([inner (if (and (> (string-length raw-val) 1)
                                              (char=?
                                                (string-ref
                                                  raw-val
                                                  (- (string-length
                                                       raw-val)
                                                     1))
                                                #\)))
                                         (substring
                                           raw-val
                                           1
                                           (- (string-length raw-val) 1))
                                         (substring
                                           raw-val
                                           1
                                           (string-length raw-val)))])
                         (let* ([raw-elems (parse-array-compound-raw
                                             inner)])
                           (let* ([brace-elems (if (env-option?
                                                     env
                                                     "braceexpand")
                                                   (append-map
                                                     brace-expand
                                                     raw-elems)
                                                   raw-elems)])
                             (let* ([expanded-elems (map (lambda (e)
                                                           (expand-word-nosplit
                                                             e
                                                             env))
                                                         brace-elems)])
                               (let* ([quoted-elems (map declare-quote-value
                                                         expanded-elems)])
                                 (let* ([rebuilt (string-append
                                                   "("
                                                   (let ([strs quoted-elems]
                                                         [sep " "])
                                                     (if (null? strs)
                                                         ""
                                                         (let lp ([result (car strs)]
                                                                  [rest (cdr strs)])
                                                           (if (null? rest)
                                                               result
                                                               (lp (string-append
                                                                     result
                                                                     sep
                                                                     (car rest))
                                                                   (cdr rest))))))
                                                   ")")])
                                   (list
                                     (string-append
                                       name-part
                                       rebuilt)))))))))
                     (let* ([raw-eq-pos (string-find-char* w #\=)])
                       (let* ([raw-name (and raw-eq-pos
                                             (substring
                                               w
                                               0
                                               (+ raw-eq-pos 1)))])
                         (let* ([raw-val (and raw-eq-pos
                                              (substring
                                                w
                                                (+ raw-eq-pos 1)
                                                (string-length w)))])
                           (if raw-eq-pos
                               (let* ([expanded-name (expand-word-nosplit
                                                       raw-name
                                                       env)])
                                 (let* ([expanded-val (expand-assignment-value
                                                        raw-val
                                                        env)])
                                   (list
                                     (string-append
                                       expanded-name
                                       expanded-val))))
                               (list (expand-word-nosplit w env)))))))))]
            [else (expand-word w env)])))
      words))
  (define (word-is-literal-empty? raw-words)
    (and (pair? raw-words)
         (let ([first-word (car raw-words)])
           (and (string? first-word)
                (let ([len (string-length first-word)])
                  (or (and (= len 2)
                           (char=? (string-ref first-word 0) #\')
                           (char=? (string-ref first-word 1) #\'))
                      (and (= len 2)
                           (char=? (string-ref first-word 0) #\")
                           (char=? (string-ref first-word 1) #\"))))))))
  (define (execute-external cmd-name args env)
    (let* ([path (which-cached cmd-name)])
      (let* ([exec-path (and path
                             (if (string-contains? path "/")
                                 (gambit-path-expand path)
                                 path))])
        (if (not path)
            (begin
              (fprintf
                (current-error-port)
                "gsh: ~a: command not found~n"
                cmd-name)
              127)
            (guard (__exn
                     [#t
                      ((lambda (e)
                         (fprintf
                           (current-error-port)
                           "gsh: ~a: ~a~n"
                           cmd-name
                           (exception-message e))
                         126)
                        __exn)])
              (let* ([pipe-in (*pipeline-stdin-fd*)])
                (let* ([pipe-out (*pipeline-stdout-fd*)])
                  (let* ([in-pipeline? (or pipe-in pipe-out)])
                    (let* ([saved-0 (and pipe-in (ffi-dup 0))])
                      (let* ([saved-1 (and pipe-out (ffi-dup 1))])
                        (when in-pipeline?
                          (mutex-lock! *pipeline-fd-mutex*))
                        (when pipe-in (ffi-dup2 pipe-in 0))
                        (when pipe-out (ffi-dup2 pipe-out 1))
                        (flush-output-port (current-output-port))
                        (flush-output-port (current-error-port))
                        (ffi-lseek-end 1)
                        (ffi-lseek-end 2)
                        (let* ([packed-argv (pack-with-soh
                                              (map string->c-safe
                                                   (cons cmd-name args)))])
                          (let* ([packed-env (pack-with-soh
                                               (map string->c-safe
                                                    (env-exported-alist
                                                      env)))])
                            (let* ([keep-fds (pack-fds-with-soh
                                               (*active-redirect-fds*))])
                              (let* ([pid (ffi-fork-exec (string->c-safe exec-path)
                                            packed-argv packed-env 0
                                            (*gambit-scheduler-rfd*)
                                            (*gambit-scheduler-wfd*)
                                            keep-fds (current-directory))])
                                (when saved-1
                                  (ffi-dup2 saved-1 1)
                                  (ffi-close-fd saved-1))
                                (when saved-0
                                  (ffi-dup2 saved-0 0)
                                  (ffi-close-fd saved-0))
                                (when in-pipeline?
                                  (mutex-unlock! *pipeline-fd-mutex*))
                                (if (< pid 0)
                                    (begin
                                      (fprintf
                                        (current-error-port)
                                        "gsh: ~a: fork failed~n"
                                        cmd-name)
                                      126)
                                    (let-values ([(exit-code stopped?)
                                                  (wait-for-foreground-process-raw
                                                    pid)])
                                      (ffi-sigchld-unblock)
                                      (if stopped?
                                          (let ([cmd-text (string-join-words
                                                            (cons
                                                              cmd-name
                                                              args))]
                                                [job (job-table-add!
                                                       (list (cons pid #f))
                                                       ""
                                                       pid)])
                                            (job-command-text-set!
                                              job
                                              cmd-text)
                                            (job-status-set! job 'stopped)
                                            (for-each
                                              (lambda (p)
                                                (job-process-status-set!
                                                  p
                                                  'stopped))
                                              (job-processes job))
                                            (fprintf
                                              (current-error-port)
                                              "~n[~a]+  Stopped                 ~a~n"
                                              (job-id job)
                                              cmd-text)
                                            (+ 128 20))
                                          (begin
                                            (when (>= exit-code 128)
                                              (let ([sig-name (signal-number->name
                                                                (- exit-code
                                                                   128))])
                                                (when sig-name
                                                  (clear-pending-signal!
                                                    sig-name))))
                                            (let ([end-pos (ffi-lseek-end
                                                             1)])
                                              (when (> end-pos 0)
                                                (guard (__exn
                                                         [#t (void __exn)])
                                                  (output-port-byte-position
                                                    (current-output-port)
                                                    end-pos))))
                                            exit-code))))))))))))))))))
  (define (execute-exec args redirections env)
    (apply-redirections-permanent! redirections env)
    (let parse-flags ([args args] [argv0 #f] [clear-env? #f])
      (cond
        [(null? args) 0]
        [(string=? (car args) "--")
         (exec-command (cdr args) argv0 clear-env? env)]
        [(string=? (car args) "-a")
         (if (pair? (cdr args))
             (parse-flags (cddr args) (cadr args) clear-env?)
             (begin
               (fprintf
                 (current-error-port)
                 "gsh: exec: -a: option requires an argument~n")
               1))]
        [(string=? (car args) "-c")
         (parse-flags (cdr args) argv0 #t)]
        [(string=? (car args) "-l")
         (parse-flags (cdr args) (or argv0 'login) clear-env?)]
        [else (exec-command args argv0 clear-env? env)])))
  (define (exec-command args argv0 clear-env? env)
    (if (null? args)
        0
        (let* ([cmd-name (car args)])
          (let* ([cmd-args (cdr args)])
            (let* ([path (which-cached cmd-name)])
              (if (not path)
                  (begin
                    (fprintf
                      (current-error-port)
                      "gsh: exec: ~a: not found~n"
                      cmd-name)
                    127)
                  (let* ([exec-path (if (string-contains? path "/")
                                        (gambit-path-expand path)
                                        path)])
                    (let* ([env-alist (if clear-env?
                                          (list)
                                          (env-exported-alist env))])
                      (let* ([actual-argv0 (cond
                                             [(eq? argv0 'login)
                                              (string-append "-" cmd-name)]
                                             [(string? argv0) argv0]
                                             [else cmd-name])])
                        (let* ([argv-list (cons actual-argv0 cmd-args)])
                          (let* ([packed-argv (pack-with-soh
                                                (map string->c-safe
                                                     argv-list))])
                            (let* ([packed-env (pack-with-soh
                                                 (map string->c-safe
                                                      env-alist))])
                              (flush-output-port (current-output-port))
                              (flush-output-port (current-error-port))
                              (let ([err (ffi-execve
                                           (string->c-safe exec-path)
                                           packed-argv
                                           packed-env)])
                                (fprintf
                                  (current-error-port)
                                  "gsh: exec: ~a: ~a~n"
                                  cmd-name
                                  (cond
                                    [(= err 13) "Permission denied"]
                                    [(= err 2) "No such file or directory"]
                                    [else
                                     (string-append
                                       "errno "
                                       (number->string err))]))
                                126)))))))))))))
  (define (execute-ast-pipeline cmd env)
    (let* ([commands (ast-pipeline-commands cmd)])
      (let* ([bang? (ast-pipeline-bang? cmd)])
        (let* ([pipe-types (ast-pipeline-pipe-types cmd)])
          (let* ([exit-codes (if bang?
                                 (parameterize ([*in-condition-context*
                                                 #t])
                                   (execute-pipeline
                                     commands
                                     env
                                     execute-command
                                     pipe-types))
                                 (execute-pipeline
                                   commands
                                   env
                                   execute-command
                                   pipe-types))])
            (let* ([last-status (if (pair? exit-codes)
                                    (last-elem exit-codes)
                                    0)])
              (let* ([status (if (env-option? env "pipefail")
                                 (let loop ([codes exit-codes] [fail 0])
                                   (if (null? codes)
                                       (if (= fail 0) last-status fail)
                                       (loop
                                         (cdr codes)
                                         (if (not (= (car codes) 0))
                                             (car codes)
                                             fail))))
                                 last-status)])
                (env-array-set-compound!
                  env
                  "PIPESTATUS"
                  (map number->string exit-codes)
                  #f)
                (let ([final (if bang? (if (= status 0) 1 0) status)])
                  (env-set-last-status! env final)
                  final))))))))
  (define (execute-and-or cmd env)
    (let* ([rest-items (and-or-list-rest cmd)])
      (let* ([first-in-condition? (pair? rest-items)])
        (let* ([status (if first-in-condition?
                           (parameterize ([*in-condition-context* #t])
                             (execute-command (and-or-list-first cmd) env))
                           (execute-command (and-or-list-first cmd) env))])
          (let loop ([rest rest-items] [status status])
            (if (null? rest)
                (begin (env-set-last-status! env status) status)
                (let* ([item (car rest)])
                  (let* ([op (car item)])
                    (let* ([pipeline (cdr item)])
                      (let* ([is-condition? (pair? (cdr rest))])
                        (cond
                          [(and (eq? op 'and) (= status 0))
                           (if is-condition?
                               (loop
                                 (cdr rest)
                                 (parameterize ([*in-condition-context*
                                                 #t])
                                   (execute-command pipeline env)))
                               (loop
                                 (cdr rest)
                                 (execute-command pipeline env)))]
                          [(and (eq? op 'or) (not (= status 0)))
                           (if is-condition?
                               (loop
                                 (cdr rest)
                                 (parameterize ([*in-condition-context*
                                                 #t])
                                   (execute-command pipeline env)))
                               (loop
                                 (cdr rest)
                                 (execute-command pipeline env)))]
                          [else (loop (cdr rest) status)])))))))))))
  (define (execute-command-list cmd env)
    (let loop ([items (command-list-items cmd)] [status 0])
      (if (null? items)
          (begin (env-set-last-status! env status) status)
          (let* ([item (car items)])
            (let* ([mode (car item)])
              (let* ([command (cdr item)])
                (case mode
                  [(sequential)
                   (let ([new-status (execute-command command env)])
                     (env-set-last-status! env new-status)
                     (thread-yield!)
                     (let ([trap-fn (*process-traps-fn*)])
                       (when trap-fn (trap-fn env)))
                     (loop (cdr items) new-status))]
                  [(background)
                   (let ([result (launch-background command env)])
                     (env-set-last-bg-pid! env (car result))
                     (let ([job (job-table-add!
                                  (cdr result)
                                  (or (ast->command-text command) "&")
                                  (car result))])
                       (when (*interactive-shell*)
                         (fprintf
                           (current-error-port)
                           "[~a] ~a~n"
                           (job-id job)
                           (car result))))
                     (loop (cdr items) 0))]
                  [else
                   (loop (cdr items) (execute-command command env))])))))))
  (define (execute-subshell cmd env)
    (let ([child-env (env-clone env)]
          [saved-cwd (current-directory)])
      (let ([status (guard (__exn
                             [#t
                              ((lambda (e)
                                 (cond
                                   [(subshell-exit-exception? e)
                                    (subshell-exit-exception-status e)]
                                   [(errexit-exception? e)
                                    (errexit-exception-status e)]
                                   [(nounset-exception? e)
                                    (nounset-exception-status e)]
                                   [(break-exception? e) 0]
                                   [(continue-exception? e) 0]
                                   [(return-exception? e)
                                    (return-exception-status e)]
                                   [else (raise e)]))
                                __exn)])
                      (parameterize ([*in-subshell* #t] [*loop-depth* 0])
                        (execute-command (subshell-body cmd) child-env)))])
        (current-directory saved-cwd)
        (env-set-last-status! env status)
        status)))
  (define (execute-redirected-command cmd env)
    (let ([inner (redirected-command-command cmd)]
          [redirs (redirected-command-redirections cmd)])
      (guard (__exn
               [#t
                ((lambda (e)
                   (cond
                     [(return-exception? e) (raise e)]
                     [(break-exception? e) (raise e)]
                     [(continue-exception? e) (raise e)]
                     [(errexit-exception? e) (raise e)]
                     [(nounset-exception? e) (raise e)]
                     [(subshell-exit-exception? e) (raise e)]
                     [else
                      (fprintf
                        (current-error-port)
                        "gsh: ~a~n"
                        (exception-message e))
                      1]))
                  __exn)])
        (let ([saved (apply-redirections redirs env)])
          (let ([result (execute-command inner env)])
            (restore-redirections saved)
            result)))))
  (define (execute-brace-group cmd env)
    (execute-command (brace-group-body cmd) env))
  (define (execute-function-def cmd env)
    (function-define! env (function-def-name cmd) (function-def-body cmd)
      (function-def-redirections cmd) (function-def-lineno cmd)
      (*current-source-file*))
    0)
  (define (execute-cond-command cmd env)
    (if (eval-cond-expr (cond-command-expr cmd) env) 0 1))
  (define (eval-cond-expr expr env)
    (cond
      [(cond-binary? expr)
       (let ([op (cond-binary-op expr)])
         (cond
           [(string=? op "&&")
            (and (eval-cond-expr (cond-binary-left expr) env)
                 (eval-cond-expr (cond-binary-right expr) env))]
           [(string=? op "||")
            (or (eval-cond-expr (cond-binary-left expr) env)
                (eval-cond-expr (cond-binary-right expr) env))]
           [else #f]))]
      [(cond-not? expr)
       (not (eval-cond-expr (cond-not-expr expr) env))]
      [(cond-unary-test? expr)
       (eval-cond-unary
         (cond-unary-test-op expr)
         (expand-word-nosplit (cond-unary-test-arg expr) env)
         env)]
      [(cond-binary-test? expr)
       (eval-cond-binary-test
         (cond-binary-test-op expr)
         (expand-word-nosplit (cond-binary-test-left expr) env)
         (cond-binary-test-right expr)
         env)]
      [(cond-word? expr)
       (let ([val (expand-word-nosplit
                    (cond-word-value expr)
                    env)])
         (and val (> (string-length val) 0)))]
      [else #f]))
  (define (eval-cond-unary op arg env)
    (cond
      [(string=? op "-z") (= (string-length arg) 0)]
      [(string=? op "-n") (> (string-length arg) 0)]
      [(string=? op "-e") (file-exists? arg)]
      [(string=? op "-f") (file-regular? arg)]
      [(string=? op "-d") (file-directory? arg)]
      [(string=? op "-L") (file-symlink? arg)]
      [(string=? op "-h") (eval-cond-unary "-L" arg env)]
      [(string=? op "-r")
       (and (file-exists? arg) (cond-file-access? arg 4))]
      [(string=? op "-w")
       (and (file-exists? arg) (cond-file-access? arg 2))]
      [(string=? op "-x")
       (and (file-exists? arg) (cond-file-access? arg 1))]
      [(string=? op "-s") (file-nonempty? arg)]
      [(string=? op "-p")
       (and (file-exists? arg)
            (eq? (gambit-file-info-type (gambit-file-info arg)) 'fifo))]
      [(string=? op "-S")
       (and (file-exists? arg)
            (eq? (gambit-file-info-type (gambit-file-info arg))
                 'socket))]
      [(string=? op "-b")
       (and (file-exists? arg)
            (eq? (gambit-file-info-type (gambit-file-info arg))
                 'block-special))]
      [(string=? op "-c")
       (and (file-exists? arg)
            (eq? (gambit-file-info-type (gambit-file-info arg))
                 'character-special))]
      [(string=? op "-t")
       (let ([fd (string->number arg)])
         (and fd (= (ffi-isatty fd) 1)))]
      [(string=? op "-a") (file-exists? arg)]
      [(string=? op "-v")
       (let ([val (env-get env arg)]) (and val #t))]
      [(string=? op "-o") #f]
      [(string=? op "-g")
       (and (file-exists? arg)
            (not (zero?
                   (bitwise-and
                     (file-info-mode (gambit-file-info arg))
                     1024))))]
      [(string=? op "-u")
       (and (file-exists? arg)
            (not (zero?
                   (bitwise-and
                     (file-info-mode (gambit-file-info arg))
                     2048))))]
      [(string=? op "-k")
       (and (file-exists? arg)
            (not (zero?
                   (bitwise-and
                     (file-info-mode (gambit-file-info arg))
                     512))))]
      [(string=? op "-G")
       (and (file-exists? arg)
            (= (file-info-group (gambit-file-info arg)) (ffi-getegid)))]
      [(string=? op "-O")
       (and (file-exists? arg)
            (= (file-info-owner (gambit-file-info arg)) (ffi-geteuid)))]
      [(string=? op "-N")
       (and (file-exists? arg)
            (let ([fi (gambit-file-info arg)])
              (> (let ([t (file-info-last-modification-time fi)])
                   (if (time? t)
                       (+ (time-second t)
                          (/ (time-nanosecond t) 1000000000.0))
                       t))
                 (let ([t (file-info-last-access-time fi)])
                   (if (time? t)
                       (+ (time-second t)
                          (/ (time-nanosecond t) 1000000000.0))
                       t)))))]
      [(string=? op "-R")
       (let ([var (env-get-var env arg)])
         (and var (shell-var-nameref? var)))]
      [else #f]))
  (define (cond-file-access? path mode)
    (= (ffi-access path mode) 0))
  (define (eval-cond-binary-test op left right-raw env)
    (cond
      [(string=? op "=~")
       (let ([right (expand-word-nosplit right-raw env)])
         (guard (__exn
                  [#t
                   ((lambda (e)
                      (fprintf
                        (current-error-port)
                        "gsh: [[ =~ ]]: invalid regex: ~a~n"
                        right)
                      #f)
                     __exn)])
           (let ([m (pregexp-match right left)])
             (if m
                 (begin
                   (env-array-set-compound!
                     env
                     "BASH_REMATCH"
                     (map (lambda (v) (or v "")) m)
                     #f)
                   #t)
                 (begin
                   (env-array-set-compound! env "BASH_REMATCH" (list) #f)
                   #f)))))]
      [(or (string=? op "=") (string=? op "=="))
       (let ([right (expand-word-nosplit right-raw env)])
         (glob-match? right left #f (env-shopt? env "extglob")))]
      [(string=? op "!=")
       (let ([right (expand-word-nosplit right-raw env)])
         (not (glob-match?
                right
                left
                #f
                (env-shopt? env "extglob"))))]
      [(string=? op "<")
       (string<? left (expand-word-nosplit right-raw env))]
      [(string=? op ">")
       (string>? left (expand-word-nosplit right-raw env))]
      [(string=? op "-eq") (cond-int-cmp = left right-raw env)]
      [(string=? op "-ne")
       (cond-int-cmp
         (lambda (a b) (not (= a b)))
         left
         right-raw
         env)]
      [(string=? op "-lt") (cond-int-cmp < left right-raw env)]
      [(string=? op "-gt") (cond-int-cmp > left right-raw env)]
      [(string=? op "-le") (cond-int-cmp <= left right-raw env)]
      [(string=? op "-ge") (cond-int-cmp >= left right-raw env)]
      [(string=? op "-ef")
       (cond-same-file? left (expand-word-nosplit right-raw env))]
      [(string=? op "-nt")
       (cond-newer-than? left (expand-word-nosplit right-raw env))]
      [(string=? op "-ot")
       (cond-newer-than? (expand-word-nosplit right-raw env) left)]
      [else #f]))
  (define (arith-string-to-int str env)
    (let ([n (string->number str)])
      (if n
          n
          (guard (__exn [#t ((lambda (e) #f) __exn)])
            (arith-eval
              str
              (arith-env-getter env)
              (arith-env-setter env))))))
  (define (cond-int-cmp cmp left right-raw env)
    (let ([right (expand-word-nosplit right-raw env)])
      (let ([a (arith-string-to-int left env)]
            [b (arith-string-to-int right env)])
        (and a b (cmp a b)))))
  (define (cond-same-file? a b)
    (and (file-exists? a)
         (file-exists? b)
         (let ([fi-a (gambit-file-info a)]
               [fi-b (gambit-file-info b)])
           (and (= (file-info-device fi-a) (file-info-device fi-b))
                (= (file-info-inode fi-a) (file-info-inode fi-b))))))
  (define (cond-newer-than? a b)
    (and (file-exists? a)
         (file-exists? b)
         (> (let ([t (file-info-last-modification-time
                       (gambit-file-info a))])
              (if (time? t)
                  (+ (time-second t) (/ (time-nanosecond t) 1000000000.0))
                  t))
            (let ([t (file-info-last-modification-time
                       (gambit-file-info b))])
              (if (time? t)
                  (+ (time-second t) (/ (time-nanosecond t) 1000000000.0))
                  t)))))
  (define (execute-arith-command cmd env)
    (guard (__exn
             [#t
              ((lambda (e)
                 (cond
                   [(nounset-exception? e) (raise e)]
                   [else
                    (fprintf
                      (current-error-port)
                      "gsh: ~a~n"
                      (exception-message e))
                    1]))
                __exn)])
      (let* ([raw-expr (arith-command-expression cmd)])
        (let* ([expr (parameterize ([*in-dquote-context* #t])
                       (expand-arith-expr raw-expr env))])
          (let* ([result (arith-eval
                           expr
                           (arith-env-getter env)
                           (arith-env-setter env)
                           (and (env-option? env "nounset")
                                (lambda (name)
                                  (nounset-error! name env))))])
            (if (= result 0) 1 0))))))
  (define (execute-arith-for cmd env exec-fn)
    (let ([getter (arith-env-getter env)]
          [setter (arith-env-setter env)]
          [expand (lambda (expr)
                    (if expr
                        (parameterize ([*in-dquote-context* #t])
                          (expand-arith-expr expr env))
                        expr))])
      (let ([init-expr (arith-for-command-init cmd)])
        (when (and init-expr (> (string-length init-expr) 0))
          (arith-eval (expand init-expr) getter setter)))
      (parameterize ([*loop-depth* (+ (*loop-depth*) 1)])
        (let ([status 0])
          (let loop ()
            (let* ([test-expr (arith-for-command-test cmd)])
              (let* ([test-val (if (and test-expr
                                        (> (string-length test-expr) 0))
                                   (arith-eval
                                     (expand test-expr)
                                     getter
                                     setter)
                                   1)])
                (if (= test-val 0)
                    status
                    (let ([caught (guard (__exn
                                           [#t
                                            ((lambda (e)
                                               (cond
                                                 [(break-exception? e)
                                                  (cons 'break e)]
                                                 [(continue-exception? e)
                                                  (cons 'continue e)]
                                                 [else (raise e)]))
                                              __exn)])
                                    (set! status
                                      (exec-fn
                                        (arith-for-command-body cmd)
                                        env))
                                    (let ([update-expr (arith-for-command-update
                                                         cmd)])
                                      (when (and update-expr
                                                 (> (string-length
                                                      update-expr)
                                                    0))
                                        (arith-eval
                                          (expand update-expr)
                                          getter
                                          setter)))
                                    #f)])
                      (cond
                        [(not caught) (loop)]
                        [(eq? (car caught) 'break)
                         (let ([levels (break-exception-levels
                                         (cdr caught))])
                           (if (> levels 1)
                               (raise (make-break-exception (- levels 1)))
                               status))]
                        [(eq? (car caught) 'continue)
                         (let ([levels (continue-exception-levels
                                         (cdr caught))])
                           (if (> levels 1)
                               (raise
                                 (make-continue-exception (- levels 1)))
                               (begin
                                 (let ([update-expr (arith-for-command-update
                                                      cmd)])
                                   (when (and update-expr
                                              (> (string-length
                                                   update-expr)
                                                 0))
                                     (arith-eval
                                       (expand update-expr)
                                       getter
                                       setter)))
                                 (loop))))]))))))))))
  (define (check-errexit! env status)
    (when (and (not (= status 0))
               (env-option? env "errexit")
               (not (*in-condition-context*)))
      (raise (make-errexit-exception status))))
  (define (execute-time-command cmd env)
    (let* ([posix? (time-command-posix? cmd)])
      (let* ([pipeline (time-command-pipeline cmd)])
        (let* ([start-real (real-time)])
          (let* ([start-cpu (cpu-time)])
            (let* ([status (if pipeline
                               (execute-command pipeline env)
                               0)])
              (let* ([end-real (real-time)])
                (let* ([end-cpu (cpu-time)])
                  (let* ([elapsed-real (fl- end-real start-real)])
                    (let* ([elapsed-user (fl- end-cpu start-cpu)])
                      (let* ([elapsed-sys 0.0])
                        (if posix?
                            (begin
                              (fprintf
                                (current-error-port)
                                "real ~a~n"
                                (format-time-posix elapsed-real))
                              (fprintf
                                (current-error-port)
                                "user ~a~n"
                                (format-time-posix elapsed-user))
                              (fprintf
                                (current-error-port)
                                "sys ~a~n"
                                (format-time-posix elapsed-sys)))
                            (begin
                              (fprintf
                                (current-error-port)
                                "~nreal\t~a~n"
                                (format-time-bash elapsed-real))
                              (fprintf
                                (current-error-port)
                                "user\t~a~n"
                                (format-time-bash elapsed-user))
                              (fprintf
                                (current-error-port)
                                "sys\t~a~n"
                                (format-time-bash elapsed-sys))))
                        (env-set-last-status! env status)
                        status)))))))))))
  (define (format-time-bash secs)
    (let* ([mins (inexact->exact (floor (fl/ secs 60.0)))])
      (let* ([remaining (fl- secs
                             (fl* (exact->inexact mins) 60.0))])
        (format
          "~am~a.~as"
          mins
          (inexact->exact (floor remaining))
          (let ([frac (fl- remaining (flfloor remaining))])
            (let ([ms (inexact->exact (floor (fl* frac 1000.0)))])
              (string-append
                (if (< ms 100) "0" "")
                (if (< ms 10) "0" "")
                (number->string ms))))))))
  (define (format-time-posix secs)
    (let* ([mins (inexact->exact (floor (fl/ secs 60.0)))])
      (let* ([remaining (fl- secs
                             (fl* (exact->inexact mins) 60.0))])
        (format
          "~a.~a"
          mins
          (let ([cs (inexact->exact (floor (fl* remaining 100.0)))])
            (string-append
              (if (< cs 10) "0" "")
              (number->string cs)))))))
  (define (return-status env status)
    (env-set-last-status! env status)
    status)
  (define (string-join-words words)
    (if (null? words)
        ""
        (call-with-output-string
          (lambda (port)
            (display (car words) port)
            (for-each
              (lambda (w) (display " " port) (display w port))
              (cdr words))))))
  (define (execute-coproc cmd env)
    (let ([name (coproc-command-name cmd)]
          [body (coproc-command-command cmd)])
      (let-values ([(p2c-read p2c-write) (ffi-pipe-raw)]
                   [(c2p-read c2p-write) (ffi-pipe-raw)])
        (let ([saved-stdin (ffi-dup 0)] [saved-stdout (ffi-dup 1)])
          (ffi-dup2 p2c-read 0)
          (ffi-dup2 c2p-write 1)
          (let ([result (if (simple-command? body)
                            (launch-background-simple body env)
                            (let* ([thread-in-fd (ffi-dup 0)])
                              (let* ([thread-out-fd (ffi-dup 1)])
                                (let* ([exit-box (box 0)])
                                  (let ([t (spawn
                                             (lambda ()
                                               (let ([in-port (open-input-file
                                                                (string-append
                                                                  "/dev/fd/"
                                                                  (number->string
                                                                    thread-in-fd)))]
                                                     [out-port (open-output-file
                                                                 (string-append
                                                                   "/dev/fd/"
                                                                   (number->string
                                                                     thread-out-fd)))])
                                                 (parameterize ([current-input-port
                                                                 in-port]
                                                                [current-output-port
                                                                 out-port])
                                                   (let ([status (execute-command
                                                                   body
                                                                   env)])
                                                     (set-box!
                                                       exit-box
                                                       status)
                                                     (flush-output-port
                                                       out-port)
                                                     (close-port out-port)
                                                     (ffi-close-fd
                                                       thread-out-fd)
                                                     (close-port in-port)
                                                     (ffi-close-fd
                                                       thread-in-fd))))))])
                                    (cons 0 (list)))))))])
            (ffi-dup2 saved-stdin 0)
            (ffi-dup2 saved-stdout 1)
            (ffi-close-fd saved-stdin)
            (ffi-close-fd saved-stdout)
            (ffi-close-fd p2c-read)
            (ffi-close-fd c2p-write)
            (env-array-set! env name 0 (number->string c2p-read))
            (env-array-set! env name 1 (number->string p2c-write))
            (let ([pid (car result)])
              (env-set!
                env
                (string-append name "_PID")
                (number->string pid))
              (env-set-last-bg-pid! env pid)
              (let ([job (job-table-add!
                           (cdr result)
                           (string-append
                             "coproc "
                             (or (ast->command-text body) name))
                           pid)])
                (when (*interactive-shell*)
                  (fprintf
                    (current-error-port)
                    "[~a] ~a~n"
                    (job-id job)
                    pid)))
              0))))))
  (define *next-fake-pid*-cell (vector 100000))
  (define-syntax *next-fake-pid*
    (identifier-syntax
      [id (vector-ref *next-fake-pid*-cell 0)]
      [(set! id v) (vector-set! *next-fake-pid*-cell 0 v)]))
  (define (next-fake-pid!)
    (let ([pid *next-fake-pid*])
      (set! *next-fake-pid* (+ pid 1))
      pid))
  (define (launch-background cmd env)
    (if (simple-command? cmd)
        (launch-background-simple cmd env)
        (let* ([fake-pid (next-fake-pid!)])
          (let* ([child-env (env-clone env)])
            (let* ([th (spawn
                         (lambda ()
                           (guard (__exn
                                    [#t
                                     ((lambda (e)
                                        (cond
                                          [(subshell-exit-exception? e)
                                           (subshell-exit-exception-status
                                             e)]
                                          [(errexit-exception? e)
                                           (errexit-exception-status e)]
                                          [else 1]))
                                       __exn)])
                             (parameterize ([*in-subshell* #t])
                               (execute-command cmd child-env)))))])
              (cons fake-pid (list (cons fake-pid th))))))))
  (define (launch-background-simple cmd env)
    (let* ([raw-words (simple-command-words cmd)])
      (let* ([first-raw (and (pair? raw-words)
                             (if (token? (car raw-words))
                                 (token-value (car raw-words))
                                 (car raw-words)))])
        (let* ([cmd-name (and first-raw
                              (string? first-raw)
                              (not (string-index first-raw #\$))
                              (not (string-index first-raw #\`))
                              first-raw)])
          (if (and cmd-name
                   (not (function-lookup env cmd-name))
                   (not (builtin-lookup cmd-name)))
              (let* ([assignments (simple-command-assignments cmd)])
                (let* ([expanded (expand-words raw-words env)])
                  (let* ([actual-args (if (pair? expanded)
                                          (cdr expanded)
                                          (list))])
                    (let* ([path (which-cached cmd-name)])
                      (if path
                          (let* ([temp-env (if (pair? assignments)
                                               (let ([child (env-push-scope
                                                              env)])
                                                 (for-each
                                                   (lambda (asgn)
                                                     (apply-assignment!
                                                       asgn
                                                       child))
                                                   assignments)
                                                 child)
                                               env)])
                            (let* ([exec-path (if (string-contains?
                                                    path
                                                    "/")
                                                  (gambit-path-expand path)
                                                  path)])
                              (let* ([_flush (begin
                                               (flush-output-port
                                                 (current-output-port))
                                               (flush-output-port
                                                 (current-error-port)))])
                                (let* ([packed-argv (pack-with-soh
                                                      (map string->c-safe
                                                           (cons
                                                             cmd-name
                                                             actual-args)))])
                                  (let* ([packed-env (pack-with-soh
                                                       (map string->c-safe
                                                            (env-exported-alist
                                                              temp-env)))])
                                    (let* ([keep-fds (pack-fds-with-soh
                                                       (*active-redirect-fds*))])
                                      (let* ([pid (ffi-fork-exec
                                                    (string->c-safe
                                                      exec-path)
                                                    packed-argv packed-env
                                                    -1
                                                    (*gambit-scheduler-rfd*)
                                                    (*gambit-scheduler-wfd*)
                                                    keep-fds
                                                    (current-directory))])
                                        (ffi-sigchld-unblock)
                                        (if (< pid 0)
                                            (let* ([fake-pid (next-fake-pid!)])
                                              (let* ([th (spawn
                                                           (lambda ()
                                                             (fprintf
                                                               (current-error-port)
                                                               "gsh: ~a: fork failed~n"
                                                               cmd-name)
                                                             126))])
                                                (cons
                                                  fake-pid
                                                  (list
                                                    (cons fake-pid th)))))
                                            (cons
                                              pid
                                              (list (cons pid #f)))))))))))
                          (let* ([fake-pid (next-fake-pid!)])
                            (let* ([child-env (env-clone env)])
                              (let* ([th (spawn
                                           (lambda ()
                                             (parameterize ([*in-subshell*
                                                             #t])
                                               (execute-command
                                                 cmd
                                                 child-env))))])
                                (cons
                                  fake-pid
                                  (list (cons fake-pid th)))))))))))
              (let* ([fake-pid (next-fake-pid!)])
                (let* ([child-env (env-clone env)])
                  (let* ([th (spawn
                               (lambda ()
                                 (guard (__exn
                                          [#t
                                           ((lambda (e)
                                              (cond
                                                [(subshell-exit-exception?
                                                   e)
                                                 (subshell-exit-exception-status
                                                   e)]
                                                [(errexit-exception? e)
                                                 (errexit-exception-status
                                                   e)]
                                                [else 1]))
                                             __exn)])
                                   (parameterize ([*in-subshell* #t])
                                     (execute-command cmd child-env)))))])
                    (cons fake-pid (list (cons fake-pid th)))))))))))
  (define (ast->command-text cmd)
    (cond
      [(simple-command? cmd)
       (let ([words (simple-command-words cmd)])
         (if (pair? words) (string-join-words words) #f))]
      [(ast-pipeline? cmd)
       (let ([cmds (ast-pipeline-commands cmd)])
         (string-join-words
           (map (lambda (c) (or (ast->command-text c) "?")) cmds)))]
      [(coproc-command? cmd)
       (string-append
         "coproc "
         (coproc-command-name cmd)
         " "
         (or (ast->command-text (coproc-command-command cmd))
             "..."))]
      [(time-command? cmd)
       (string-append
         "time "
         (or (ast->command-text (time-command-pipeline cmd)) ""))]
      [else #f])))
