#!chezscheme
(library (jsh main)
  (export main)
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
   (except (jsh util) string-index string-join file-directory?
     string-join string-index string-downcase file-regular?
     string-upcase)
   (jsh ast) (jsh environment) (jsh lexer) (jsh parser)
   (jsh executor) (jsh expander) (jsh functions) (jsh registry)
   (except (jsh builtins) list-head)
   (except (jsh history) string-trim)
   (except (jsh prompt) string-prefix?) (jsh fuzzy)
   (except (jsh lineedit) user-name) (jsh fzf) (jsh completion)
   (jsh signals) (except (jsh jobs) any every find)
   (jsh script) (jsh startup) (jsh arithmetic) (jsh ffi)
   (jsh stage)
   (only (jsh sandbox) *current-jsh-env*))
  ;; Force invocation of (jsh builtins) for defbuiltin registration
  (define _force-builtins special-builtin?)
  (define (parse-args args)
    (let ([result (let ([ht (make-hash-table)])
                    (hash-put! ht 'command #f)
                    (hash-put! ht 'script #f)
                    (hash-put! ht 'login? #f)
                    (hash-put! ht 'interactive? #f)
                    (hash-put! ht 'verbose? #f)
                    (hash-put! ht 'errexit? #f)
                    (hash-put! ht 'args (list))
                    ht)])
      (let loop ([args args])
        (cond
          [(null? args) result]
          [(string=? (car args) "-c")
           (when (pair? (cdr args))
             (hash-put! result 'command (cadr args))
             (when (pair? (cddr args))
               (hash-put! result 'args (cddr args))))
           result]
          [(or (string=? (car args) "--login")
               (string=? (car args) "-l"))
           (hash-put! result 'login? #t)
           (loop (cdr args))]
          [(string=? (car args) "-i")
           (hash-put! result 'interactive? #t)
           (loop (cdr args))]
          [(string=? (car args) "-e")
           (hash-put! result 'errexit? #t)
           (loop (cdr args))]
          [(string=? (car args) "-x")
           (hash-put! result 'verbose? #t)
           (loop (cdr args))]
          [(string=? (car args) "--")
           (when (pair? (cdr args))
             (hash-put! result 'script (cadr args))
             (hash-put! result 'args (cddr args)))
           result]
          [(and (> (string-length (car args)) 0)
                (char=? (string-ref (car args) 0) #\-))
           (loop (cdr args))]
          [else
           (hash-put! result 'script (car args))
           (hash-put! result 'args (cdr args))
           result]))
      result))
  (define (register-late-builtins! env)
    (*execute-input*
      (lambda (input env) (execute-string input env)))
    (*arith-eval-fn* arith-eval)
    (*execute-external-fn* execute-external)
    (*process-traps-fn* (lambda (env) (process-traps! env)))
    (let ([source-handler (lambda (args env)
                            (let ([args (if (and (pair? args)
                                                 (string=?
                                                   (car args)
                                                   "--"))
                                            (cdr args)
                                            args)])
                              (if (null? args)
                                  (begin
                                    (fprintf
                                      (current-error-port)
                                      "jsh: source: filename argument required~n")
                                    2)
                                  (let* ([filename (car args)])
                                    (let* ([filepath (if (string-contains?
                                                           filename
                                                           "/")
                                                         filename
                                                         (or (find-file-in-path
                                                               filename
                                                               (env-get
                                                                 env
                                                                 "PATH"))
                                                             filename))])
                                      (if (pair? (cdr args))
                                          (let ([saved-pos (shell-environment-positional
                                                             env)])
                                            (env-set-positional!
                                              env
                                              (cdr args))
                                            (let ([result (source-file!
                                                            filepath
                                                            env)])
                                              (shell-environment-positional-set!
                                                env
                                                saved-pos)
                                              result))
                                          (source-file!
                                            filepath
                                            env)))))))])
      (builtin-register! "source" source-handler)
      (builtin-register! "." source-handler)))
  (define (init-shell-env args-hash)
    (let ([env (make-shell-environment)])
      (env-init! env)
      (env-set!
        env
        "SHELL"
        (or (getenv "GSH_EXE" #f)
            (gambit-path-normalize (car (command-line)))))
      (env-set! env "GSH_VERSION" "0.1.0")
      (unless (env-get env "PS1")
        (env-set! env "PS1" "\\u@\\h:\\w\\$ "))
      (unless (env-get env "PS2") (env-set! env "PS2" "> "))
      (unless (env-get env "PS4") (env-set! env "PS4" "+ "))
      (unless (env-get env "HISTFILE")
        (let ([home (or (env-get env "HOME") (home-directory))])
          (env-set!
            env
            "HISTFILE"
            (string-append home "/.gsh_history"))))
      (unless (env-get env "HISTSIZE")
        (env-set! env "HISTSIZE" "1000"))
      (when (hash-ref args-hash 'verbose?)
        (env-option-set! env "xtrace" #t))
      (when (hash-ref args-hash 'errexit?)
        (env-option-set! env "errexit" #t))
      (register-late-builtins! env)
      env))
  (define (repl env)
    (let ([histfile (or (env-get env "HISTFILE")
                        "~/.gsh_history")]
          [histsize (or (string->number
                          (or (env-get env "HISTSIZE") "1000"))
                        1000)])
      (history-init! histfile histsize))
    (setup-default-signal-handlers!)
    (release-user-fds!)
    (when (= (ffi-isatty 0) 1) (ffi-termios-save 0 0))
    (let ([edit-mode (if (env-option? env "vi") 'vi 'emacs)])
      (let loop ([cmd-num 1])
        (let* ([ps1 (or (env-get env "PS1") "$ ")])
          (let* ([prompt-str (expand-prompt ps1 (lambda (name) (env-get env name))
                               (job-count) cmd-num (history-count)
                               (lambda (cmd)
                                 (let ([saved-stdin (ffi-dup 0)])
                                   (guard (__exn
                                            [#t
                                             ((lambda (e)
                                                (ffi-dup2 saved-stdin 0)
                                                (ffi-close-fd saved-stdin)
                                                "")
                                               __exn)])
                                     (let ([null-fd (ffi-open-raw
                                                      "/dev/null"
                                                      0
                                                      0)])
                                       (ffi-dup2 null-fd 0)
                                       (ffi-close-fd null-fd)
                                       (let ([result (command-substitute
                                                       cmd
                                                       env)])
                                         (ffi-dup2 saved-stdin 0)
                                         (ffi-close-fd saved-stdin)
                                         result))))))])
            (let* ([complete-fn (lambda (line cursor)
                                  (complete-word line cursor env))])
              (let* ([input (line-edit prompt-str complete-fn edit-mode)])
                (cond
                  [(eq? input 'eof)
                   (when (env-option? env "ignoreeof")
                     (fprintf
                       (current-error-port)
                       "Use \"exit\" to leave the shell.~n")
                     (loop cmd-num))
                   (run-exit-trap! env)
                   (newline (current-error-port))]
                  [(string=? (string-trim-whitespace input) "")
                   (loop cmd-num)]
                  [(scheme-eval-line? input)
                   (let* ([expr-str (extract-scheme-expr input)])
                     (let* ([result-status (eval-scheme-expr expr-str)])
                       (let* ([result (car result-status)])
                         (let* ([status (cdr result-status)])
                           (unless (string=? result "")
                             (display result)
                             (newline))
                           (history-add! input)
                           (env-set-last-status! env status)
                           (loop cmd-num)))))]
                  [else
                   (let ([result (guard (__exn
                                          [#t
                                           ((lambda (e)
                                              (fprintf
                                                (current-error-port)
                                                "jsh: ~a~n"
                                                (exception-message e))
                                              #f)
                                             __exn)])
                                   (history-expand input))])
                     (when result
                       (let ([expanded (car result)]
                             [execute? (cdr result)])
                         (when (not (string=? expanded input))
                           (begin
                             (display expanded)
                             (display (current-error-port))
                             (newline)))
                         (history-add! expanded)
                         (when execute?
                           (let ([status (guard (__exn
                                                  [#t
                                                   ((lambda (e)
                                                      (cond
                                                        [(break-exception?
                                                           e)
                                                         0]
                                                        [(continue-exception?
                                                           e)
                                                         0]
                                                        [(return-exception?
                                                           e)
                                                         (return-exception-status
                                                           e)]
                                                        [else (raise e)]))
                                                     __exn)])
                                           (execute-input expanded env))])
                             (env-set-last-status! env status)
                             (process-traps! env)
                             (when (and (not (= status 0))
                                        (trap-get "ERR"))
                               (let ([action (trap-get "ERR")])
                                 (when (string? action)
                                   (execute-input action env)))))))))
                   (when (= (ffi-isatty 0) 1)
                     (tty-mode-set! (current-input-port) #t #t #f #f 0))
                   (loop (+ cmd-num 1))]))))))))
  (define (execute-input input env)
    (if (scheme-eval-line? input)
        (let* ([expr-str (extract-scheme-expr input)])
          (let* ([result-status (eval-scheme-expr expr-str)])
            (let* ([result (car result-status)])
              (let* ([status (cdr result-status)])
                (unless (string=? result "") (display result) (newline))
                status))))
        (guard (__exn
                 [#t
                  ((lambda (e)
                     (cond
                       [(nounset-exception? e) (raise e)]
                       [(errexit-exception? e)
                        (errexit-exception-status e)]
                       [(subshell-exit-exception? e) (raise e)]
                       [(break-exception? e) (raise e)]
                       [(continue-exception? e) (raise e)]
                       [(return-exception? e) (raise e)]
                       [else
                        (let ([msg (exception-message e)])
                          (fprintf (current-error-port) "jsh: ~a~n" msg)
                          (if (and (string? msg)
                                   (or (let ([pfx "parse error"] [str msg])
                                         (let ([plen (string-length pfx)])
                                           (and (<= plen
                                                    (string-length str))
                                                (string=?
                                                  pfx
                                                  (substring
                                                    str
                                                    0
                                                    plen)))))
                                       (let ([pfx "bad substitution: unclosed"]
                                             [str msg])
                                         (let ([plen (string-length pfx)])
                                           (and (<= plen
                                                    (string-length str))
                                                (string=?
                                                  pfx
                                                  (substring
                                                    str
                                                    0
                                                    plen)))))))
                              2
                              1))]))
                    __exn)])
          (let ([cmd (guard (__exn
                              [#t
                               ((lambda (e)
                                  (fprintf
                                    (current-error-port)
                                    "jsh: syntax error: ~a~n"
                                    (exception-message e))
                                  'error)
                                 __exn)])
                       (let ([alias-fn (and (env-shopt?
                                              env
                                              "expand_aliases")
                                            (lambda (word)
                                              (alias-get env word)))])
                         (parse-complete-command
                           input
                           (env-shopt? env "extglob")
                           alias-fn)))])
            (cond
              [(eq? cmd 'error) 2]
              [(not cmd) 0]
              [else (execute-command cmd env)])))))
  (define (process-traps! env)
    (let ([signals (pending-signals!)])
      (for-each
        (lambda (sig-name)
          (cond
            [(string=? sig-name "CHLD")
             (job-update-status!)
             (job-notify!)]
            [(string=? sig-name "WINCH") (%%void)]
            [else (%%void)])
          (let ([action (trap-get sig-name)])
            (when (and action (string? action))
              (let ([saved-status (shell-environment-last-status env)])
                (execute-input action env)
                (env-set-last-status! env saved-status)))))
        signals)))
  (define (run-exit-trap! env)
    (let ([action (trap-get "EXIT")])
      (when (and action (string? action))
        (trap-set! "EXIT" 'default)
        (execute-input action env))))
  (define (move-internal-fds-high!) (ffi-move-gambit-fds 255))
  (define *reserved-user-fds* (list))
  (define (reserve-user-fds!)
    (let loop ([fds (list)])
      (let ([fd (ffi-open-raw "/dev/null" 0 0)])
        (if (<= fd 9)
            (loop (cons fd fds))
            (begin (ffi-close-fd fd) (set! *reserved-user-fds* fds))))))
  (define (release-user-fds!)
    (for-each ffi-close-fd *reserved-user-fds*)
    (set! *reserved-user-fds* (list)))
  (define (init-smp!)
    (let* ([env-val (getenv "GSH_PROCESSORS" #f)])
      (let* ([n (cond
                  [(not env-val) #f]
                  [(string=? env-val "auto") (\x23;\x23;cpu-count)]
                  [else (string->number env-val)])])
        (when (and n (> n 1))
          (\x23;\x23;set-parallelism-level! n)
          (\x23;\x23;startup-parallelism!)))))
  (define (main . args)
    (init-smp!)
    (move-internal-fds-high!)
    (reserve-user-fds!)
    (*gambit-scheduler-rfd* (ffi-gambit-scheduler-rfd))
    (*gambit-scheduler-wfd* (ffi-gambit-scheduler-wfd))
    (let* ([args-hash (parse-args args)])
      (let* ([command (hash-ref args-hash 'command)])
        (let* ([script (hash-ref args-hash 'script)])
          (let* ([login? (hash-ref args-hash 'login?)])
            (let* ([env (init-shell-env args-hash)])
              (*current-jsh-env* env)
              (cond
                [command
                 (let ([script-args (hash-ref args-hash 'args)])
                   (when (pair? script-args)
                     (env-set-shell-name! env (car script-args))
                     (env-set-positional! env (cdr script-args)))
                   (setup-noninteractive-signal-handlers!)
                   (release-user-fds!)
                   (load-startup-files! env login? #f)
                   (let* ([interactive? (hash-ref
                                          args-hash
                                          'interactive?)])
                     (let* ([status (guard (__exn
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
                                                   [(nounset-exception? e)
                                                    (nounset-exception-status
                                                      e)]
                                                   [(break-exception? e) 0]
                                                   [(continue-exception? e)
                                                    0]
                                                   [(return-exception? e)
                                                    (return-exception-status
                                                      e)]
                                                   [else
                                                    (let ([msg (exception-message
                                                                 e)])
                                                      (fprintf
                                                        (current-error-port)
                                                        "jsh: ~a~n"
                                                        msg)
                                                      (if (and (string?
                                                                 msg)
                                                               (or (let ([pfx "parse error"]
                                                                         [str msg])
                                                                     (let ([plen (string-length
                                                                                   pfx)])
                                                                       (and (<= plen
                                                                                (string-length
                                                                                  str))
                                                                            (string=?
                                                                              pfx
                                                                              (substring
                                                                                str
                                                                                0
                                                                                plen)))))
                                                                   (let ([pfx "bad substitution: unclosed"]
                                                                         [str msg])
                                                                     (let ([plen (string-length
                                                                                   pfx)])
                                                                       (and (<= plen
                                                                                (string-length
                                                                                  str))
                                                                            (string=?
                                                                              pfx
                                                                              (substring
                                                                                str
                                                                                0
                                                                                plen)))))))
                                                          2
                                                          1))]))
                                                __exn)])
                                      (execute-string
                                        command
                                        env
                                        interactive?))])
                       (process-traps! env)
                       (run-exit-trap! env)
                       (exit status))))]
                [script
                 (let ([script-args (hash-ref args-hash 'args)])
                   (setup-noninteractive-signal-handlers!)
                   (release-user-fds!)
                   (load-startup-files! env login? #f)
                   (let ([status (execute-script script script-args env)])
                     (run-exit-trap! env)
                     (exit status)))]
                [else
                 (let ([interactive? (or (hash-ref args-hash 'interactive?)
                                         (guard (__exn
                                                  [#t
                                                   ((lambda (e) #f)
                                                     __exn)])
                                           (= (ffi-isatty 0) 1)))])
                   (when interactive?
                     (hash-put! args-hash 'interactive? #t)
                     (env-shopt-set! env "expand_aliases" #t))
                   (load-startup-files! env login? interactive?)
                   (if interactive?
                       (begin
                         (*interactive-shell* #t)
                         (fzf-init!)
                         (repl env)
                         (when login? (run-logout! env))
                         (history-save!)
                         (exit 0))
                       (let ([status (execute-stdin env)])
                         (exit status))))])))))))
  (define (execute-stdin env)
    (let loop ([status 0])
      (let ([line (get-line (current-input-port))])
        (if (eof-object? line)
            (begin (run-exit-trap! env) status)
            (let ([new-status (guard (__exn
                                       [#t
                                        ((lambda (e)
                                           (cond
                                             [(nounset-exception? e)
                                              (nounset-exception-status e)]
                                             [(break-exception? e) 0]
                                             [(continue-exception? e) 0]
                                             [(return-exception? e)
                                              (return-exception-status e)]
                                             [else (raise e)]))
                                          __exn)])
                                (execute-input line env))])
              (env-set-last-status! env new-status)
              (process-traps! env)
              (when (and (not (= new-status 0)) (trap-get "ERR"))
                (let ([action (trap-get "ERR")])
                  (when (string? action) (execute-input action env))))
              (if (and (not (= new-status 0)) (env-option? env "errexit"))
                  (begin (run-exit-trap! env) new-status)
                  (loop new-status))))))))
