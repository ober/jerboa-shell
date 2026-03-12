#!chezscheme
(library (jsh control)
  (export with-loop-context execute-if execute-for execute-while
    execute-until execute-case execute-select
    any-pattern-matches? valid-identifier? string-trim-ws)
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
    (std sugar) (jsh ast) (jsh environment) (jsh expander)
    (jsh glob) (jsh functions))
  (define (with-loop-context thunk)
    (parameterize ([*loop-depth* (+ (*loop-depth*) 1)])
      (thunk)))
  (define (execute-if cmd env execute-fn)
    (let loop ([clauses (if-command-clauses cmd)])
      (if (null? clauses)
          (if (if-command-else-part cmd)
              (execute-fn (if-command-else-part cmd) env)
              0)
          (let* ([clause (car clauses)])
            (let* ([condition (car clause)])
              (let* ([body (cdr clause)])
                (let* ([test-status (parameterize ([*in-condition-context*
                                                    #t])
                                      (execute-fn condition env))])
                  (if (= test-status 0)
                      (execute-fn body env)
                      (loop (cdr clauses))))))))))
  (define (execute-for cmd env execute-fn)
    (let* ([var-name (for-command-var cmd)])
      (if (not (valid-identifier? var-name))
          (begin
            (fprintf
              (current-error-port)
              "jsh: `~a': not a valid identifier~n"
              var-name)
            2)
          (let* ([word-list (for-command-words cmd)])
            (let* ([items (if word-list
                              (expand-words word-list env)
                              (env-at env))])
              (with-loop-context
                (lambda ()
                  (let ([remaining items] [status 0])
                    (let loop ()
                      (if (null? remaining)
                          status
                          (begin
                            (thread-yield!)
                            (let ([trap-fn (*process-traps-fn*)])
                              (when trap-fn (trap-fn env)))
                            (env-set! env var-name (car remaining))
                            (set! remaining (cdr remaining))
                            (let ([caught (guard (__exn
                                                   [#t
                                                    ((lambda (e)
                                                       (cond
                                                         [(break-exception?
                                                            e)
                                                          (cons 'break e)]
                                                         [(continue-exception?
                                                            e)
                                                          (cons
                                                            'continue
                                                            e)]
                                                         [else (raise e)]))
                                                      __exn)])
                                            (set! status
                                              (execute-fn
                                                (for-command-body cmd)
                                                env))
                                            #f)])
                              (cond
                                [(not caught) (loop)]
                                [(eq? (car caught) 'break)
                                 (let ([levels (break-exception-levels
                                                 (cdr caught))])
                                   (if (> levels 1)
                                       (raise
                                         (make-break-exception
                                           (- levels 1)))
                                       (shell-environment-last-status
                                         env)))]
                                [(eq? (car caught) 'continue)
                                 (let ([levels (continue-exception-levels
                                                 (cdr caught))])
                                   (if (> levels 1)
                                       (raise
                                         (make-continue-exception
                                           (- levels 1)))
                                       (loop)))])))))))))))))
  (define (execute-while cmd env execute-fn)
    (with-loop-context
      (lambda ()
        (let ([status 0])
          (let loop ()
            (thread-yield!)
            (let ([trap-fn (*process-traps-fn*)])
              (when trap-fn (trap-fn env)))
            (let ([test-caught (guard (__exn
                                        [#t
                                         ((lambda (e)
                                            (cond
                                              [(break-exception? e)
                                               (cons 'break e)]
                                              [(continue-exception? e)
                                               (cons 'continue e)]
                                              [else (raise e)]))
                                           __exn)])
                                 (let ([ts (parameterize ([*in-condition-context*
                                                           #t])
                                             (execute-fn
                                               (while-command-test cmd)
                                               env))])
                                   (cons 'test ts)))])
              (cond
                [(eq? (car test-caught) 'break)
                 (let ([levels (break-exception-levels (cdr test-caught))])
                   (if (> levels 1)
                       (raise (make-break-exception (- levels 1)))
                       status))]
                [(eq? (car test-caught) 'continue)
                 (let ([levels (continue-exception-levels
                                 (cdr test-caught))])
                   (if (> levels 1)
                       (raise (make-continue-exception (- levels 1)))
                       (loop)))]
                [else
                 (let ([test-status (cdr test-caught)])
                   (if (= test-status 0)
                       (let ([body-caught (guard (__exn
                                                   [#t
                                                    ((lambda (e)
                                                       (cond
                                                         [(break-exception?
                                                            e)
                                                          (cons 'break e)]
                                                         [(continue-exception?
                                                            e)
                                                          (cons
                                                            'continue
                                                            e)]
                                                         [else (raise e)]))
                                                      __exn)])
                                            (set! status
                                              (execute-fn
                                                (while-command-body cmd)
                                                env))
                                            #f)])
                         (cond
                           [(not body-caught) (loop)]
                           [(eq? (car body-caught) 'break)
                            (let ([levels (break-exception-levels
                                            (cdr body-caught))])
                              (if (> levels 1)
                                  (raise
                                    (make-break-exception (- levels 1)))
                                  (shell-environment-last-status env)))]
                           [(eq? (car body-caught) 'continue)
                            (let ([levels (continue-exception-levels
                                            (cdr body-caught))])
                              (if (> levels 1)
                                  (raise
                                    (make-continue-exception (- levels 1)))
                                  (loop)))]))
                       status))])))))))
  (define (execute-until cmd env execute-fn)
    (with-loop-context
      (lambda ()
        (let ([status 0])
          (let loop ()
            (thread-yield!)
            (let ([trap-fn (*process-traps-fn*)])
              (when trap-fn (trap-fn env)))
            (let ([test-status (parameterize ([*in-condition-context*
                                               #t])
                                 (execute-fn
                                   (until-command-test cmd)
                                   env))])
              (if (not (= test-status 0))
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
                                    (execute-fn
                                      (until-command-body cmd)
                                      env))
                                  #f)])
                    (cond
                      [(not caught) (loop)]
                      [(eq? (car caught) 'break)
                       (let ([levels (break-exception-levels
                                       (cdr caught))])
                         (if (> levels 1)
                             (raise (make-break-exception (- levels 1)))
                             (shell-environment-last-status env)))]
                      [(eq? (car caught) 'continue)
                       (let ([levels (continue-exception-levels
                                       (cdr caught))])
                         (if (> levels 1)
                             (raise (make-continue-exception (- levels 1)))
                             (loop)))]))
                  status)))))))
  (define (execute-case cmd env execute-fn)
    (let* ([word (expand-word-nosplit
                   (case-command-word cmd)
                   env)])
      (let* ([clauses (case-command-clauses cmd)])
        (let loop ([clauses clauses])
          (if (null? clauses)
              0
              (let* ([clause (car clauses)])
                (let* ([patterns (case-clause-patterns clause)])
                  (let* ([body (case-clause-body clause)])
                    (let* ([terminator (case-clause-terminator clause)])
                      (if (any-pattern-matches? patterns word env)
                          (let ([status (if body (execute-fn body env) 0)])
                            (case terminator
                              [(break) status]
                              [(fallthrough)
                               (let fall ([rest (cdr clauses)]
                                          [last-status status])
                                 (if (null? rest)
                                     last-status
                                     (let* ([next-clause (car rest)])
                                       (let* ([next-body (case-clause-body
                                                           next-clause)])
                                         (let* ([next-term (case-clause-terminator
                                                             next-clause)])
                                           (let* ([next-status (if next-body
                                                                   (execute-fn
                                                                     next-body
                                                                     env)
                                                                   last-status)])
                                             (if (eq? next-term
                                                      'fallthrough)
                                                 (fall
                                                   (cdr rest)
                                                   next-status)
                                                 next-status)))))))]
                              [(test-next)
                               (let ([rest-status (loop (cdr clauses))])
                                 (if (= rest-status 0)
                                     status
                                     rest-status))]
                              [else status]))
                          (loop (cdr clauses))))))))))))
  (define (execute-select cmd env execute-fn)
    (let* ([var-name (select-command-var cmd)])
      (let* ([word-list (select-command-words cmd)])
        (let* ([items (expand-words word-list env)])
          (let* ([ps3 (or (env-get env "PS3") "#? ")])
            (let menu-loop ([status 0])
              (let ([i 1])
                (for-each
                  (lambda (item)
                    (fprintf (current-error-port) "~a) ~a~n" i item)
                    (set! i (+ i 1)))
                  items))
              (display ps3 (current-error-port))
              (flush-output-port (current-error-port))
              (let ([line (get-line (current-input-port))])
                (if (eof-object? line)
                    status
                    (let ([n (string->number (string-trim-ws line))])
                      (if (and n (> n 0) (<= n (length items)))
                          (begin
                            (env-set!
                              env
                              var-name
                              (list-ref items (- n 1)))
                            (env-set! env "REPLY" line)
                            (guard (__exn
                                     [#t
                                      ((lambda (e)
                                         (cond
                                           [(break-exception? e) status]
                                           [else (raise e)]))
                                        __exn)])
                              (let ([new-status (execute-fn
                                                  (select-command-body cmd)
                                                  env)])
                                (menu-loop new-status))))
                          (begin
                            (env-set! env var-name "")
                            (env-set! env "REPLY" line)
                            (let ([new-status (execute-fn
                                                (select-command-body cmd)
                                                env)])
                              (menu-loop new-status)))))))))))))
  (define (any-pattern-matches? patterns word env)
    (let loop ([pats patterns])
      (if (null? pats)
          #f
          (let ([pat (expand-word-as-pattern (car pats) env)])
            (if (or (string=? pat "*")
                    (glob-match? pat word #f (env-shopt? env "extglob")))
                #t
                (loop (cdr pats)))))))
  (define (valid-identifier? name)
    (and (string? name)
         (> (string-length name) 0)
         (let ([ch0 (string-ref name 0)])
           (or (char-alphabetic? ch0) (char=? ch0 #\_)))
         (let loop ([i 1])
           (if (>= i (string-length name))
               #t
               (let ([ch (string-ref name i)])
                 (if (or (char-alphabetic? ch)
                         (char-numeric? ch)
                         (char=? ch #\_))
                     (loop (+ i 1))
                     #f))))))
  (define (string-trim-ws str)
    (let* ([len (string-length str)])
      (let* ([start (let loop ([i 0])
                      (if (and (< i len)
                               (char-whitespace? (string-ref str i)))
                          (loop (+ i 1))
                          i))])
        (let* ([end (let loop ([i (- len 1)])
                      (if (and (>= i start)
                               (char-whitespace? (string-ref str i)))
                          (loop (- i 1))
                          (+ i 1)))])
          (substring str start end))))))
