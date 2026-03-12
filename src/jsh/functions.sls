#!chezscheme
(library (jsh functions)
  (export shell-function::t make-shell-function shell-function?
   shell-function-name shell-function-body
   shell-function-redirections shell-function-lineno
   shell-function-source-file shell-function-name-set!
   shell-function-body-set! shell-function-redirections-set!
   shell-function-lineno-set! shell-function-source-file-set!
   &shell-function-name &shell-function-body
   &shell-function-redirections &shell-function-lineno
   &shell-function-source-file &shell-function-name-set!
   &shell-function-body-set! &shell-function-redirections-set!
   &shell-function-lineno-set! &shell-function-source-file-set!
   function-define! function-lookup function-unset!
   function-list function-call cleanup-exported-locals!
   return-exception::t make-return-exception return-exception?
   return-exception-status return-exception-status-set!
   &return-exception-status &return-exception-status-set!
   shell-return! break-exception::t make-break-exception
   break-exception? break-exception-levels
   break-exception-levels-set! &break-exception-levels
   &break-exception-levels-set! continue-exception::t
   make-continue-exception continue-exception?
   continue-exception-levels continue-exception-levels-set!
   &continue-exception-levels &continue-exception-levels-set!
   *loop-depth* shell-break! shell-continue!
   errexit-exception::t make-errexit-exception
   errexit-exception? errexit-exception-status
   errexit-exception-status-set! &errexit-exception-status
   &errexit-exception-status-set! nounset-exception::t
   make-nounset-exception nounset-exception?
   nounset-exception-status nounset-exception-status-set!
   &nounset-exception-status &nounset-exception-status-set!
   subshell-exit-exception::t make-subshell-exit-exception
   subshell-exit-exception? subshell-exit-exception-status
   subshell-exit-exception-status-set!
   &subshell-exit-exception-status
   &subshell-exit-exception-status-set! alias-set! alias-get
   alias-unset! alias-clear! alias-list alias-expand
   alias-continues?)
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
    (std sugar) (gsh ast) (gsh environment) (gsh ffi))
  (begin
    (define shell-function::t
      (make-class-type 'gerbil\x23;shell-function::t 'shell-function
        (list object::t)
        '(name body redirections lineno source-file)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-shell-function . args)
      (let* ([type shell-function::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (shell-function? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;shell-function::t))
    (define (shell-function-name obj)
      (unchecked-slot-ref obj 'name))
    (define (shell-function-body obj)
      (unchecked-slot-ref obj 'body))
    (define (shell-function-redirections obj)
      (unchecked-slot-ref obj 'redirections))
    (define (shell-function-lineno obj)
      (unchecked-slot-ref obj 'lineno))
    (define (shell-function-source-file obj)
      (unchecked-slot-ref obj 'source-file))
    (define (shell-function-name-set! obj val)
      (unchecked-slot-set! obj 'name val))
    (define (shell-function-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (shell-function-redirections-set! obj val)
      (unchecked-slot-set! obj 'redirections val))
    (define (shell-function-lineno-set! obj val)
      (unchecked-slot-set! obj 'lineno val))
    (define (shell-function-source-file-set! obj val)
      (unchecked-slot-set! obj 'source-file val))
    (define (&shell-function-name obj)
      (unchecked-slot-ref obj 'name))
    (define (&shell-function-body obj)
      (unchecked-slot-ref obj 'body))
    (define (&shell-function-redirections obj)
      (unchecked-slot-ref obj 'redirections))
    (define (&shell-function-lineno obj)
      (unchecked-slot-ref obj 'lineno))
    (define (&shell-function-source-file obj)
      (unchecked-slot-ref obj 'source-file))
    (define (&shell-function-name-set! obj val)
      (unchecked-slot-set! obj 'name val))
    (define (&shell-function-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (&shell-function-redirections-set! obj val)
      (unchecked-slot-set! obj 'redirections val))
    (define (&shell-function-lineno-set! obj val)
      (unchecked-slot-set! obj 'lineno val))
    (define (&shell-function-source-file-set! obj val)
      (unchecked-slot-set! obj 'source-file val)))
  (define function-define!
    (case-lambda
      [(env name body)
       (let* ([redirections (list)] [lineno #f] [source-file #f])
         (hash-put!
           (shell-environment-functions env)
           name
           (make-shell-function name body redirections lineno
             source-file)))]
      [(env name body redirections)
       (let* ([lineno #f] [source-file #f])
         (hash-put!
           (shell-environment-functions env)
           name
           (make-shell-function name body redirections lineno
             source-file)))]
      [(env name body redirections lineno)
       (let* ([source-file #f])
         (hash-put!
           (shell-environment-functions env)
           name
           (make-shell-function name body redirections lineno
             source-file)))]
      [(env name body redirections lineno source-file)
       (hash-put!
         (shell-environment-functions env)
         name
         (make-shell-function name body redirections lineno
           source-file))]))
  (define (function-lookup env name)
    (hash-get (shell-environment-functions env) name))
  (define (function-unset! env name)
    (hash-remove! (shell-environment-functions env) name))
  (define (function-list env)
    (hash-keys (shell-environment-functions env)))
  (define (function-call func args env execute-fn)
    (let* ([child-env (env-push-scope env)])
      (let* ([_ (env-set-positional! child-env args)])
        (let* ([_ (env-set!
                    child-env
                    "FUNCNAME"
                    (shell-function-name func))])
          (let ([status (guard (__exn
                                 [#t
                                  ((lambda (e)
                                     (if (return-exception? e)
                                         (return-exception-status e)
                                         (raise e)))
                                    __exn)])
                          (let ([s (execute-fn
                                     (shell-function-body func)
                                     child-env)])
                            (env-set-last-status! env s)
                            s))])
            (cleanup-exported-locals! child-env env)
            status)))))
  (define (cleanup-exported-locals! child-env parent-env)
    (for-each
      (lambda (pair)
        (let ([name (car pair)] [var (cdr pair)])
          (when (and (shell-var-local? var) (shell-var-exported? var))
            (let ([parent-var (env-get-raw-var parent-env name)])
              (if (and parent-var (shell-var-exported? parent-var))
                  (let ([v (shell-var-scalar-value parent-var)])
                    (if v (setenv name v) (ffi-unsetenv name)))
                  (ffi-unsetenv name))))))
      (hash->list (shell-environment-vars child-env))))
  (begin
    (define return-exception::t
      (make-class-type 'gerbil\x23;return-exception::t 'return-exception
        (list object::t) '(status)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-return-exception . args)
      (let* ([type return-exception::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (return-exception? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;return-exception::t))
    (define (return-exception-status obj)
      (unchecked-slot-ref obj 'status))
    (define (return-exception-status-set! obj val)
      (unchecked-slot-set! obj 'status val))
    (define (&return-exception-status obj)
      (unchecked-slot-ref obj 'status))
    (define (&return-exception-status-set! obj val)
      (unchecked-slot-set! obj 'status val)))
  (define shell-return!
    (case-lambda
      [()
       (let* ([status 0]) (raise (make-return-exception status)))]
      [(status) (raise (make-return-exception status))]))
  (begin
    (define break-exception::t
      (make-class-type 'gerbil\x23;break-exception::t 'break-exception
        (list object::t) '(levels)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-break-exception . args)
      (let* ([type break-exception::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (break-exception? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;break-exception::t))
    (define (break-exception-levels obj)
      (unchecked-slot-ref obj 'levels))
    (define (break-exception-levels-set! obj val)
      (unchecked-slot-set! obj 'levels val))
    (define (&break-exception-levels obj)
      (unchecked-slot-ref obj 'levels))
    (define (&break-exception-levels-set! obj val)
      (unchecked-slot-set! obj 'levels val)))
  (begin
    (define continue-exception::t
      (make-class-type 'gerbil\x23;continue-exception::t 'continue-exception
        (list object::t) '(levels)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-continue-exception . args)
      (let* ([type continue-exception::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (continue-exception? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;continue-exception::t))
    (define (continue-exception-levels obj)
      (unchecked-slot-ref obj 'levels))
    (define (continue-exception-levels-set! obj val)
      (unchecked-slot-set! obj 'levels val))
    (define (&continue-exception-levels obj)
      (unchecked-slot-ref obj 'levels))
    (define (&continue-exception-levels-set! obj val)
      (unchecked-slot-set! obj 'levels val)))
  (define *loop-depth* (make-parameter 0))
  (define shell-break!
    (case-lambda
      [()
       (let* ([levels 1])
         (if (> (*loop-depth*) 0)
             (raise (make-break-exception levels))
             (begin
               (fprintf
                 (current-error-port)
                 "break: only meaningful in a `for', `while', or `until' loop~n")
               0)))]
      [(levels)
       (if (> (*loop-depth*) 0)
           (raise (make-break-exception levels))
           (begin
             (fprintf
               (current-error-port)
               "break: only meaningful in a `for', `while', or `until' loop~n")
             0))]))
  (define shell-continue!
    (case-lambda
      [()
       (let* ([levels 1])
         (if (> (*loop-depth*) 0)
             (raise (make-continue-exception levels))
             (begin
               (fprintf
                 (current-error-port)
                 "continue: only meaningful in a `for', `while', or `until' loop~n")
               0)))]
      [(levels)
       (if (> (*loop-depth*) 0)
           (raise (make-continue-exception levels))
           (begin
             (fprintf
               (current-error-port)
               "continue: only meaningful in a `for', `while', or `until' loop~n")
             0))]))
  (begin
    (define errexit-exception::t
      (make-class-type 'gerbil\x23;errexit-exception::t 'errexit-exception
        (list object::t) '(status)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-errexit-exception . args)
      (let* ([type errexit-exception::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (errexit-exception? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;errexit-exception::t))
    (define (errexit-exception-status obj)
      (unchecked-slot-ref obj 'status))
    (define (errexit-exception-status-set! obj val)
      (unchecked-slot-set! obj 'status val))
    (define (&errexit-exception-status obj)
      (unchecked-slot-ref obj 'status))
    (define (&errexit-exception-status-set! obj val)
      (unchecked-slot-set! obj 'status val)))
  (begin
    (define nounset-exception::t
      (make-class-type 'gerbil\x23;nounset-exception::t 'nounset-exception
        (list object::t) '(status)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-nounset-exception . args)
      (let* ([type nounset-exception::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (nounset-exception? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;nounset-exception::t))
    (define (nounset-exception-status obj)
      (unchecked-slot-ref obj 'status))
    (define (nounset-exception-status-set! obj val)
      (unchecked-slot-set! obj 'status val))
    (define (&nounset-exception-status obj)
      (unchecked-slot-ref obj 'status))
    (define (&nounset-exception-status-set! obj val)
      (unchecked-slot-set! obj 'status val)))
  (begin
    (define subshell-exit-exception::t
      (make-class-type 'gerbil\x23;subshell-exit-exception::t
        'subshell-exit-exception (list object::t) '(status)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-subshell-exit-exception . args)
      (let* ([type subshell-exit-exception::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (subshell-exit-exception? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;subshell-exit-exception::t))
    (define (subshell-exit-exception-status obj)
      (unchecked-slot-ref obj 'status))
    (define (subshell-exit-exception-status-set! obj val)
      (unchecked-slot-set! obj 'status val))
    (define (&subshell-exit-exception-status obj)
      (unchecked-slot-ref obj 'status))
    (define (&subshell-exit-exception-status-set! obj val)
      (unchecked-slot-set! obj 'status val)))
  (define (alias-set! env name value)
    (hash-put! (shell-environment-aliases env) name value))
  (define (alias-get env name)
    (hash-get (shell-environment-aliases env) name))
  (define (alias-unset! env name)
    (hash-remove! (shell-environment-aliases env) name))
  (define (alias-clear! env)
    (for-each
      (lambda (pair)
        (let ([k (car pair)] [v (cdr pair)])
          (hash-remove! (shell-environment-aliases env) k)))
      (hash->list (shell-environment-aliases env))))
  (define (alias-list env)
    (hash->list (shell-environment-aliases env)))
  (define (alias-expand env word)
    (let ([val (alias-get env word)]) (if val val #f)))
  (define (alias-continues? value)
    (and (> (string-length value) 0)
         (char=?
           (string-ref value (- (string-length value) 1))
           #\space))))
