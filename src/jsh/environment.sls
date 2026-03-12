#!chezscheme
(library (jsh environment)
  (export *execute-input* *arith-eval-fn* *interactive-shell*
   *process-traps-fn* *current-source-file*
   *in-condition-context* *in-subshell* *pipeline-stdin-fd*
   *pipeline-stdout-fd* *gambit-scheduler-rfd*
   *gambit-scheduler-wfd* *active-redirect-fds* *internal-pwd*
   shell-var::t make-shell-var shell-var? shell-var-value
   shell-var-exported? shell-var-readonly? shell-var-local?
   shell-var-integer? shell-var-uppercase? shell-var-lowercase?
   shell-var-nameref? shell-var-array? shell-var-assoc?
   shell-var-value-set! shell-var-exported?-set!
   shell-var-readonly?-set! shell-var-local?-set!
   shell-var-integer?-set! shell-var-uppercase?-set!
   shell-var-lowercase?-set! shell-var-nameref?-set!
   shell-var-array?-set! shell-var-assoc?-set! &shell-var-value
   &shell-var-exported? &shell-var-readonly? &shell-var-local?
   &shell-var-integer? &shell-var-uppercase?
   &shell-var-lowercase? &shell-var-nameref? &shell-var-array?
   &shell-var-assoc? &shell-var-value-set!
   &shell-var-exported?-set! &shell-var-readonly?-set!
   &shell-var-local?-set! &shell-var-integer?-set!
   &shell-var-uppercase?-set! &shell-var-lowercase?-set!
   &shell-var-nameref?-set! &shell-var-array?-set!
   &shell-var-assoc?-set! \x2B;unset-sentinel+
   shell-environment::t shell-environment shell-environment?
   make-shell-environment &shell-environment-vars-set!
   &shell-environment-vars shell-environment-vars-set!
   shell-environment-vars &shell-environment-parent-set!
   &shell-environment-parent shell-environment-parent-set!
   shell-environment-parent &shell-environment-functions-set!
   &shell-environment-functions
   shell-environment-functions-set! shell-environment-functions
   &shell-environment-aliases-set! &shell-environment-aliases
   shell-environment-aliases-set! shell-environment-aliases
   &shell-environment-options-set! &shell-environment-options
   shell-environment-options-set! shell-environment-options
   &shell-environment-shopts-set! &shell-environment-shopts
   shell-environment-shopts-set! shell-environment-shopts
   &shell-environment-positional-set!
   &shell-environment-positional
   shell-environment-positional-set!
   shell-environment-positional
   &shell-environment-last-status-set!
   &shell-environment-last-status
   shell-environment-last-status-set!
   shell-environment-last-status
   &shell-environment-last-bg-pid-set!
   &shell-environment-last-bg-pid
   shell-environment-last-bg-pid-set!
   shell-environment-last-bg-pid
   &shell-environment-shell-pid-set!
   &shell-environment-shell-pid
   shell-environment-shell-pid-set! shell-environment-shell-pid
   &shell-environment-shell-name-set!
   &shell-environment-shell-name
   shell-environment-shell-name-set!
   shell-environment-shell-name
   &shell-environment-start-time-set!
   &shell-environment-start-time
   shell-environment-start-time-set!
   shell-environment-start-time
   &shell-environment-cmd-number-set!
   &shell-environment-cmd-number
   shell-environment-cmd-number-set!
   shell-environment-cmd-number &shell-environment-traps-set!
   &shell-environment-traps shell-environment-traps-set!
   shell-environment-traps &shell-environment-dir-stack-set!
   &shell-environment-dir-stack
   shell-environment-dir-stack-set! shell-environment-dir-stack
   resolve-nameref env-get env-get-local env-get-chain
   shell-var-scalar-value apply-var-attrs env-get-var
   env-get-raw-var env-matching-names env-set!
   env-set-shell-name! env-export! env-unset!
   env-unset-nameref! env-readonly! env-exported-alist
   env-collect-exports! env-push-scope env-clone
   clone-var-table clone-vars-from-chain! env-pop-scope
   env-set-positional! env-positional-list env-star env-at
   env-option-set! env-option? env-all-options
   env-all-variables env-shopt-set! env-shopt?
   env-set-last-status! env-set-last-bg-pid!
   env-inc-cmd-number! env-init! set-color-vars!
   positional-index find-var-in-chain
   find-var-in-chain-for-write env-root find-var-owner-in-chain
   options->flag-string string-join-with env-array-get
   env-array-element-set? env-array-set! env-array-values
   env-array-keys env-array-length env-array-unset-element!
   env-array-set-compound! env-array-append-compound!
   array-kv-pair? parse-array-kv string-find-char-in
   env-is-array? parse-arith-subscript arith-env-getter
   arith-env-setter shell-environment:::init!)
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
    (std sugar) (gsh ffi))
  (define *execute-input* (make-parameter #f))
  (define *arith-eval-fn* (make-parameter #f))
  (define *interactive-shell* (make-parameter #f))
  (define *process-traps-fn* (make-parameter #f))
  (define *current-source-file* (make-parameter "main"))
  (define *in-condition-context* (make-parameter #f))
  (define *in-subshell* (make-parameter #f))
  (define *pipeline-stdin-fd* (make-parameter #f))
  (define *pipeline-stdout-fd* (make-parameter #f))
  (define *gambit-scheduler-rfd* (make-parameter -1))
  (define *gambit-scheduler-wfd* (make-parameter -1))
  (define *active-redirect-fds* (make-parameter (list)))
  (define *internal-pwd* (make-parameter #f))
  (begin
    (define shell-var::t
      (make-class-type 'gerbil\x23;shell-var::t 'shell-var (list object::t)
        '(value exported? readonly? local? integer? uppercase?
           lowercase? nameref? array? assoc?)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-shell-var . args)
      (let* ([type shell-var::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (shell-var? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;shell-var::t))
    (define (shell-var-value obj)
      (unchecked-slot-ref obj 'value))
    (define (shell-var-exported? obj)
      (unchecked-slot-ref obj 'exported?))
    (define (shell-var-readonly? obj)
      (unchecked-slot-ref obj 'readonly?))
    (define (shell-var-local? obj)
      (unchecked-slot-ref obj 'local?))
    (define (shell-var-integer? obj)
      (unchecked-slot-ref obj 'integer?))
    (define (shell-var-uppercase? obj)
      (unchecked-slot-ref obj 'uppercase?))
    (define (shell-var-lowercase? obj)
      (unchecked-slot-ref obj 'lowercase?))
    (define (shell-var-nameref? obj)
      (unchecked-slot-ref obj 'nameref?))
    (define (shell-var-array? obj)
      (unchecked-slot-ref obj 'array?))
    (define (shell-var-assoc? obj)
      (unchecked-slot-ref obj 'assoc?))
    (define (shell-var-value-set! obj val)
      (unchecked-slot-set! obj 'value val))
    (define (shell-var-exported?-set! obj val)
      (unchecked-slot-set! obj 'exported? val))
    (define (shell-var-readonly?-set! obj val)
      (unchecked-slot-set! obj 'readonly? val))
    (define (shell-var-local?-set! obj val)
      (unchecked-slot-set! obj 'local? val))
    (define (shell-var-integer?-set! obj val)
      (unchecked-slot-set! obj 'integer? val))
    (define (shell-var-uppercase?-set! obj val)
      (unchecked-slot-set! obj 'uppercase? val))
    (define (shell-var-lowercase?-set! obj val)
      (unchecked-slot-set! obj 'lowercase? val))
    (define (shell-var-nameref?-set! obj val)
      (unchecked-slot-set! obj 'nameref? val))
    (define (shell-var-array?-set! obj val)
      (unchecked-slot-set! obj 'array? val))
    (define (shell-var-assoc?-set! obj val)
      (unchecked-slot-set! obj 'assoc? val))
    (define (&shell-var-value obj)
      (unchecked-slot-ref obj 'value))
    (define (&shell-var-exported? obj)
      (unchecked-slot-ref obj 'exported?))
    (define (&shell-var-readonly? obj)
      (unchecked-slot-ref obj 'readonly?))
    (define (&shell-var-local? obj)
      (unchecked-slot-ref obj 'local?))
    (define (&shell-var-integer? obj)
      (unchecked-slot-ref obj 'integer?))
    (define (&shell-var-uppercase? obj)
      (unchecked-slot-ref obj 'uppercase?))
    (define (&shell-var-lowercase? obj)
      (unchecked-slot-ref obj 'lowercase?))
    (define (&shell-var-nameref? obj)
      (unchecked-slot-ref obj 'nameref?))
    (define (&shell-var-array? obj)
      (unchecked-slot-ref obj 'array?))
    (define (&shell-var-assoc? obj)
      (unchecked-slot-ref obj 'assoc?))
    (define (&shell-var-value-set! obj val)
      (unchecked-slot-set! obj 'value val))
    (define (&shell-var-exported?-set! obj val)
      (unchecked-slot-set! obj 'exported? val))
    (define (&shell-var-readonly?-set! obj val)
      (unchecked-slot-set! obj 'readonly? val))
    (define (&shell-var-local?-set! obj val)
      (unchecked-slot-set! obj 'local? val))
    (define (&shell-var-integer?-set! obj val)
      (unchecked-slot-set! obj 'integer? val))
    (define (&shell-var-uppercase?-set! obj val)
      (unchecked-slot-set! obj 'uppercase? val))
    (define (&shell-var-lowercase?-set! obj val)
      (unchecked-slot-set! obj 'lowercase? val))
    (define (&shell-var-nameref?-set! obj val)
      (unchecked-slot-set! obj 'nameref? val))
    (define (&shell-var-array?-set! obj val)
      (unchecked-slot-set! obj 'array? val))
    (define (&shell-var-assoc?-set! obj val)
      (unchecked-slot-set! obj 'assoc? val)))
  (define \x2B;unset-sentinel+ (gensym "unset"))
  (begin
    (define shell-environment::t
      (make-class-type 'gerbil\x23;shell-environment::t 'shell-environment
        (list object::t)
        '(vars parent functions aliases options shopts positional
           last-status last-bg-pid shell-pid shell-name start-time
           cmd-number traps dir-stack)
        '((constructor: . :init!) (transparent: . #t)) ':init!))
    (define (shell-environment . args)
      (apply make-shell-environment args))
    (define (shell-environment? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;shell-environment::t))
    (define (make-shell-environment . args)
      (apply make-instance shell-environment::t args))
    (define (&shell-environment-vars-set! obj val)
      (unchecked-slot-set! obj 'vars val))
    (define (&shell-environment-vars obj)
      (unchecked-slot-ref obj 'vars))
    (define (shell-environment-vars-set! obj val)
      (unchecked-slot-set! obj 'vars val))
    (define (shell-environment-vars obj)
      (unchecked-slot-ref obj 'vars))
    (define (&shell-environment-parent-set! obj val)
      (unchecked-slot-set! obj 'parent val))
    (define (&shell-environment-parent obj)
      (unchecked-slot-ref obj 'parent))
    (define (shell-environment-parent-set! obj val)
      (unchecked-slot-set! obj 'parent val))
    (define (shell-environment-parent obj)
      (unchecked-slot-ref obj 'parent))
    (define (&shell-environment-functions-set! obj val)
      (unchecked-slot-set! obj 'functions val))
    (define (&shell-environment-functions obj)
      (unchecked-slot-ref obj 'functions))
    (define (shell-environment-functions-set! obj val)
      (unchecked-slot-set! obj 'functions val))
    (define (shell-environment-functions obj)
      (unchecked-slot-ref obj 'functions))
    (define (&shell-environment-aliases-set! obj val)
      (unchecked-slot-set! obj 'aliases val))
    (define (&shell-environment-aliases obj)
      (unchecked-slot-ref obj 'aliases))
    (define (shell-environment-aliases-set! obj val)
      (unchecked-slot-set! obj 'aliases val))
    (define (shell-environment-aliases obj)
      (unchecked-slot-ref obj 'aliases))
    (define (&shell-environment-options-set! obj val)
      (unchecked-slot-set! obj 'options val))
    (define (&shell-environment-options obj)
      (unchecked-slot-ref obj 'options))
    (define (shell-environment-options-set! obj val)
      (unchecked-slot-set! obj 'options val))
    (define (shell-environment-options obj)
      (unchecked-slot-ref obj 'options))
    (define (&shell-environment-shopts-set! obj val)
      (unchecked-slot-set! obj 'shopts val))
    (define (&shell-environment-shopts obj)
      (unchecked-slot-ref obj 'shopts))
    (define (shell-environment-shopts-set! obj val)
      (unchecked-slot-set! obj 'shopts val))
    (define (shell-environment-shopts obj)
      (unchecked-slot-ref obj 'shopts))
    (define (&shell-environment-positional-set! obj val)
      (unchecked-slot-set! obj 'positional val))
    (define (&shell-environment-positional obj)
      (unchecked-slot-ref obj 'positional))
    (define (shell-environment-positional-set! obj val)
      (unchecked-slot-set! obj 'positional val))
    (define (shell-environment-positional obj)
      (unchecked-slot-ref obj 'positional))
    (define (&shell-environment-last-status-set! obj val)
      (unchecked-slot-set! obj 'last-status val))
    (define (&shell-environment-last-status obj)
      (unchecked-slot-ref obj 'last-status))
    (define (shell-environment-last-status-set! obj val)
      (unchecked-slot-set! obj 'last-status val))
    (define (shell-environment-last-status obj)
      (unchecked-slot-ref obj 'last-status))
    (define (&shell-environment-last-bg-pid-set! obj val)
      (unchecked-slot-set! obj 'last-bg-pid val))
    (define (&shell-environment-last-bg-pid obj)
      (unchecked-slot-ref obj 'last-bg-pid))
    (define (shell-environment-last-bg-pid-set! obj val)
      (unchecked-slot-set! obj 'last-bg-pid val))
    (define (shell-environment-last-bg-pid obj)
      (unchecked-slot-ref obj 'last-bg-pid))
    (define (&shell-environment-shell-pid-set! obj val)
      (unchecked-slot-set! obj 'shell-pid val))
    (define (&shell-environment-shell-pid obj)
      (unchecked-slot-ref obj 'shell-pid))
    (define (shell-environment-shell-pid-set! obj val)
      (unchecked-slot-set! obj 'shell-pid val))
    (define (shell-environment-shell-pid obj)
      (unchecked-slot-ref obj 'shell-pid))
    (define (&shell-environment-shell-name-set! obj val)
      (unchecked-slot-set! obj 'shell-name val))
    (define (&shell-environment-shell-name obj)
      (unchecked-slot-ref obj 'shell-name))
    (define (shell-environment-shell-name-set! obj val)
      (unchecked-slot-set! obj 'shell-name val))
    (define (shell-environment-shell-name obj)
      (unchecked-slot-ref obj 'shell-name))
    (define (&shell-environment-start-time-set! obj val)
      (unchecked-slot-set! obj 'start-time val))
    (define (&shell-environment-start-time obj)
      (unchecked-slot-ref obj 'start-time))
    (define (shell-environment-start-time-set! obj val)
      (unchecked-slot-set! obj 'start-time val))
    (define (shell-environment-start-time obj)
      (unchecked-slot-ref obj 'start-time))
    (define (&shell-environment-cmd-number-set! obj val)
      (unchecked-slot-set! obj 'cmd-number val))
    (define (&shell-environment-cmd-number obj)
      (unchecked-slot-ref obj 'cmd-number))
    (define (shell-environment-cmd-number-set! obj val)
      (unchecked-slot-set! obj 'cmd-number val))
    (define (shell-environment-cmd-number obj)
      (unchecked-slot-ref obj 'cmd-number))
    (define (&shell-environment-traps-set! obj val)
      (unchecked-slot-set! obj 'traps val))
    (define (&shell-environment-traps obj)
      (unchecked-slot-ref obj 'traps))
    (define (shell-environment-traps-set! obj val)
      (unchecked-slot-set! obj 'traps val))
    (define (shell-environment-traps obj)
      (unchecked-slot-ref obj 'traps))
    (define (&shell-environment-dir-stack-set! obj val)
      (unchecked-slot-set! obj 'dir-stack val))
    (define (&shell-environment-dir-stack obj)
      (unchecked-slot-ref obj 'dir-stack))
    (define (shell-environment-dir-stack-set! obj val)
      (unchecked-slot-set! obj 'dir-stack val))
    (define (shell-environment-dir-stack obj)
      (unchecked-slot-ref obj 'dir-stack)))
  (define resolve-nameref
    (case-lambda
      [(name env)
       (let* ([depth 0])
         (if (> depth 10)
             name
             (let ([var (find-var-in-chain env name)])
               (if (and var
                        (shell-var-nameref? var)
                        (string? (shell-var-value var)))
                   (resolve-nameref (shell-var-value var) env (+ depth 1))
                   name))))]
      [(name env depth)
       (if (> depth 10)
           name
           (let ([var (find-var-in-chain env name)])
             (if (and var
                      (shell-var-nameref? var)
                      (string? (shell-var-value var)))
                 (resolve-nameref (shell-var-value var) env (+ depth 1))
                 name)))]))
  (define (env-get env name)
    (cond
      [(string=? name "?")
       (number->string (shell-environment-last-status env))]
      [(string=? name "$")
       (number->string (shell-environment-shell-pid env))]
      [(string=? name "!")
       (number->string (shell-environment-last-bg-pid env))]
      [(string=? name "#")
       (number->string
         (vector-length (shell-environment-positional env)))]
      [(string=? name "@")
       (let ([pos (env-positional-list env)])
         (if (null? pos) #f (string-join-with " " pos)))]
      [(string=? name "*")
       (let ([pos (env-positional-list env)])
         (if (null? pos) #f (env-star env)))]
      [(string=? name "0") (shell-environment-shell-name env)]
      [(string=? name "-") (options->flag-string env)]
      [(string=? name "_") (env-get-local env "_")]
      [(string=? name "RANDOM") (number->string (random 32768))]
      [(string=? name "SECONDS")
       (number->string
         (inexact->exact
           (floor
             (- (let ([t (current-time)])
                  (if (time? t)
                      (+ (time-second t)
                         (/ (time-nanosecond t) 1000000000.0))
                      t))
                (shell-environment-start-time env)))))]
      [(string=? name "LINENO")
       (or (env-get-local env "LINENO") "0")]
      [(positional-index name) =>
       (lambda (idx)
         (let ([pos (shell-environment-positional env)])
           (if (and (> idx 0) (<= idx (vector-length pos)))
               (vector-ref pos (- idx 1))
               "")))]
      [else
       (let ([resolved (resolve-nameref name env)])
         (let ([var (find-var-in-chain env resolved)])
           (cond
             [(not var) (getenv resolved #f)]
             [(shell-var-nameref? var) #f]
             [else (shell-var-scalar-value var)])))]))
  (define (env-get-local env name)
    (let ([var (hash-get (shell-environment-vars env) name)])
      (and var
           (not (eq? (shell-var-value var) \x2B;unset-sentinel+))
           (shell-var-scalar-value var))))
  (define (env-get-chain env name)
    (let ([var (hash-get (shell-environment-vars env) name)])
      (if var
          (if (eq? (shell-var-value var) \x2B;unset-sentinel+)
              #f
              (shell-var-scalar-value var))
          (let ([parent (shell-environment-parent env)])
            (if parent (env-get-chain parent name) (getenv name #f))))))
  (define (shell-var-scalar-value var)
    (let ([v (shell-var-value var)])
      (if (hash-table? v)
          (or (hash-get v 0) (hash-get v "0"))
          v)))
  (define apply-var-attrs
    (case-lambda
      [(var value)
       (let* ([env #f])
         (let ([v (if (shell-var-integer? var)
                      (let ([arith-fn (*arith-eval-fn*)])
                        (if arith-fn
                            (number->string
                              (arith-fn
                                value
                                (if env
                                    (lambda (name)
                                      (or (env-get env name) "0"))
                                    (lambda (name) "0"))
                                (if env
                                    (lambda (name val)
                                      (env-set!
                                        env
                                        name
                                        (number->string val)))
                                    (lambda (name val) (%%void)))))
                            (let ([n (or (string->number value) 0)])
                              (number->string n))))
                      value)])
           (cond
             [(shell-var-uppercase? var) (string-upcase v)]
             [(shell-var-lowercase? var) (string-downcase v)]
             [else v])))]
      [(var value env)
       (let ([v (if (shell-var-integer? var)
                    (let ([arith-fn (*arith-eval-fn*)])
                      (if arith-fn
                          (number->string
                            (arith-fn
                              value
                              (if env
                                  (lambda (name)
                                    (or (env-get env name) "0"))
                                  (lambda (name) "0"))
                              (if env
                                  (lambda (name val)
                                    (env-set!
                                      env
                                      name
                                      (number->string val)))
                                  (lambda (name val) (%%void)))))
                          (let ([n (or (string->number value) 0)])
                            (number->string n))))
                    value)])
         (cond
           [(shell-var-uppercase? var) (string-upcase v)]
           [(shell-var-lowercase? var) (string-downcase v)]
           [else v]))]))
  (define (env-get-var env name)
    (let ([var (hash-get (shell-environment-vars env) name)])
      (cond
        [(and var (eq? (shell-var-value var) \x2B;unset-sentinel+))
         #f]
        [var var]
        [else
         (let ([parent (shell-environment-parent env)])
           (if parent (env-get-var parent name) #f))])))
  (define (env-get-raw-var env name)
    (let ([var (hash-get (shell-environment-vars env) name)])
      (cond
        [var var]
        [else
         (let ([parent (shell-environment-parent env)])
           (if parent (env-get-raw-var parent name) #f))])))
  (define (env-matching-names env prefix)
    (let ([result (make-hash-table)])
      (let walk ([e env])
        (when e
          (for-each
            (lambda (pair)
              (let ([k (car pair)] [v (cdr pair)])
                (when (and (string? k)
                           (let ([pfx prefix] [str k])
                             (let ([plen (string-length pfx)])
                               (and (<= plen (string-length str))
                                    (string=?
                                      pfx
                                      (substring str 0 plen)))))
                           (not (eq? (shell-var-value v)
                                     \x2B;unset-sentinel+)))
                  (hash-put! result k #t))))
            (hash->list (shell-environment-vars e)))
          (walk (shell-environment-parent e))))
      (for-each
        (lambda (pair)
          (when (let ([pfx prefix] [str (car pair)])
                  (let ([plen (string-length pfx)])
                    (and (<= plen (string-length str))
                         (string=? pfx (substring str 0 plen)))))
            (hash-put! result (car pair) #t)))
        (get-environment-variables))
      (sort! (hash-keys result) string<?)))
  (define (env-set! env name value)
    (let* ([resolved (resolve-nameref name env)])
      (let* ([existing (hash-get
                         (shell-environment-vars env)
                         resolved)])
        (cond
          [(and existing (shell-var-readonly? existing))
           (error 'gerbil (format "~a: readonly variable" name))]
          [existing
           (let ([final-value (apply-var-attrs existing value env)])
             (shell-var-value-set! existing final-value)
             (when (shell-var-exported? existing)
               (setenv resolved final-value)))]
          [else
           (let ([owner (find-var-owner-in-chain
                          (shell-environment-parent env)
                          resolved)])
             (if owner
                 (let ([parent-var (hash-get
                                     (shell-environment-vars owner)
                                     resolved)])
                   (when (and parent-var (shell-var-readonly? parent-var))
                     (error 'gerbil
                       (format "~a: readonly variable" resolved)))
                   (let ([final-value (apply-var-attrs
                                        parent-var
                                        value
                                        env)])
                     (shell-var-value-set! parent-var final-value)
                     (when (shell-var-exported? parent-var)
                       (setenv resolved final-value))))
                 (let* ([root (env-root env)])
                   (let* ([os-val (getenv resolved #f)])
                     (let* ([export? (or os-val
                                         (env-option? env "allexport"))])
                       (if export?
                           (begin
                             (hash-put!
                               (shell-environment-vars root)
                               resolved
                               (make-shell-var value #t #f #f #f #f #f #f
                                 #f #f))
                             (setenv resolved value))
                           (hash-put!
                             (shell-environment-vars root)
                             resolved
                             (make-shell-var value #f #f #f #f #f #f #f #f
                               #f))))))))])
        (when (env-option? env "allexport")
          (let ([var (hash-get
                       (shell-environment-vars env)
                       resolved)])
            (when (and var (not (shell-var-exported? var)))
              (shell-var-exported?-set! var #t)
              (when (shell-var-value var)
                (setenv resolved (shell-var-value var)))))))))
  (define (env-set-shell-name! env name)
    (shell-environment-shell-name-set! env name))
  (define env-export!
    (case-lambda
      [(env name)
       (let* ([value #f])
         (when value (env-set! env name value))
         (let ([var (env-get-raw-var env name)])
           (if var
               (begin
                 (when (or (shell-var-array? var) (shell-var-assoc? var))
                   (shell-var-exported?-set! var #f)
                   (ffi-unsetenv name))
                 (unless (or (shell-var-array? var) (shell-var-assoc? var))
                   (shell-var-exported?-set! var #t)
                   (let ([v (shell-var-scalar-value var)])
                     (when v (setenv name v)))))
               (if value
                   (begin
                     (hash-put!
                       (shell-environment-vars env)
                       name
                       (make-shell-var value #t #f #f #f #f #f #f #f #f))
                     (setenv name value))
                   (hash-put!
                     (shell-environment-vars env)
                     name
                     (make-shell-var \x2B;unset-sentinel+ #t #f #f #f #f #f
                       #f #f #f))))))]
      [(env name value)
       (when value (env-set! env name value))
       (let ([var (env-get-raw-var env name)])
         (if var
             (begin
               (when (or (shell-var-array? var) (shell-var-assoc? var))
                 (shell-var-exported?-set! var #f)
                 (ffi-unsetenv name))
               (unless (or (shell-var-array? var) (shell-var-assoc? var))
                 (shell-var-exported?-set! var #t)
                 (let ([v (shell-var-scalar-value var)])
                   (when v (setenv name v)))))
             (if value
                 (begin
                   (hash-put!
                     (shell-environment-vars env)
                     name
                     (make-shell-var value #t #f #f #f #f #f #f #f #f))
                   (setenv name value))
                 (hash-put!
                   (shell-environment-vars env)
                   name
                   (make-shell-var \x2B;unset-sentinel+ #t #f #f #f #f #f
                     #f #f #f)))))]))
  (define (env-unset! env name)
    (let ([resolved (resolve-nameref name env)])
      (let scope-loop ([e env])
        (if (not e)
            (when (getenv resolved #f) (ffi-unsetenv resolved))
            (let ([var (hash-get (shell-environment-vars e) resolved)])
              (if var
                  (begin
                    (when (shell-var-readonly? var)
                      (error 'gerbil
                        (format "~a: readonly variable" resolved)))
                    (when (or (shell-var-exported? var)
                              (getenv resolved #f))
                      (ffi-unsetenv resolved))
                    (hash-remove! (shell-environment-vars e) resolved))
                  (scope-loop (shell-environment-parent e))))))))
  (define (env-unset-nameref! env name)
    (let ([var (hash-get (shell-environment-vars env) name)])
      (when (and var (shell-var-readonly? var))
        (error 'gerbil (format "~a: readonly variable" name)))
      (hash-remove! (shell-environment-vars env) name)))
  (define env-readonly!
    (case-lambda
      [(env name)
       (let* ([value #f])
         (when value (env-set! env name value))
         (let ([var (env-get-raw-var env name)])
           (when var (shell-var-readonly?-set! var #t))))]
      [(env name value)
       (when value (env-set! env name value))
       (let ([var (env-get-raw-var env name)])
         (when var (shell-var-readonly?-set! var #t)))]))
  (define (env-exported-alist env)
    (let ([result (make-hash-table)])
      (for-each
        (lambda (pair) (hash-put! result (car pair) (cdr pair)))
        (get-environment-variables))
      (env-collect-exports! env result)
      (hash-map (lambda (k v) (string-append k "=" v)) result)))
  (define (env-collect-exports! env target)
    (let ([parent (shell-environment-parent env)])
      (when parent (env-collect-exports! parent target)))
    (for-each
      (lambda (pair)
        (let ([name (car pair)] [var (cdr pair)])
          (when (shell-var-exported? var)
            (let ([v (shell-var-value var)])
              (unless (eq? v \x2B;unset-sentinel+)
                (hash-put!
                  target
                  name
                  (or (shell-var-scalar-value var) "")))))))
      (hash->list (shell-environment-vars env))))
  (define (env-push-scope env)
    (make-shell-environment env (shell-environment-shell-name env)))
  (define (env-clone env)
    (let ([clone (let ([e (make-shell-environment)])
                   (shell-environment-shell-name-set! e (shell-environment-shell-name env))
                   e)])
      (shell-environment-vars-set! clone (clone-var-table env))
      (shell-environment-functions-set!
        clone
        (hash-copy (shell-environment-functions env)))
      (shell-environment-aliases-set!
        clone
        (hash-copy (shell-environment-aliases env)))
      (shell-environment-options-set!
        clone
        (hash-copy (shell-environment-options env)))
      (shell-environment-shopts-set!
        clone
        (hash-copy (shell-environment-shopts env)))
      (shell-environment-positional-set!
        clone
        (vector-copy (shell-environment-positional env)))
      (shell-environment-last-status-set!
        clone
        (shell-environment-last-status env))
      (shell-environment-last-bg-pid-set!
        clone
        (shell-environment-last-bg-pid env))
      (shell-environment-shell-pid-set!
        clone
        (shell-environment-shell-pid env))
      (shell-environment-start-time-set!
        clone
        (shell-environment-start-time env))
      (shell-environment-cmd-number-set!
        clone
        (shell-environment-cmd-number env))
      (shell-environment-traps-set!
        clone
        (hash-copy (shell-environment-traps env)))
      (shell-environment-dir-stack-set!
        clone
        (shell-environment-dir-stack env))
      clone))
  (define (clone-var-table env)
    (let ([result (make-hash-table)])
      (clone-vars-from-chain! env result)
      result))
  (define (clone-vars-from-chain! env target)
    (let ([parent (shell-environment-parent env)])
      (when parent (clone-vars-from-chain! parent target)))
    (for-each
      (lambda (pair)
        (let ([name (car pair)] [var (cdr pair)])
          (hash-put!
            target
            name
            (make-shell-var
              (let ([v (shell-var-value var)])
                (if (hash-table? v) (hash-copy v) v))
              (shell-var-exported? var) (shell-var-readonly? var)
              (shell-var-local? var) (shell-var-integer? var)
              (shell-var-uppercase? var) (shell-var-lowercase? var)
              (shell-var-nameref? var) (shell-var-array? var)
              (shell-var-assoc? var)))))
      (hash->list (shell-environment-vars env))))
  (define (env-pop-scope env)
    (or (shell-environment-parent env)
        (error 'gerbil "cannot pop root environment")))
  (define (env-set-positional! env args)
    (shell-environment-positional-set! env (list->vector args)))
  (define (env-positional-list env)
    (vector->list (shell-environment-positional env)))
  (define (env-star env)
    (let* ([pos (env-positional-list env)])
      (let* ([ifs (or (env-get env "IFS") " \t\n")])
        (let* ([sep (if (> (string-length ifs) 0)
                        (string (string-ref ifs 0))
                        "")])
          (string-join-with sep pos)))))
  (define (env-at env) (env-positional-list env))
  (define (env-option-set! env name value)
    (hash-put! (shell-environment-options env) name value))
  (define (env-option? env name)
    (hash-get (shell-environment-options env) name))
  (define (env-all-options env)
    (let ([opts (shell-environment-options env)]
          [known '("allexport" "braceexpand" "emacs" "errexit" "errtrace"
                   "functrace" "hashall" "histexpand"
                   "interactive-comments" "keyword" "monitor" "noclobber"
                   "noexec" "noglob" "nolog" "notify" "nounset" "onecmd"
                   "physical" "pipefail" "posix" "privileged" "verbose"
                   "vi" "xtrace")])
      (map (lambda (name)
             (cons name (and (hash-get opts name) #t)))
           known)))
  (define (env-all-variables env)
    (let ([result (list)])
      (for-each
        (lambda (pair)
          (let ([name (car pair)] [var (cdr pair)])
            (when (and (shell-var? var)
                       (not (shell-var-array? var))
                       (not (shell-var-assoc? var))
                       (shell-var-value var))
              (set! result
                (cons (cons name (shell-var-value var)) result)))))
        (hash->list (shell-environment-vars env)))
      (sort result (lambda (a b) (string<? (car a) (car b))))))
  (define (env-shopt-set! env name value)
    (hash-put! (shell-environment-shopts env) name value))
  (define (env-shopt? env name)
    (hash-get (shell-environment-shopts env) name))
  (define (env-set-last-status! env status)
    (shell-environment-last-status-set!
      env
      (bitwise-and status 255)))
  (define (env-set-last-bg-pid! env pid)
    (shell-environment-last-bg-pid-set! env pid))
  (define (env-inc-cmd-number! env)
    (shell-environment-cmd-number-set!
      env
      (+ 1 (shell-environment-cmd-number env))))
  (define (env-init! env)
    (for-each
      (lambda (pair)
        (hash-put!
          (shell-environment-vars env)
          (car pair)
          (make-shell-var (cdr pair) #t #f #f #f #f #f #f #f #f)))
      (get-environment-variables))
    (unless (env-get-local env "IFS")
      (env-set! env "IFS" " \t\n"))
    (unless (env-get-local env "PS1")
      (env-set! env "PS1" "\\u@\\h:\\w\\$ "))
    (unless (env-get-local env "PS2") (env-set! env "PS2" "> "))
    (unless (env-get-local env "PS4") (env-set! env "PS4" "+ "))
    (unless (env-get-local env "HISTFILE")
      (env-set!
        env
        "HISTFILE"
        (string-append
          (or (getenv "HOME" #f) ".")
          "/.gsh_history")))
    (unless (env-get-local env "HISTSIZE")
      (env-set! env "HISTSIZE" "1000"))
    (unless (env-get-local env "HISTFILESIZE")
      (env-set! env "HISTFILESIZE" "2000"))
    (let* ([inherited-pwd (getenv "PWD" #f)])
      (let* ([physical-pwd (let ([p (current-directory)])
                             (let ([len (string-length p)])
                               (if (and (> len 1)
                                        (char=?
                                          (string-ref p (- len 1))
                                          #\/))
                                   (substring p 0 (- len 1))
                                   p)))])
        (let* ([valid-inherited? (and inherited-pwd
                                      (> (string-length inherited-pwd) 0)
                                      (char=?
                                        (string-ref inherited-pwd 0)
                                        #\/)
                                      (guard (__exn
                                               [#t
                                                ((lambda (e) #f) __exn)])
                                        (let ([a (gambit-file-info
                                                   inherited-pwd)]
                                              [b (gambit-file-info
                                                   physical-pwd)])
                                          (and (= (file-info-device a)
                                                  (file-info-device b))
                                               (= (file-info-inode a)
                                                  (file-info-inode
                                                    b))))))])
          (let ([pwd-val (if valid-inherited?
                             inherited-pwd
                             physical-pwd)])
            (env-set! env "PWD" pwd-val)
            (*internal-pwd* pwd-val)))))
    (let* ([shlvl-str (or (getenv "SHLVL" #f) "0")])
      (let* ([shlvl (or (string->number shlvl-str) 0)])
        (env-set! env "SHLVL" (number->string (+ shlvl 1)))
        (env-export! env "SHLVL")))
    (env-set! env "PPID" (number->string (ffi-getppid)))
    (env-set! env "SHELL" (or (getenv "SHELL" #f) "/bin/gsh"))
    (env-option-set! env "hashall" #t)
    (env-option-set! env "braceexpand" #t)
    (env-option-set! env "interactive-comments" #t)
    (env-shopt-set! env "cmdhist" #t)
    (env-shopt-set! env "complete_fullquote" #t)
    (env-shopt-set! env "extquote" #t)
    (env-shopt-set! env "force_fignore" #t)
    (env-shopt-set! env "hostcomplete" #t)
    (env-shopt-set! env "interactive_comments" #t)
    (env-shopt-set! env "progcomp" #t)
    (env-shopt-set! env "promptvars" #t)
    (env-shopt-set! env "sourcepath" #t)
    (env-shopt-set! env "globskipdots" #t)
    (set-color-vars! env))
  (define (set-color-vars! env)
    (let* ([esc (string (integer->char 27))])
      (let* ([csi (string-append esc "[")])
        (define (ansi code) (string-append csi code "m"))
        (for-each
          (lambda (pair) (env-set! env (car pair) (cdr pair)))
          `(("_ansi_reset" . ,(ansi "0")) ("_ansi_bold" . ,(ansi "1"))
            ("_fg_norm_black_" . ,(ansi "30"))
            ("_fg_norm_red" . ,(ansi "31"))
            ("_fg_norm_green" . ,(ansi "32"))
            ("_fg_norm_yellow" . ,(ansi "33"))
            ("_fg_norm_blue" . ,(ansi "34"))
            ("_fg_norm_magenta" . ,(ansi "35"))
            ("_fg_norm_cyan" . ,(ansi "36"))
            ("_fg_norm_white" . ,(ansi "37"))
            ("_fg_bright_black" . ,(ansi "90"))
            ("_fg_bright_red" . ,(ansi "91"))
            ("_fg_bright_green" . ,(ansi "92"))
            ("_fg_bright_yellow" . ,(ansi "93"))
            ("_fg_bright_blue" . ,(ansi "94"))
            ("_fg_bright_magenta" . ,(ansi "95"))
            ("_fg_bright_cyan" . ,(ansi "96"))
            ("_fg_bright_white" . ,(ansi "97"))
            ("_bg_norm_black_" . ,(ansi "40"))
            ("_bg_norm_red" . ,(ansi "41"))
            ("_bg_norm_green" . ,(ansi "42"))
            ("_bg_norm_yellow" . ,(ansi "43"))
            ("_bg_norm_blue" . ,(ansi "44"))
            ("_bg_norm_magenta" . ,(ansi "45"))
            ("_bg_norm_cyan" . ,(ansi "46"))
            ("_bg_norm_white" . ,(ansi "47"))
            ("_bg_bright_black" . ,(ansi "100"))
            ("_bg_bright_red" . ,(ansi "101"))
            ("_bg_bright_green" . ,(ansi "102"))
            ("_bg_bright_yellow" . ,(ansi "103"))
            ("_bg_bright_blue" . ,(ansi "104"))
            ("_bg_bright_magenta" . ,(ansi "105"))
            ("_bg_bright_cyan" . ,(ansi "106"))
            ("_bg_bright_white" . ,(ansi "107")))))))
  (define (positional-index name)
    (and (> (string-length name) 0)
         (let loop ([i 0])
           (if (>= i (string-length name))
               (string->number name)
               (and (char-numeric? (string-ref name i)) (loop (+ i 1)))))))
  (define (find-var-in-chain env name)
    (let ([var (hash-get (shell-environment-vars env) name)])
      (cond
        [(and var (eq? (shell-var-value var) \x2B;unset-sentinel+))
         #f]
        [var var]
        [else
         (let ([parent (shell-environment-parent env)])
           (and parent (find-var-in-chain parent name)))])))
  (define (find-var-in-chain-for-write env name)
    (let ([var (hash-get (shell-environment-vars env) name)])
      (cond
        [var var]
        [else
         (let ([parent (shell-environment-parent env)])
           (and parent (find-var-in-chain-for-write parent name)))])))
  (define (env-root env)
    (let ([parent (shell-environment-parent env)])
      (if parent (env-root parent) env)))
  (define (find-var-owner-in-chain env name)
    (and env
         (let ([var (hash-get (shell-environment-vars env) name)])
           (cond
             [(and var (eq? (shell-var-value var) \x2B;unset-sentinel+))
              #f]
             [var env]
             [else
              (find-var-owner-in-chain
                (shell-environment-parent env)
                name)]))))
  (define (options->flag-string env)
    (let ([flags (list)])
      (when (env-option? env "errexit")
        (set! flags (cons #\e flags)))
      (when (env-option? env "noglob")
        (set! flags (cons #\f flags)))
      (when (env-option? env "hashall")
        (set! flags (cons #\h flags)))
      (when (env-option? env "monitor")
        (set! flags (cons #\m flags)))
      (when (env-option? env "noclobber")
        (set! flags (cons #\C flags)))
      (when (env-option? env "nounset")
        (set! flags (cons #\u flags)))
      (when (env-option? env "xtrace")
        (set! flags (cons #\x flags)))
      (when (env-option? env "verbose")
        (set! flags (cons #\v flags)))
      (list->string (reverse flags))))
  (define (string-join-with sep lst)
    (if (null? lst)
        ""
        (call-with-output-string
          (lambda (port)
            (display (car lst) port)
            (for-each
              (lambda (s) (display sep port) (display s port))
              (cdr lst))))))
  (define (env-array-get env name index)
    (let* ([name (resolve-nameref name env)])
      (let* ([var (find-var-in-chain env name)])
        (if (and var
                 (or (shell-var-array? var) (shell-var-assoc? var)))
            (let* ([tbl (shell-var-value var)])
              (let* ([raw-key (if (shell-var-assoc? var)
                                  index
                                  (if (string? index)
                                      (or (string->number index) 0)
                                      index))])
                (let* ([key (if (and (not (shell-var-assoc? var))
                                     (number? raw-key)
                                     (< raw-key 0))
                                (let ([max-key (hash-fold
                                                 (lambda (k v mx)
                                                   (if (> k mx) k mx))
                                                 -1
                                                 tbl)])
                                  (+ max-key 1 raw-key))
                                raw-key)])
                  (or (hash-get tbl key) ""))))
            (if (and var (or (equal? index "0") (equal? index 0)))
                (or (shell-var-scalar-value var) "")
                "")))))
  (define (env-array-element-set? env name index)
    (let* ([name (resolve-nameref name env)])
      (let* ([var (find-var-in-chain env name)])
        (cond
          [(not var) #f]
          [(or (shell-var-array? var) (shell-var-assoc? var))
           (let* ([tbl (shell-var-value var)])
             (let* ([raw-key (if (shell-var-assoc? var)
                                 index
                                 (if (string? index)
                                     (or (string->number index) 0)
                                     index))])
               (let* ([key (if (and (not (shell-var-assoc? var))
                                    (number? raw-key)
                                    (< raw-key 0))
                               (let ([max-key (hash-fold
                                                (lambda (k v mx)
                                                  (if (> k mx) k mx))
                                                -1
                                                tbl)])
                                 (+ max-key 1 raw-key))
                               raw-key)])
                 (hash-key? tbl key))))]
          [(or (equal? index "0") (equal? index 0))
           (not (eq? (shell-var-value var) \x2B;unset-sentinel+))]
          [else #f]))))
  (define (env-array-set! env name index value)
    (let* ([name (resolve-nameref name env)])
      (let* ([var (find-var-in-chain-for-write env name)])
        (cond
          [(and var (shell-var-readonly? var))
           (error 'gerbil (format "~a: readonly variable" name))]
          [(and var
                (or (shell-var-array? var) (shell-var-assoc? var)))
           (let* ([tbl (shell-var-value var)])
             (let* ([raw-key (if (shell-var-assoc? var)
                                 index
                                 (if (string? index)
                                     (or (string->number index) 0)
                                     index))])
               (let* ([key (if (and (not (shell-var-assoc? var))
                                    (number? raw-key)
                                    (< raw-key 0))
                               (let ([max-key (hash-fold
                                                (lambda (k v mx)
                                                  (if (> k mx) k mx))
                                                -1
                                                tbl)])
                                 (+ max-key 1 raw-key))
                               raw-key)])
                 (let* ([final-val (apply-var-attrs var value env)])
                   (hash-put! tbl key final-val)))))]
          [var
           (let ([tbl (make-hash-table)]
                 [key (if (string? index)
                          (or (string->number index) 0)
                          index)])
             (hash-put! tbl key (apply-var-attrs var value env))
             (shell-var-value-set! var tbl)
             (shell-var-array?-set! var #t))]
          [else
           (let* ([root (env-root env)])
             (let* ([tbl (make-hash-table)])
               (let* ([key (if (string? index)
                               (or (string->number index) 0)
                               index)])
                 (let* ([new-var (make-shell-var tbl #f #f #f #f #f #f #f
                                   #t #f)])
                   (hash-put! tbl key value)
                   (hash-put!
                     (shell-environment-vars root)
                     name
                     new-var)))))]))))
  (define (env-array-values env name)
    (let* ([name (resolve-nameref name env)])
      (let* ([var (find-var-in-chain env name)])
        (if (and var
                 (or (shell-var-array? var) (shell-var-assoc? var)))
            (let ([tbl (shell-var-value var)])
              (if (shell-var-assoc? var)
                  (hash-values tbl)
                  (let ([keys (sort! (hash-keys tbl) <)])
                    (map (lambda (k) (hash-get tbl k)) keys))))
            (if var (list (shell-var-scalar-value var)) (list))))))
  (define (env-array-keys env name)
    (let* ([name (resolve-nameref name env)])
      (let* ([var (find-var-in-chain env name)])
        (if (and var
                 (or (shell-var-array? var) (shell-var-assoc? var)))
            (let ([tbl (shell-var-value var)])
              (if (shell-var-assoc? var)
                  (map (lambda (k) (if (string? k) k (number->string k)))
                       (hash-keys tbl))
                  (let ([keys (sort! (hash-keys tbl) <)])
                    (map number->string keys))))
            (if (and var (shell-var-scalar-value var))
                (list "0")
                (list))))))
  (define (env-array-length env name)
    (let* ([name (resolve-nameref name env)])
      (let* ([var (find-var-in-chain env name)])
        (if (and var
                 (or (shell-var-array? var) (shell-var-assoc? var)))
            (hash-length (shell-var-value var))
            (if (and var (shell-var-scalar-value var)) 1 0)))))
  (define (env-array-unset-element! env name index)
    (let* ([name (resolve-nameref name env)])
      (let* ([var (find-var-in-chain-for-write env name)])
        (when (and var (shell-var-readonly? var))
          (error 'gerbil (format "~a: readonly variable" name)))
        (cond
          [(not var) (void)]
          [(or (shell-var-array? var) (shell-var-assoc? var))
           (let* ([tbl (shell-var-value var)])
             (let* ([raw-key (if (shell-var-assoc? var)
                                 index
                                 (if (string? index)
                                     (let ([n (string->number index)])
                                       (if n
                                           n
                                           (guard (__exn
                                                    [#t
                                                     ((lambda (e) 0)
                                                       __exn)])
                                             (let ([arith-fn (*arith-eval-fn*)])
                                               (if arith-fn
                                                   (arith-fn
                                                     index
                                                     (lambda (nm)
                                                       (or (env-get env nm)
                                                           "0"))
                                                     (lambda (nm val)
                                                       (env-set!
                                                         env
                                                         nm
                                                         (number->string
                                                           val))))
                                                   0)))))
                                     index))])
               (let* ([key (if (and (not (shell-var-assoc? var))
                                    (number? raw-key)
                                    (< raw-key 0))
                               (let ([max-key (hash-fold
                                                (lambda (k v mx)
                                                  (if (> k mx) k mx))
                                                -1
                                                tbl)])
                                 (+ max-key 1 raw-key))
                               raw-key)])
                 (hash-remove! tbl key))))]
          [else
           (error 'gerbil
             (format "~a: not an array variable" name))]))))
  (define (env-array-set-compound! env name values assoc?)
    (let* ([name (resolve-nameref name env)])
      (let* ([var (or (find-var-in-chain env name)
                      (let ([new-var (make-shell-var (make-hash-table) #f #f #f #f #f #f
                                       #f (not assoc?) assoc?)])
                        (hash-put!
                          (shell-environment-vars (env-root env))
                          name
                          new-var)
                        new-var))])
        (when (shell-var-readonly? var)
          (error 'gerbil (format "~a: readonly variable" name)))
        (let ([tbl (make-hash-table)])
          (shell-var-value-set! var tbl)
          (shell-var-array?-set! var (not assoc?))
          (shell-var-assoc?-set! var assoc?)
          (let loop ([vals values] [auto-idx 0])
            (when (pair? vals)
              (let ([v (car vals)])
                (if (array-kv-pair? v)
                    (let-values ([(key val) (parse-array-kv v)])
                      (if assoc?
                          (hash-put! tbl key val)
                          (let ([idx (or (string->number key) auto-idx)])
                            (hash-put! tbl idx val)
                            (loop (cdr vals) (+ idx 1)))))
                    (begin
                      (if assoc? (%%void) (hash-put! tbl auto-idx v))
                      (loop (cdr vals) (+ auto-idx 1)))))))))))
  (define (env-array-append-compound! env name values)
    (let* ([name (resolve-nameref name env)])
      (let* ([var (find-var-in-chain env name)])
        (when (and var (shell-var-readonly? var))
          (error 'gerbil (format "~a: readonly variable" name)))
        (if (and var
                 (or (shell-var-array? var) (shell-var-assoc? var)))
            (let* ([tbl (shell-var-value var)])
              (let* ([assoc? (shell-var-assoc? var)])
                (let* ([next-idx (if assoc?
                                     0
                                     (if (> (hash-length tbl) 0)
                                         (+ 1 (apply max (hash-keys tbl)))
                                         0))])
                  (let loop ([vals values] [idx next-idx])
                    (when (pair? vals)
                      (let ([v (car vals)])
                        (if (array-kv-pair? v)
                            (let-values ([(key val) (parse-array-kv v)])
                              (if assoc?
                                  (hash-put! tbl key val)
                                  (let ([kidx (or (string->number key)
                                                  idx)])
                                    (hash-put! tbl kidx val)
                                    (loop (cdr vals) (+ kidx 1)))))
                            (begin
                              (unless assoc? (hash-put! tbl idx v))
                              (loop (cdr vals) (+ idx 1))))))))))
            (env-array-set-compound! env name values #f)))))
  (define (array-kv-pair? str)
    (and (> (string-length str) 0)
         (char=? (string-ref str 0) #\[)
         (let ([close (string-find-char-in str #\])])
           (and close
                (< (+ close 1) (string-length str))
                (char=? (string-ref str (+ close 1)) #\=)))))
  (define (parse-array-kv str)
    (let* ([close (string-find-char-in str #\])])
      (let* ([key (substring str 1 close)])
        (let* ([val (substring
                      str
                      (+ close 2)
                      (string-length str))])
          (values key val)))))
  (define (string-find-char-in str ch)
    (let loop ([i 0])
      (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))])))
  (define (env-is-array? env name)
    (let* ([name (resolve-nameref name env)])
      (let* ([var (find-var-in-chain env name)])
        (and var
             (or (shell-var-array? var) (shell-var-assoc? var))))))
  (define (parse-arith-subscript name)
    (let ([bpos (string-find-char-in name #\[)])
      (if (and bpos
               (> bpos 0)
               (> (string-length name) (+ bpos 1))
               (char=? (string-ref name (- (string-length name) 1)) #\]))
          (values
            (substring name 0 bpos)
            (substring name (+ bpos 1) (- (string-length name) 1)))
          (values #f #f))))
  (define (arith-env-getter env)
    (lambda (name)
      (let-values ([(base idx) (parse-arith-subscript name)])
        (if base
            (let ([var (env-get-var env base)])
              (if var
                  (let ([key (or (string->number idx) idx)])
                    (env-array-get env base key))
                  #f))
            (env-get env name)))))
  (define (arith-env-setter env)
    (lambda (name val)
      (let-values ([(base idx) (parse-arith-subscript name)])
        (if base
            (let ([key (or (string->number idx) idx)])
              (env-array-set! env base key val))
            (env-set! env name val)))))
  (begin
    (define shell-environment:::init!
      (case-lambda
        [(self)
         (let* ([parent #f] [name "gsh"])
           (slot-set! self 'vars (make-hash-table))
           (slot-set! self 'parent parent)
           (slot-set!
             self
             'functions
             (if parent
                 (shell-environment-functions parent)
                 (make-hash-table)))
           (slot-set!
             self
             'aliases
             (if parent
                 (shell-environment-aliases parent)
                 (make-hash-table)))
           (slot-set!
             self
             'options
             (if parent
                 (shell-environment-options parent)
                 (make-hash-table)))
           (slot-set!
             self
             'shopts
             (if parent
                 (shell-environment-shopts parent)
                 (make-hash-table)))
           (slot-set!
             self
             'positional
             (if parent (shell-environment-positional parent) (vector)))
           (slot-set! self 'last-status 0)
           (slot-set! self 'last-bg-pid 0)
           (slot-set!
             self
             'shell-pid
             (if parent
                 (shell-environment-shell-pid parent)
                 (ffi-getpid)))
           (slot-set! self 'shell-name name)
           (slot-set!
             self
             'start-time
             (if parent
                 (shell-environment-start-time parent)
                 (let ([t (current-time)])
                   (if (time? t)
                       (+ (time-second t)
                          (/ (time-nanosecond t) 1000000000.0))
                       t))))
           (slot-set!
             self
             'cmd-number
             (if parent (shell-environment-cmd-number parent) 0))
           (slot-set!
             self
             'traps
             (if parent
                 (shell-environment-traps parent)
                 (make-hash-table)))
           (slot-set!
             self
             'dir-stack
             (if parent (shell-environment-dir-stack parent) (list))))]
        [(self parent)
         (let* ([name "gsh"])
           (slot-set! self 'vars (make-hash-table))
           (slot-set! self 'parent parent)
           (slot-set!
             self
             'functions
             (if parent
                 (shell-environment-functions parent)
                 (make-hash-table)))
           (slot-set!
             self
             'aliases
             (if parent
                 (shell-environment-aliases parent)
                 (make-hash-table)))
           (slot-set!
             self
             'options
             (if parent
                 (shell-environment-options parent)
                 (make-hash-table)))
           (slot-set!
             self
             'shopts
             (if parent
                 (shell-environment-shopts parent)
                 (make-hash-table)))
           (slot-set!
             self
             'positional
             (if parent (shell-environment-positional parent) (vector)))
           (slot-set! self 'last-status 0)
           (slot-set! self 'last-bg-pid 0)
           (slot-set!
             self
             'shell-pid
             (if parent
                 (shell-environment-shell-pid parent)
                 (ffi-getpid)))
           (slot-set! self 'shell-name name)
           (slot-set!
             self
             'start-time
             (if parent
                 (shell-environment-start-time parent)
                 (let ([t (current-time)])
                   (if (time? t)
                       (+ (time-second t)
                          (/ (time-nanosecond t) 1000000000.0))
                       t))))
           (slot-set!
             self
             'cmd-number
             (if parent (shell-environment-cmd-number parent) 0))
           (slot-set!
             self
             'traps
             (if parent
                 (shell-environment-traps parent)
                 (make-hash-table)))
           (slot-set!
             self
             'dir-stack
             (if parent (shell-environment-dir-stack parent) (list))))]
        [(self parent name)
         (slot-set! self 'vars (make-hash-table))
         (slot-set! self 'parent parent)
         (slot-set!
           self
           'functions
           (if parent
               (shell-environment-functions parent)
               (make-hash-table)))
         (slot-set!
           self
           'aliases
           (if parent
               (shell-environment-aliases parent)
               (make-hash-table)))
         (slot-set!
           self
           'options
           (if parent
               (shell-environment-options parent)
               (make-hash-table)))
         (slot-set!
           self
           'shopts
           (if parent
               (shell-environment-shopts parent)
               (make-hash-table)))
         (slot-set!
           self
           'positional
           (if parent (shell-environment-positional parent) (vector)))
         (slot-set! self 'last-status 0)
         (slot-set! self 'last-bg-pid 0)
         (slot-set!
           self
           'shell-pid
           (if parent
               (shell-environment-shell-pid parent)
               (ffi-getpid)))
         (slot-set! self 'shell-name name)
         (slot-set!
           self
           'start-time
           (if parent
               (shell-environment-start-time parent)
               (let ([t (current-time)])
                 (if (time? t)
                     (+ (time-second t)
                        (/ (time-nanosecond t) 1000000000.0))
                     t))))
         (slot-set!
           self
           'cmd-number
           (if parent (shell-environment-cmd-number parent) 0))
         (slot-set!
           self
           'traps
           (if parent
               (shell-environment-traps parent)
               (make-hash-table)))
         (slot-set!
           self
           'dir-stack
           (if parent (shell-environment-dir-stack parent) (list)))]))
    (bind-method!
      shell-environment::t
      ':init!
      shell-environment:::init!)))
