#!chezscheme
(library (jsh arithmetic)
  (export arith-eval arith-tokenize arith-token? arith-token-type
    arith-token-value arith-state? arith-state-tokens
    arith-state-pos arith-state-env-get arith-state-env-set
    arith-state-suppress-effects arith-state-nounset?)
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
    (except (std misc string) string-trim string-join
      string-split string-index string-empty?)
    (except (std misc list) take drop filter-map)
    (except (std misc alist) pget pgetv pgetq aget agetv agetq)
    (except
      (std os path)
      path-expand
      path-normalize
      path-absolute?)
    (except (std format) format) (std sort) (std pregexp)
    (std sugar))
  (define arith-eval
    (case-lambda
      [(expr env-get-fn env-set-fn)
       (let* ([nounset? #f])
         (let* ([tokens (arith-tokenize expr)])
           (let* ([state (make-arith-state tokens 0 env-get-fn
                           env-set-fn #f nounset?)])
             (if (null? tokens)
                 0
                 (let ([result (parse-comma-expr state)])
                   (when (< (arith-state-pos state) (length tokens))
                     (error 'gerbil
                       (format
                         "arithmetic: syntax error: unexpected token '~a'"
                         (arith-token-value
                           (list-ref tokens (arith-state-pos state))))))
                   result)))))]
      [(expr env-get-fn env-set-fn nounset?)
       (let* ([tokens (arith-tokenize expr)])
         (let* ([state (make-arith-state tokens 0 env-get-fn
                         env-set-fn #f nounset?)])
           (if (null? tokens)
               0
               (let ([result (parse-comma-expr state)])
                 (when (< (arith-state-pos state) (length tokens))
                   (error 'gerbil
                     (format
                       "arithmetic: syntax error: unexpected token '~a'"
                       (arith-token-value
                         (list-ref tokens (arith-state-pos state))))))
                 result))))]))
  (begin
    (define arith-state::t
      (make-class-type 'gerbil\x23;arith-state::t 'arith-state (list object::t)
        '(tokens pos env-get env-set suppress-effects nounset?)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-arith-state . args)
      (let* ([type arith-state::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (arith-state? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;arith-state::t))
    (define (arith-state-tokens obj)
      (unchecked-slot-ref obj 'tokens))
    (define (arith-state-pos obj) (unchecked-slot-ref obj 'pos))
    (define (arith-state-env-get obj)
      (unchecked-slot-ref obj 'env-get))
    (define (arith-state-env-set obj)
      (unchecked-slot-ref obj 'env-set))
    (define (arith-state-suppress-effects obj)
      (unchecked-slot-ref obj 'suppress-effects))
    (define (arith-state-nounset? obj)
      (unchecked-slot-ref obj 'nounset?))
    (define (arith-state-tokens-set! obj val)
      (unchecked-slot-set! obj 'tokens val))
    (define (arith-state-pos-set! obj val)
      (unchecked-slot-set! obj 'pos val))
    (define (arith-state-env-get-set! obj val)
      (unchecked-slot-set! obj 'env-get val))
    (define (arith-state-env-set-set! obj val)
      (unchecked-slot-set! obj 'env-set val))
    (define (arith-state-suppress-effects-set! obj val)
      (unchecked-slot-set! obj 'suppress-effects val))
    (define (arith-state-nounset?-set! obj val)
      (unchecked-slot-set! obj 'nounset? val))
    (define (&arith-state-tokens obj)
      (unchecked-slot-ref obj 'tokens))
    (define (&arith-state-pos obj)
      (unchecked-slot-ref obj 'pos))
    (define (&arith-state-env-get obj)
      (unchecked-slot-ref obj 'env-get))
    (define (&arith-state-env-set obj)
      (unchecked-slot-ref obj 'env-set))
    (define (&arith-state-suppress-effects obj)
      (unchecked-slot-ref obj 'suppress-effects))
    (define (&arith-state-nounset? obj)
      (unchecked-slot-ref obj 'nounset?))
    (define (&arith-state-tokens-set! obj val)
      (unchecked-slot-set! obj 'tokens val))
    (define (&arith-state-pos-set! obj val)
      (unchecked-slot-set! obj 'pos val))
    (define (&arith-state-env-get-set! obj val)
      (unchecked-slot-set! obj 'env-get val))
    (define (&arith-state-env-set-set! obj val)
      (unchecked-slot-set! obj 'env-set val))
    (define (&arith-state-suppress-effects-set! obj val)
      (unchecked-slot-set! obj 'suppress-effects val))
    (define (&arith-state-nounset?-set! obj val)
      (unchecked-slot-set! obj 'nounset? val)))
  (begin
    (define arith-token::t
      (make-class-type 'gerbil\x23;arith-token::t 'arith-token (list object::t)
        '(type value) '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-arith-token . args)
      (let* ([type arith-token::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (arith-token? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;arith-token::t))
    (define (arith-token-type obj)
      (unchecked-slot-ref obj 'type))
    (define (arith-token-value obj)
      (unchecked-slot-ref obj 'value))
    (define (arith-token-type-set! obj val)
      (unchecked-slot-set! obj 'type val))
    (define (arith-token-value-set! obj val)
      (unchecked-slot-set! obj 'value val))
    (define (&arith-token-type obj)
      (unchecked-slot-ref obj 'type))
    (define (&arith-token-value obj)
      (unchecked-slot-ref obj 'value))
    (define (&arith-token-type-set! obj val)
      (unchecked-slot-set! obj 'type val))
    (define (&arith-token-value-set! obj val)
      (unchecked-slot-set! obj 'value val)))
  (define (arith-tokenize expr)
    (let loop ([i 0] [tokens (list)])
      (cond
        [(>= i (string-length expr)) (reverse tokens)]
        [(char-whitespace? (string-ref expr i))
         (loop (+ i 1) tokens)]
        [(char-numeric? (string-ref expr i))
         (let-values ([(num end) (read-number expr i)])
           (loop end (cons (make-arith-token 'number num) tokens)))]
        [(or (char-alphabetic? (string-ref expr i))
             (char=? (string-ref expr i) #\_))
         (let-values ([(name end) (read-name expr i)])
           (loop end (cons (make-arith-token 'name name) tokens)))]
        [(char=? (string-ref expr i) #\#)
         (error 'gerbil
           (format "arithmetic: syntax error: unexpected token '#'"))]
        [else
         (let-values ([(op end) (read-operator expr i)])
           (loop end (cons (make-arith-token 'op op) tokens)))])))
  (define (read-number expr i)
    (let ([len (string-length expr)])
      (cond
        [(and (< (+ i 1) len)
              (char=? (string-ref expr i) #\0)
              (or (char=? (string-ref expr (+ i 1)) #\x)
                  (char=? (string-ref expr (+ i 1)) #\X)))
         (let loop ([j (+ i 2)])
           (if (and (< j len) (hex-digit? (string-ref expr j)))
               (loop (+ j 1))
               (begin
                 (when (and (< j len)
                            (let ([ch (string-ref expr j)])
                              (or (char-alphabetic? ch) (char=? ch #\_))))
                   (error 'gerbil
                     (format
                       "arithmetic: invalid hex constant: ~a"
                       (substring
                         expr
                         i
                         (let lp ([k j])
                           (if (and (< k len)
                                    (let ([c (string-ref expr k)])
                                      (or (arith-alnum? c)
                                          (char=? c #\_))))
                               (lp (+ k 1))
                               k))))))
                 (let ([num (string->number
                              (substring expr (+ i 2) j)
                              16)])
                   (if num
                       (values num j)
                       (error 'gerbil
                         (format
                           "arithmetic: invalid hex constant: ~a"
                           (substring expr i j))))))))]
        [(and (< (+ i 1) len)
              (char=? (string-ref expr i) #\0)
              (or (char=? (string-ref expr (+ i 1)) #\b)
                  (char=? (string-ref expr (+ i 1)) #\B)))
         (let loop ([j (+ i 2)])
           (if (and (< j len)
                    (or (char=? (string-ref expr j) #\0)
                        (char=? (string-ref expr j) #\1)))
               (loop (+ j 1))
               (values (string->number (substring expr (+ i 2) j) 2) j)))]
        [(and (char=? (string-ref expr i) #\0)
              (< (+ i 1) len)
              (char-numeric? (string-ref expr (+ i 1))))
         (let loop ([j (+ i 1)])
           (if (and (< j len) (char-numeric? (string-ref expr j)))
               (loop (+ j 1))
               (let check ([k (+ i 1)])
                 (if (< k j)
                     (if (octal-digit? (string-ref expr k))
                         (check (+ k 1))
                         (error 'gerbil
                           (format
                             "arithmetic: invalid octal constant: ~a"
                             (substring expr i j))))
                     (if (and (< j len) (char=? (string-ref expr j) #\#))
                         (let ([end (let lp ([k (+ j 1)])
                                      (if (and (< k len)
                                               (arith-alnum?
                                                 (string-ref expr k)))
                                          (lp (+ k 1))
                                          k))])
                           (error 'gerbil
                             (format
                               "arithmetic: invalid number: ~a"
                               (substring expr i end))))
                         (let ([num (string->number
                                      (substring expr (+ i 1) j)
                                      8)])
                           (if num
                               (values num j)
                               (error 'gerbil
                                 (format
                                   "arithmetic: invalid octal constant: ~a"
                                   (substring expr i j))))))))))]
        [else
         (let loop ([j i])
           (if (and (< j len) (char-numeric? (string-ref expr j)))
               (loop (+ j 1))
               (cond
                 [(and (< j len) (char=? (string-ref expr j) #\.))
                  (error 'gerbil
                    "arithmetic: invalid number (float not supported)")]
                 [(and (< j len) (char=? (string-ref expr j) #\#))
                  (let ([base (string->number (substring expr i j))])
                    (if (and base (>= base 2) (<= base 64))
                        (let vloop ([k (+ j 1)] [val 0])
                          (if (and (< k len)
                                   (base-n-digit?
                                     (string-ref expr k)
                                     base))
                              (vloop
                                (+ k 1)
                                (+ (* val base)
                                   (base-n-digit-value
                                     (string-ref expr k))))
                              (if (and (< k len)
                                       (let ([ch (string-ref expr k)])
                                         (or (arith-alnum? ch)
                                             (char=? ch #\_)
                                             (char=? ch #\@))))
                                  (error 'gerbil
                                    (format
                                      "arithmetic: invalid base ~a constant: ~a"
                                      base
                                      (substring
                                        expr
                                        i
                                        (let lp ([kk k])
                                          (if (and (< kk len)
                                                   (let ([c (string-ref
                                                              expr
                                                              kk)])
                                                     (or (arith-alnum? c)
                                                         (char=? c #\_)
                                                         (char=? c #\@))))
                                              (lp (+ kk 1))
                                              kk)))))
                                  (if (= k (+ j 1))
                                      (error 'gerbil
                                        (format
                                          "arithmetic: invalid constant: ~a"
                                          (substring expr i (+ j 1))))
                                      (values val k)))))
                        (error 'gerbil
                          (format
                            "arithmetic: invalid base: ~a"
                            (substring expr i j)))))]
                 [(and (< j len)
                       (let ([ch (string-ref expr j)])
                         (or (char-alphabetic? ch) (char=? ch #\_))))
                  (let lp ([k j])
                    (if (and (< k len)
                             (let ([ch (string-ref expr k)])
                               (or (arith-alnum? ch) (char=? ch #\_))))
                        (lp (+ k 1))
                        (error 'gerbil
                          (format
                            "arithmetic: invalid constant: ~a"
                            (substring expr i k)))))]
                 [else
                  (values (string->number (substring expr i j)) j)])))])))
  (define (read-name expr i)
    (let ([len (string-length expr)])
      (let loop ([j i])
        (if (and (< j len)
                 (let ([ch (string-ref expr j)])
                   (or (char-alphabetic? ch)
                       (char-numeric? ch)
                       (char=? ch #\_))))
            (loop (+ j 1))
            (values (substring expr i j) j)))))
  (define (read-operator expr i)
    (let ([len (string-length expr)] [ch (string-ref expr i)])
      (cond
        [(and (< (+ i 2) len)
              (string=? (substring expr i (+ i 3)) "<<="))
         (values "<<=" (+ i 3))]
        [(and (< (+ i 2) len)
              (string=? (substring expr i (+ i 3)) ">>="))
         (values ">>=" (+ i 3))]
        [(< (+ i 1) len)
         (let ([two (substring expr i (+ i 2))])
           (cond
             [(member
                two
                '("==" "!=" "<=" ">=" "&&" "||" "<<" ">>" "+=" "-=" "*="
                   "/=" "%=" "&=" "^=" "|=" "++" "--" "**"))
              (values two (+ i 2))]
             [else (values (string ch) (+ i 1))]))]
        [else (values (string ch) (+ i 1))])))
  (define (arith-alnum? ch)
    (or (char-alphabetic? ch) (char-numeric? ch)))
  (define (hex-digit? ch)
    (or (char-numeric? ch)
        (and (char>=? ch #\a) (char<=? ch #\f))
        (and (char>=? ch #\A) (char<=? ch #\F))))
  (define (octal-digit? ch)
    (and (char>=? ch #\0) (char<=? ch #\7)))
  (define (base-n-digit? ch base)
    (let ([v (base-n-digit-value-raw ch)]) (and v (< v base))))
  (define (base-n-digit-value ch)
    (or (base-n-digit-value-raw ch) 0))
  (define (base-n-digit-value-raw ch)
    (cond
      [(and (char>=? ch #\0) (char<=? ch #\9))
       (- (char->integer ch) (char->integer #\0))]
      [(and (char>=? ch #\a) (char<=? ch #\z))
       (+ 10 (- (char->integer ch) (char->integer #\a)))]
      [(and (char>=? ch #\A) (char<=? ch #\Z))
       (+ 36 (- (char->integer ch) (char->integer #\A)))]
      [(char=? ch #\@) 62]
      [(char=? ch #\_) 63]
      [else #f]))
  (define (arith-peek state)
    (if (>= (arith-state-pos state)
            (length (arith-state-tokens state)))
        #f
        (list-ref
          (arith-state-tokens state)
          (arith-state-pos state))))
  (define (arith-advance! state)
    (arith-state-pos-set! state (+ 1 (arith-state-pos state))))
  (define (arith-expect-op! state expected)
    (let ([tok (arith-peek state)])
      (if (and tok
               (eq? (arith-token-type tok) 'op)
               (string=? (arith-token-value tok) expected))
          (begin (arith-advance! state) #t)
          (error 'gerbil
            (format "arithmetic: expected ~a" expected)))))
  (define (arith-match-op? state op)
    (let ([tok (arith-peek state)])
      (and tok
           (eq? (arith-token-type tok) 'op)
           (string=? (arith-token-value tok) op))))
  (define (arith-consume-op! state op)
    (if (arith-match-op? state op)
        (begin (arith-advance! state) #t)
        #f))
  (define (arith-get-var state name)
    (let resolve ([name name] [depth 0])
      (if (> depth 10)
          0
          (let ([val ((arith-state-env-get state) name)])
            (cond
              [(not val)
               (let ([nu (arith-state-nounset? state)])
                 (cond
                   [(procedure? nu) (nu name)]
                   [nu
                    (error 'gerbil
                      (format "arithmetic: ~a: unbound variable" name))]))
               0]
              [else
               (let ([trimmed (string-trim val)])
                 (if (string=? trimmed "")
                     0
                     (let ([num (string->number trimmed)])
                       (if num
                           num
                           (if (and (> (string-length trimmed) 0)
                                    (or (char-alphabetic?
                                          (string-ref trimmed 0))
                                        (char=?
                                          (string-ref trimmed 0)
                                          #\_))
                                    (let check ([j 0])
                                      (or (>= j (string-length trimmed))
                                          (let ([ch (string-ref
                                                      trimmed
                                                      j)])
                                            (and (or (char-alphabetic? ch)
                                                     (char-numeric? ch)
                                                     (char=? ch #\_))
                                                 (check (+ j 1)))))))
                               (resolve trimmed (+ depth 1))
                               (arith-eval
                                 trimmed
                                 (arith-state-env-get state)
                                 (arith-state-env-set state)))))))])))))
  (define (string-trim s)
    (let* ([len (string-length s)])
      (let* ([start (let loop ([i 0])
                      (if (and (< i len)
                               (char-whitespace? (string-ref s i)))
                          (loop (+ i 1))
                          i))])
        (let* ([end (let loop ([i len])
                      (if (and (> i start)
                               (char-whitespace? (string-ref s (- i 1))))
                          (loop (- i 1))
                          i))])
          (substring s start end)))))
  (define (arith-set-var! state name value)
    (unless (arith-state-suppress-effects state)
      ((arith-state-env-set state) name (number->string value)))
    value)
  (define (arith-resolve-name state name)
    (let resolve ([name name] [depth 0])
      (if (> depth 10)
          name
          (let ([val ((arith-state-env-get state) name)])
            (if (not val)
                name
                (let ([num (string->number val)])
                  (if num
                      name
                      (if (and (> (string-length val) 0)
                               (or (char-alphabetic? (string-ref val 0))
                                   (char=? (string-ref val 0) #\_))
                               (let check ([j 0])
                                 (or (>= j (string-length val))
                                     (let ([ch (string-ref val j)])
                                       (and (or (char-alphabetic? ch)
                                                (char-numeric? ch)
                                                (char=? ch #\_))
                                            (check (+ j 1)))))))
                          (resolve val (+ depth 1))
                          name))))))))
  (define (parse-comma-expr state)
    (let loop ([result (parse-assignment-expr state)])
      (if (arith-consume-op! state ",")
          (loop (parse-assignment-expr state))
          result)))
  (define (parse-assignment-expr state)
    (let ([tok (arith-peek state)])
      (if (and tok (eq? (arith-token-type tok) 'name))
          (let* ([name (arith-token-value tok)])
            (let* ([saved-pos (arith-state-pos state)])
              (arith-advance! state)
              (let-values ([(final-name is-array?)
                            (parse-array-index state name)])
                (let ([op-tok (arith-peek state)])
                  (if (and op-tok
                           (eq? (arith-token-type op-tok) 'op)
                           (member
                             (arith-token-value op-tok)
                             '("=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>="
                                   "&=" "^=" "|=")))
                      (let ([op (arith-token-value op-tok)]
                            [target (if is-array?
                                        final-name
                                        (arith-resolve-name
                                          state
                                          final-name))])
                        (arith-advance! state)
                        (let ([rhs (parse-assignment-expr state)])
                          (cond
                            [(string=? op "=")
                             (arith-set-var! state target rhs)]
                            [(string=? op "+=")
                             (arith-set-var!
                               state
                               target
                               (+ (arith-get-var state target) rhs))]
                            [(string=? op "-=")
                             (arith-set-var!
                               state
                               target
                               (- (arith-get-var state target) rhs))]
                            [(string=? op "*=")
                             (arith-set-var!
                               state
                               target
                               (* (arith-get-var state target) rhs))]
                            [(string=? op "/=")
                             (arith-set-var!
                               state
                               target
                               (truncate-quotient
                                 (arith-get-var state target)
                                 rhs))]
                            [(string=? op "%=")
                             (arith-set-var!
                               state
                               target
                               (truncate-remainder
                                 (arith-get-var state target)
                                 rhs))]
                            [(string=? op "<<=")
                             (arith-set-var!
                               state
                               target
                               (arithmetic-shift
                                 (arith-get-var state target)
                                 rhs))]
                            [(string=? op ">>=")
                             (arith-set-var!
                               state
                               target
                               (arithmetic-shift
                                 (arith-get-var state target)
                                 (- rhs)))]
                            [(string=? op "&=")
                             (arith-set-var!
                               state
                               target
                               (bitwise-and
                                 (arith-get-var state target)
                                 rhs))]
                            [(string=? op "^=")
                             (arith-set-var!
                               state
                               target
                               (bitwise-xor
                                 (arith-get-var state target)
                                 rhs))]
                            [(string=? op "|=")
                             (arith-set-var!
                               state
                               target
                               (bitwise-ior
                                 (arith-get-var state target)
                                 rhs))]
                            [else
                             (error 'gerbil "unknown assignment op" op)])))
                      (begin
                        (arith-state-pos-set! state saved-pos)
                        (parse-ternary-expr state)))))))
          (parse-ternary-expr state))))
  (define (parse-array-index state name)
    (if (arith-consume-op! state "[")
        (let ([idx (parse-comma-expr state)])
          (arith-expect-op! state "]")
          (values
            (string-append name "[" (number->string idx) "]")
            #t))
        (values name #f)))
  (define (parse-ternary-expr state)
    (let ([cond-val (parse-logical-or state)])
      (if (arith-consume-op! state "?")
          (if (not (= cond-val 0))
              (let ([then-val (parse-comma-expr state)])
                (arith-expect-op! state ":")
                (let ([saved (arith-state-suppress-effects state)])
                  (arith-state-suppress-effects-set! state #t)
                  (parse-ternary-expr state)
                  (arith-state-suppress-effects-set! state saved))
                then-val)
              (begin
                (let ([saved (arith-state-suppress-effects state)])
                  (arith-state-suppress-effects-set! state #t)
                  (parse-comma-expr state)
                  (arith-state-suppress-effects-set! state saved))
                (arith-expect-op! state ":")
                (parse-ternary-expr state)))
          cond-val)))
  (define (parse-logical-or state)
    (let loop ([result (parse-logical-and state)])
      (if (arith-consume-op! state "||")
          (if (not (= result 0))
              (let ([saved (arith-state-suppress-effects state)])
                (arith-state-suppress-effects-set! state #t)
                (parse-logical-and state)
                (arith-state-suppress-effects-set! state saved)
                (loop 1))
              (let ([rhs (parse-logical-and state)])
                (loop (if (not (= rhs 0)) 1 0))))
          result)))
  (define (parse-logical-and state)
    (let loop ([result (parse-bitwise-or state)])
      (if (arith-consume-op! state "&&")
          (if (= result 0)
              (let ([saved (arith-state-suppress-effects state)])
                (arith-state-suppress-effects-set! state #t)
                (parse-bitwise-or state)
                (arith-state-suppress-effects-set! state saved)
                (loop 0))
              (let ([rhs (parse-bitwise-or state)])
                (loop (if (not (= rhs 0)) 1 0))))
          result)))
  (define (parse-bitwise-or state)
    (let loop ([result (parse-bitwise-xor state)])
      (if (arith-consume-op! state "|")
          (loop (bitwise-ior result (parse-bitwise-xor state)))
          result)))
  (define (parse-bitwise-xor state)
    (let loop ([result (parse-bitwise-and state)])
      (if (arith-consume-op! state "^")
          (loop (bitwise-xor result (parse-bitwise-and state)))
          result)))
  (define (parse-bitwise-and state)
    (let loop ([result (parse-equality state)])
      (if (arith-consume-op! state "&")
          (loop (bitwise-and result (parse-equality state)))
          result)))
  (define (parse-equality state)
    (let loop ([result (parse-comparison state)])
      (cond
        [(arith-consume-op! state "==")
         (loop (if (= result (parse-comparison state)) 1 0))]
        [(arith-consume-op! state "!=")
         (loop (if (not (= result (parse-comparison state))) 1 0))]
        [else result])))
  (define (parse-comparison state)
    (let loop ([result (parse-shift state)])
      (cond
        [(arith-consume-op! state "<=")
         (loop (if (<= result (parse-shift state)) 1 0))]
        [(arith-consume-op! state ">=")
         (loop (if (>= result (parse-shift state)) 1 0))]
        [(arith-consume-op! state "<")
         (loop (if (< result (parse-shift state)) 1 0))]
        [(arith-consume-op! state ">")
         (loop (if (> result (parse-shift state)) 1 0))]
        [else result])))
  (define (parse-shift state)
    (let loop ([result (parse-additive state)])
      (cond
        [(arith-consume-op! state "<<")
         (let* ([amt (parse-additive state)])
           (let* ([effective-amt (bitwise-and amt 63)])
             (loop
               (arith-truncate-64
                 (arithmetic-shift result effective-amt)))))]
        [(arith-consume-op! state ">>")
         (let* ([amt (parse-additive state)])
           (let* ([effective-amt (bitwise-and amt 63)])
             (loop
               (arith-truncate-64
                 (arithmetic-shift
                   (arith-to-signed-64 result)
                   (- effective-amt))))))]
        [else result])))
  (define (arith-truncate-64 n)
    (let ([masked (bitwise-and n 18446744073709551615)])
      (if (> masked 9223372036854775807)
          (- masked 18446744073709551616)
          masked)))
  (define (arith-to-signed-64 n)
    (let ([masked (bitwise-and n 18446744073709551615)])
      (if (> masked 9223372036854775807)
          (- masked 18446744073709551616)
          masked)))
  (define (parse-additive state)
    (let loop ([result (parse-multiplicative state)])
      (cond
        [(arith-consume-op! state "+")
         (loop (+ result (parse-multiplicative state)))]
        [(arith-consume-op! state "-")
         (loop (- result (parse-multiplicative state)))]
        [else result])))
  (define (parse-multiplicative state)
    (let loop ([result (parse-exponent state)])
      (cond
        [(arith-consume-op! state "*")
         (loop (* result (parse-exponent state)))]
        [(arith-consume-op! state "/")
         (let ([divisor (parse-exponent state)])
           (when (= divisor 0)
             (error 'gerbil "arithmetic: division by zero"))
           (loop (truncate-quotient result divisor)))]
        [(arith-consume-op! state "%")
         (let ([divisor (parse-exponent state)])
           (when (= divisor 0)
             (error 'gerbil "arithmetic: division by zero"))
           (loop (truncate-remainder result divisor)))]
        [else result])))
  (define (parse-exponent state)
    (let ([base (parse-unary state)])
      (if (arith-consume-op! state "**")
          (let ([exp (parse-exponent state)])
            (when (< exp 0)
              (error 'gerbil "arithmetic: exponent less than 0"))
            (expt base exp))
          base)))
  (define (parse-unary state)
    (cond
      [(arith-consume-op! state "!")
       (if (= (parse-unary state) 0) 1 0)]
      [(arith-consume-op! state "~")
       (bitwise-not (parse-unary state))]
      [(arith-consume-op! state "-") (- (parse-unary state))]
      [(arith-consume-op! state "+") (parse-unary state)]
      [(arith-consume-op! state "++")
       (let ([tok (arith-peek state)])
         (if (and tok (eq? (arith-token-type tok) 'name))
             (let* ([name (arith-token-value tok)])
               (arith-advance! state)
               (let-values ([(target is-array?)
                             (parse-array-index state name)])
                 (let ([resolved (if is-array?
                                     target
                                     (arith-resolve-name state target))])
                   (arith-set-var!
                     state
                     resolved
                     (+ (arith-get-var state resolved) 1)))))
             (error 'gerbil "arithmetic: ++ requires variable")))]
      [(arith-consume-op! state "--")
       (let ([tok (arith-peek state)])
         (if (and tok (eq? (arith-token-type tok) 'name))
             (let* ([name (arith-token-value tok)])
               (arith-advance! state)
               (let-values ([(target is-array?)
                             (parse-array-index state name)])
                 (let ([resolved (if is-array?
                                     target
                                     (arith-resolve-name state target))])
                   (arith-set-var!
                     state
                     resolved
                     (- (arith-get-var state resolved) 1)))))
             (error 'gerbil "arithmetic: -- requires variable")))]
      [else (parse-postfix state)]))
  (define (parse-postfix state)
    (let ([val (parse-primary state)]) val))
  (define (parse-primary state)
    (let ([tok (arith-peek state)])
      (cond
        [(not tok)
         (error 'gerbil "arithmetic: unexpected end of expression")]
        [(eq? (arith-token-type tok) 'number)
         (arith-advance! state)
         (arith-token-value tok)]
        [(eq? (arith-token-type tok) 'name)
         (arith-advance! state)
         (let ([name (arith-token-value tok)])
           (let-values ([(effective-name is-array?)
                         (parse-array-index state name)])
             (cond
               [(arith-consume-op! state "++")
                (let* ([target (if is-array?
                                   effective-name
                                   (arith-resolve-name
                                     state
                                     effective-name))])
                  (let* ([val (arith-get-var state target)])
                    (arith-set-var! state target (+ val 1))
                    val))]
               [(arith-consume-op! state "--")
                (let* ([target (if is-array?
                                   effective-name
                                   (arith-resolve-name
                                     state
                                     effective-name))])
                  (let* ([val (arith-get-var state target)])
                    (arith-set-var! state target (- val 1))
                    val))]
               [else (arith-get-var state effective-name)])))]
        [(and (eq? (arith-token-type tok) 'op)
              (string=? (arith-token-value tok) "("))
         (arith-advance! state)
         (let ([result (parse-comma-expr state)])
           (arith-expect-op! state ")")
           result)]
        [else
         (error 'gerbil
           (format
             "arithmetic: unexpected token ~a"
             (arith-token-value tok)))]))))
