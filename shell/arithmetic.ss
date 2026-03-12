;;; arithmetic.ss — Shell arithmetic evaluation for gsh
;;; Implements $(( )) arithmetic with full operator precedence

(export arith-eval arith-tokenize arith-token? arith-token-type arith-token-value
        arith-state? arith-state-tokens arith-state-pos arith-state-env-get arith-state-env-set
        arith-state-suppress-effects arith-state-nounset?)
(import :std/sugar
        :std/format)

;;; --- Public interface ---

;; Evaluate a shell arithmetic expression string
;; env-get-fn: (lambda (name) value-string-or-#f)
;; env-set-fn: (lambda (name value) void)
;; nounset?: when #t, referencing undefined variables is an error
;; Returns: integer result
(def (arith-eval expr env-get-fn env-set-fn (nounset? #f))
  (let* ((tokens (arith-tokenize expr))
         (state (make-arith-state tokens 0 env-get-fn env-set-fn #f nounset?)))
    (if (null? tokens)
      0
      (let ((result (parse-comma-expr state)))
        ;; Validate all tokens were consumed
        (when (< (arith-state-pos state) (length tokens))
          (error (format "arithmetic: syntax error: unexpected token '~a'"
                         (arith-token-value (list-ref tokens (arith-state-pos state))))))
        result))))

;;; --- Tokenizer ---

;; Added suppress-effects for short-circuit evaluation, nounset? for set -u
(defstruct arith-state (tokens pos env-get env-set suppress-effects nounset?) transparent: #t)

;; Arith token types: 'number 'name 'op
(defstruct arith-token (type value) transparent: #t)

(def (arith-tokenize expr)
  (let loop ((i 0) (tokens []))
    (cond
      ((>= i (string-length expr))
       (reverse tokens))
      ;; Skip whitespace
      ((char-whitespace? (string-ref expr i))
       (loop (+ i 1) tokens))
      ;; Numbers: decimal, hex (0x), octal (0), binary (0b), base-N (N#val)
      ((char-numeric? (string-ref expr i))
       (let-values (((num end) (read-number expr i)))
         (loop end (cons (make-arith-token 'number num) tokens))))
      ;; Names (variable references) — may also start base-N constant
      ((or (char-alphabetic? (string-ref expr i))
           (char=? (string-ref expr i) #\_))
       (let-values (((name end) (read-name expr i)))
         (loop end (cons (make-arith-token 'name name) tokens))))
      ;; # is not a valid arithmetic operator — reject it early to prevent
      ;; side-effects from being committed before the syntax error is detected
      ((char=? (string-ref expr i) #\#)
       (error (format "arithmetic: syntax error: unexpected token '#'")))
      ;; Multi-char operators
      (else
       (let-values (((op end) (read-operator expr i)))
         (loop end (cons (make-arith-token 'op op) tokens)))))))

(def (read-number expr i)
  (let ((len (string-length expr)))
    (cond
      ;; Hex: 0x or 0X
      ((and (< (+ i 1) len)
            (char=? (string-ref expr i) #\0)
            (or (char=? (string-ref expr (+ i 1)) #\x)
                (char=? (string-ref expr (+ i 1)) #\X)))
       (let loop ((j (+ i 2)))
         (if (and (< j len) (hex-digit? (string-ref expr j)))
           (loop (+ j 1))
           ;; Check for trailing alphanumeric (invalid hex like 0x1X)
           (begin
             (when (and (< j len)
                        (let ((ch (string-ref expr j)))
                          (or (char-alphabetic? ch) (char=? ch #\_))))
               (error (format "arithmetic: invalid hex constant: ~a"
                              (substring expr i (let lp ((k j))
                                                  (if (and (< k len)
                                                           (let ((c (string-ref expr k)))
                                                             (or (arith-alnum? c) (char=? c #\_))))
                                                    (lp (+ k 1)) k))))))
             (let ((num (string->number (substring expr (+ i 2) j) 16)))
               (if num (values num j)
                 (error (format "arithmetic: invalid hex constant: ~a"
                                (substring expr i j)))))))))
      ;; Binary: 0b or 0B
      ((and (< (+ i 1) len)
            (char=? (string-ref expr i) #\0)
            (or (char=? (string-ref expr (+ i 1)) #\b)
                (char=? (string-ref expr (+ i 1)) #\B)))
       (let loop ((j (+ i 2)))
         (if (and (< j len) (or (char=? (string-ref expr j) #\0)
                                 (char=? (string-ref expr j) #\1)))
           (loop (+ j 1))
           (values (string->number (substring expr (+ i 2) j) 2) j))))
      ;; Octal: starts with 0 followed by digit
      ((and (char=? (string-ref expr i) #\0)
            (< (+ i 1) len)
            (char-numeric? (string-ref expr (+ i 1))))
       ;; Read all consecutive digits first, then validate
       (let loop ((j (+ i 1)))
         (if (and (< j len) (char-numeric? (string-ref expr j)))
           (loop (+ j 1))
           ;; Check that all digits are valid octal (0-7)
           (let check ((k (+ i 1)))
             (if (< k j)
               (if (octal-digit? (string-ref expr k))
                 (check (+ k 1))
                 (error (format "arithmetic: invalid octal constant: ~a"
                                (substring expr i j))))
               ;; Check for trailing # (invalid base-N with leading 0: 02#xxx)
               (if (and (< j len) (char=? (string-ref expr j) #\#))
                 (let ((end (let lp ((k (+ j 1)))
                              (if (and (< k len) (arith-alnum? (string-ref expr k)))
                                (lp (+ k 1)) k))))
                   (error (format "arithmetic: invalid number: ~a"
                                  (substring expr i end))))
                 (let ((num (string->number (substring expr (+ i 1) j) 8)))
                   (if num (values num j)
                     (error (format "arithmetic: invalid octal constant: ~a"
                                    (substring expr i j)))))))))))
      ;; Decimal — possibly followed by #val for base-N
      (else
       (let loop ((j i))
         (if (and (< j len) (char-numeric? (string-ref expr j)))
           (loop (+ j 1))
           (cond
             ;; Float literal: reject
             ((and (< j len) (char=? (string-ref expr j) #\.))
              (error "arithmetic: invalid number (float not supported)"))
             ;; Check for base#value pattern
             ((and (< j len) (char=? (string-ref expr j) #\#))
              (let ((base (string->number (substring expr i j))))
                (if (and base (>= base 2) (<= base 64))
                  ;; Read the value part (alphanumeric + _ + @)
                  (let vloop ((k (+ j 1)) (val 0))
                    (if (and (< k len) (base-n-digit? (string-ref expr k) base))
                      (vloop (+ k 1) (+ (* val base) (base-n-digit-value (string-ref expr k))))
                      ;; Check for trailing invalid digit (e.g. 2#A)
                      (if (and (< k len)
                               (let ((ch (string-ref expr k)))
                                 (or (arith-alnum? ch) (char=? ch #\_) (char=? ch #\@))))
                        (error (format "arithmetic: invalid base ~a constant: ~a"
                                       base (substring expr i (let lp ((kk k))
                                                                (if (and (< kk len)
                                                                         (let ((c (string-ref expr kk)))
                                                                           (or (arith-alnum? c) (char=? c #\_) (char=? c #\@))))
                                                                  (lp (+ kk 1)) kk)))))
                        (if (= k (+ j 1))
                          ;; No digits at all after base# — error
                          (error (format "arithmetic: invalid constant: ~a"
                                         (substring expr i (+ j 1))))
                          (values val k)))))
                  (error (format "arithmetic: invalid base: ~a"
                                 (substring expr i j))))))
             ;; Check for trailing alphabetic chars (e.g. 42x) — invalid constant
             ((and (< j len)
                   (let ((ch (string-ref expr j)))
                     (or (char-alphabetic? ch) (char=? ch #\_))))
              ;; Read the full invalid token
              (let lp ((k j))
                (if (and (< k len)
                         (let ((ch (string-ref expr k)))
                           (or (arith-alnum? ch) (char=? ch #\_))))
                  (lp (+ k 1))
                  (error (format "arithmetic: invalid constant: ~a"
                                 (substring expr i k))))))
             (else
              (values (string->number (substring expr i j)) j)))))))))

(def (read-name expr i)
  (let ((len (string-length expr)))
    (let loop ((j i))
      (if (and (< j len)
               (let ((ch (string-ref expr j)))
                 (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_))))
        (loop (+ j 1))
        (values (substring expr i j) j)))))

(def (read-operator expr i)
  (let ((len (string-length expr))
        (ch (string-ref expr i)))
    (cond
      ;; Three-char operators
      ((and (< (+ i 2) len)
            (string=? (substring expr i (+ i 3)) "<<="))
       (values "<<=" (+ i 3)))
      ((and (< (+ i 2) len)
            (string=? (substring expr i (+ i 3)) ">>="))
       (values ">>=" (+ i 3)))
      ;; Two-char operators
      ((< (+ i 1) len)
       (let ((two (substring expr i (+ i 2))))
         (cond
           ((member two '("==" "!=" "<=" ">=" "&&" "||" "<<" ">>"
                          "+=" "-=" "*=" "/=" "%=" "&=" "^=" "|="
                          "++" "--" "**"))
            (values two (+ i 2)))
           (else
            (values (string ch) (+ i 1))))))
      (else
       (values (string ch) (+ i 1))))))

(def (arith-alnum? ch)
  (or (char-alphabetic? ch) (char-numeric? ch)))

(def (hex-digit? ch)
  (or (char-numeric? ch)
      (and (char>=? ch #\a) (char<=? ch #\f))
      (and (char>=? ch #\A) (char<=? ch #\F))))

(def (octal-digit? ch)
  (and (char>=? ch #\0) (char<=? ch #\7)))

;; Check if a character is a valid digit for base N
;; Bases 2-36: 0-9 a-z (case insensitive)
;; Bases 37-62: 0-9 a-z A-Z
;; Bases 63-64: 0-9 a-z A-Z _ @
(def (base-n-digit? ch base)
  (let ((v (base-n-digit-value-raw ch)))
    (and v (< v base))))

(def (base-n-digit-value ch)
  (or (base-n-digit-value-raw ch) 0))

(def (base-n-digit-value-raw ch)
  (cond
    ((and (char>=? ch #\0) (char<=? ch #\9))
     (- (char->integer ch) (char->integer #\0)))
    ((and (char>=? ch #\a) (char<=? ch #\z))
     (+ 10 (- (char->integer ch) (char->integer #\a))))
    ((and (char>=? ch #\A) (char<=? ch #\Z))
     (+ 36 (- (char->integer ch) (char->integer #\A))))
    ((char=? ch #\@) 62)
    ((char=? ch #\_) 63)
    (else #f)))

;;; --- Recursive descent parser (operator precedence) ---

(def (arith-peek state)
  (if (>= (arith-state-pos state) (length (arith-state-tokens state)))
    #f
    (list-ref (arith-state-tokens state) (arith-state-pos state))))

(def (arith-advance! state)
  (set! (arith-state-pos state) (+ 1 (arith-state-pos state))))

(def (arith-expect-op! state expected)
  (let ((tok (arith-peek state)))
    (if (and tok (eq? (arith-token-type tok) 'op)
             (string=? (arith-token-value tok) expected))
      (begin (arith-advance! state) #t)
      (error (format "arithmetic: expected ~a" expected)))))

(def (arith-match-op? state op)
  (let ((tok (arith-peek state)))
    (and tok (eq? (arith-token-type tok) 'op)
         (string=? (arith-token-value tok) op))))

(def (arith-consume-op! state op)
  (if (arith-match-op? state op)
    (begin (arith-advance! state) #t)
    #f))

;; Get variable value as integer, with recursive name resolution
;; If the value of a variable is another variable name, resolve it
;; For dynamic arithmetic resolution: e=1+2; echo $((e+3)) → 6
(def (arith-get-var state name)
  (let resolve ((name name) (depth 0))
    (if (> depth 10) 0  ;; prevent infinite loops
      (let ((val ((arith-state-env-get state) name)))
        (cond
          ((not val)
           ;; Check nounset — if nounset? is a procedure, call it (raises exception)
           ;; If it's #t, raise generic error. If #f, silently return 0.
           (let ((nu (arith-state-nounset? state)))
             (cond
               ((procedure? nu) (nu name))
               (nu (error (format "arithmetic: ~a: unbound variable" name)))))
           0)
          (else
           (let ((trimmed (string-trim val)))
             (if (string=? trimmed "") 0
               (let ((num (string->number trimmed)))
                 (if num num
                   ;; Try to resolve as an arithmetic expression
                   ;; First check if it's a plain variable name
                   (if (and (> (string-length trimmed) 0)
                            (or (char-alphabetic? (string-ref trimmed 0))
                                (char=? (string-ref trimmed 0) #\_))
                            (let check ((j 0))
                              (or (>= j (string-length trimmed))
                                  (let ((ch (string-ref trimmed j)))
                                    (and (or (char-alphabetic? ch) (char-numeric? ch)
                                             (char=? ch #\_))
                                         (check (+ j 1)))))))
                     ;; Plain variable name — resolve through variable chain
                     (resolve trimmed (+ depth 1))
                     ;; Contains operators — evaluate as arithmetic expression
                     (arith-eval trimmed
                                 (arith-state-env-get state)
                                 (arith-state-env-set state)))))))))))))

(def (string-trim s)
  (let* ((len (string-length s))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref s i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i len))
                (if (and (> i start) (char-whitespace? (string-ref s (- i 1))))
                  (loop (- i 1)) i))))
    (substring s start end)))

;; Set variable value (respects suppress-effects for short-circuit)
(def (arith-set-var! state name value)
  (unless (arith-state-suppress-effects state)
    ((arith-state-env-set state) name (number->string value)))
  value)

;; Resolve name through dynamic variable references (for assignment targets)
(def (arith-resolve-name state name)
  (let resolve ((name name) (depth 0))
    (if (> depth 10) name
      (let ((val ((arith-state-env-get state) name)))
        (if (not val) name
          (let ((num (string->number val)))
            (if num name  ;; value is numeric, stop
              ;; Value looks like a variable name, follow it
              (if (and (> (string-length val) 0)
                       (or (char-alphabetic? (string-ref val 0))
                           (char=? (string-ref val 0) #\_))
                       (let check ((j 0))
                         (or (>= j (string-length val))
                             (let ((ch (string-ref val j)))
                               (and (or (char-alphabetic? ch) (char-numeric? ch)
                                        (char=? ch #\_))
                                    (check (+ j 1)))))))
                (resolve val (+ depth 1))
                name))))))))

;;; --- Precedence levels (lowest to highest) ---

;; Level 1: Comma (sequence)
(def (parse-comma-expr state)
  (let loop ((result (parse-assignment-expr state)))
    (if (arith-consume-op! state ",")
      (loop (parse-assignment-expr state))
      result)))

;; Level 2: Assignment
(def (parse-assignment-expr state)
  (let ((tok (arith-peek state)))
    (if (and tok (eq? (arith-token-type tok) 'name))
      ;; Peek ahead for assignment operator (possibly past array index)
      (let* ((name (arith-token-value tok))
             (saved-pos (arith-state-pos state)))
        (arith-advance! state)
        ;; Check for array indexing: name[expr]
        (let-values (((final-name is-array?) (parse-array-index state name)))
          (let ((op-tok (arith-peek state)))
            (if (and op-tok (eq? (arith-token-type op-tok) 'op)
                     (member (arith-token-value op-tok)
                             '("=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>=" "&=" "^=" "|=")))
              (let ((op (arith-token-value op-tok))
                    ;; For dynamic names, resolve to final target
                    (target (if is-array? final-name (arith-resolve-name state final-name))))
                (arith-advance! state)
                (let ((rhs (parse-assignment-expr state)))
                  (cond
                    ((string=? op "=") (arith-set-var! state target rhs))
                    ((string=? op "+=") (arith-set-var! state target (+ (arith-get-var state target) rhs)))
                    ((string=? op "-=") (arith-set-var! state target (- (arith-get-var state target) rhs)))
                    ((string=? op "*=") (arith-set-var! state target (* (arith-get-var state target) rhs)))
                    ((string=? op "/=") (arith-set-var! state target (truncate-quotient (arith-get-var state target) rhs)))
                    ((string=? op "%=") (arith-set-var! state target (truncate-remainder (arith-get-var state target) rhs)))
                    ((string=? op "<<=") (arith-set-var! state target (arithmetic-shift (arith-get-var state target) rhs)))
                    ((string=? op ">>=") (arith-set-var! state target (arithmetic-shift (arith-get-var state target) (- rhs))))
                    ((string=? op "&=") (arith-set-var! state target (bitwise-and (arith-get-var state target) rhs)))
                    ((string=? op "^=") (arith-set-var! state target (bitwise-xor (arith-get-var state target) rhs)))
                    ((string=? op "|=") (arith-set-var! state target (bitwise-ior (arith-get-var state target) rhs)))
                    (else (error "unknown assignment op" op)))))
              ;; Not an assignment, backtrack
              (begin
                (set! (arith-state-pos state) saved-pos)
                (parse-ternary-expr state))))))
      (parse-ternary-expr state))))

;; Parse optional array index after a name: name[expr]
;; Returns (values effective-name is-array?)
(def (parse-array-index state name)
  (if (arith-consume-op! state "[")
    (let ((idx (parse-comma-expr state)))
      (arith-expect-op! state "]")
      (values (string-append name "[" (number->string idx) "]") #t))
    (values name #f)))

;; Level 3: Ternary ?:
(def (parse-ternary-expr state)
  (let ((cond-val (parse-logical-or state)))
    (if (arith-consume-op! state "?")
      (if (not (= cond-val 0))
        ;; Condition true: evaluate then-branch, suppress else-branch
        (let ((then-val (parse-comma-expr state)))
          (arith-expect-op! state ":")
          (let ((saved (arith-state-suppress-effects state)))
            (set! (arith-state-suppress-effects state) #t)
            (parse-ternary-expr state)
            (set! (arith-state-suppress-effects state) saved))
          then-val)
        ;; Condition false: suppress then-branch, evaluate else-branch
        (begin
          (let ((saved (arith-state-suppress-effects state)))
            (set! (arith-state-suppress-effects state) #t)
            (parse-comma-expr state)
            (set! (arith-state-suppress-effects state) saved))
          (arith-expect-op! state ":")
          (parse-ternary-expr state)))
      cond-val)))

;; Level 4: Logical OR || (short-circuit)
(def (parse-logical-or state)
  (let loop ((result (parse-logical-and state)))
    (if (arith-consume-op! state "||")
      (if (not (= result 0))
        ;; Short-circuit: parse but suppress side effects on RHS
        (let ((saved (arith-state-suppress-effects state)))
          (set! (arith-state-suppress-effects state) #t)
          (parse-logical-and state)
          (set! (arith-state-suppress-effects state) saved)
          (loop 1))
        (let ((rhs (parse-logical-and state)))
          (loop (if (not (= rhs 0)) 1 0))))
      result)))

;; Level 5: Logical AND && (short-circuit)
(def (parse-logical-and state)
  (let loop ((result (parse-bitwise-or state)))
    (if (arith-consume-op! state "&&")
      (if (= result 0)
        ;; Short-circuit: parse but suppress side effects on RHS
        (let ((saved (arith-state-suppress-effects state)))
          (set! (arith-state-suppress-effects state) #t)
          (parse-bitwise-or state)
          (set! (arith-state-suppress-effects state) saved)
          (loop 0))
        (let ((rhs (parse-bitwise-or state)))
          (loop (if (not (= rhs 0)) 1 0))))
      result)))

;; Level 6: Bitwise OR |
(def (parse-bitwise-or state)
  (let loop ((result (parse-bitwise-xor state)))
    (if (arith-consume-op! state "|")
      (loop (bitwise-ior result (parse-bitwise-xor state)))
      result)))

;; Level 7: Bitwise XOR ^
(def (parse-bitwise-xor state)
  (let loop ((result (parse-bitwise-and state)))
    (if (arith-consume-op! state "^")
      (loop (bitwise-xor result (parse-bitwise-and state)))
      result)))

;; Level 8: Bitwise AND &
(def (parse-bitwise-and state)
  (let loop ((result (parse-equality state)))
    (if (arith-consume-op! state "&")
      (loop (bitwise-and result (parse-equality state)))
      result)))

;; Level 9: Equality == !=
(def (parse-equality state)
  (let loop ((result (parse-comparison state)))
    (cond
      ((arith-consume-op! state "==")
       (loop (if (= result (parse-comparison state)) 1 0)))
      ((arith-consume-op! state "!=")
       (loop (if (not (= result (parse-comparison state))) 1 0)))
      (else result))))

;; Level 10: Comparison < <= > >=
(def (parse-comparison state)
  (let loop ((result (parse-shift state)))
    (cond
      ((arith-consume-op! state "<=")
       (loop (if (<= result (parse-shift state)) 1 0)))
      ((arith-consume-op! state ">=")
       (loop (if (>= result (parse-shift state)) 1 0)))
      ((arith-consume-op! state "<")
       (loop (if (< result (parse-shift state)) 1 0)))
      ((arith-consume-op! state ">")
       (loop (if (> result (parse-shift state)) 1 0)))
      (else result))))

;; Level 11: Bit shift << >>
;; Bash allows negative shifts (implementation-defined behavior)
;; We emulate 64-bit C behavior: mask shift amount to 0-63
(def (parse-shift state)
  (let loop ((result (parse-additive state)))
    (cond
      ((arith-consume-op! state "<<")
       (let* ((amt (parse-additive state))
              ;; Emulate 64-bit C: mask to 6 bits (0-63)
              (effective-amt (bitwise-and amt 63)))
         (loop (arith-truncate-64 (arithmetic-shift result effective-amt)))))
      ((arith-consume-op! state ">>")
       (let* ((amt (parse-additive state))
              (effective-amt (bitwise-and amt 63)))
         (loop (arith-truncate-64 (arithmetic-shift (arith-to-signed-64 result) (- effective-amt))))))
      (else result))))

;; Truncate to 64-bit signed integer range (emulate C int64_t)
(def (arith-truncate-64 n)
  (let ((masked (bitwise-and n #xFFFFFFFFFFFFFFFF)))
    (if (> masked #x7FFFFFFFFFFFFFFF)
      (- masked #x10000000000000000)
      masked)))

;; Convert to 64-bit signed representation
(def (arith-to-signed-64 n)
  (let ((masked (bitwise-and n #xFFFFFFFFFFFFFFFF)))
    (if (> masked #x7FFFFFFFFFFFFFFF)
      (- masked #x10000000000000000)
      masked)))

;; Level 12: Addition + -
(def (parse-additive state)
  (let loop ((result (parse-multiplicative state)))
    (cond
      ((arith-consume-op! state "+")
       (loop (+ result (parse-multiplicative state))))
      ((arith-consume-op! state "-")
       (loop (- result (parse-multiplicative state))))
      (else result))))

;; Level 13: Multiplication * / %
;; Use truncate-quotient and truncate-remainder for C-style semantics
(def (parse-multiplicative state)
  (let loop ((result (parse-exponent state)))
    (cond
      ((arith-consume-op! state "*")
       (loop (* result (parse-exponent state))))
      ((arith-consume-op! state "/")
       (let ((divisor (parse-exponent state)))
         (when (= divisor 0) (error "arithmetic: division by zero"))
         (loop (truncate-quotient result divisor))))
      ((arith-consume-op! state "%")
       (let ((divisor (parse-exponent state)))
         (when (= divisor 0) (error "arithmetic: division by zero"))
         (loop (truncate-remainder result divisor))))
      (else result))))

;; Level 14: Exponentiation ** (right-associative)
(def (parse-exponent state)
  (let ((base (parse-unary state)))
    (if (arith-consume-op! state "**")
      (let ((exp (parse-exponent state)))  ;; right-associative
        (when (< exp 0) (error "arithmetic: exponent less than 0"))
        (expt base exp))
      base)))

;; Level 15: Unary ! ~ + - (prefix)
(def (parse-unary state)
  (cond
    ((arith-consume-op! state "!")
     (if (= (parse-unary state) 0) 1 0))
    ((arith-consume-op! state "~")
     (bitwise-not (parse-unary state)))
    ((arith-consume-op! state "-")
     (- (parse-unary state)))
    ((arith-consume-op! state "+")
     (parse-unary state))
    ;; Pre-increment/decrement
    ((arith-consume-op! state "++")
     (let ((tok (arith-peek state)))
       (if (and tok (eq? (arith-token-type tok) 'name))
         (let* ((name (arith-token-value tok)))
           (arith-advance! state)
           (let-values (((target is-array?) (parse-array-index state name)))
             (let ((resolved (if is-array? target (arith-resolve-name state target))))
               (arith-set-var! state resolved (+ (arith-get-var state resolved) 1)))))
         (error "arithmetic: ++ requires variable"))))
    ((arith-consume-op! state "--")
     (let ((tok (arith-peek state)))
       (if (and tok (eq? (arith-token-type tok) 'name))
         (let* ((name (arith-token-value tok)))
           (arith-advance! state)
           (let-values (((target is-array?) (parse-array-index state name)))
             (let ((resolved (if is-array? target (arith-resolve-name state target))))
               (arith-set-var! state resolved (- (arith-get-var state resolved) 1)))))
         (error "arithmetic: -- requires variable"))))
    (else (parse-postfix state))))

;; Level 16: Postfix ++ --
(def (parse-postfix state)
  (let ((val (parse-primary state)))
    ;; Check for postfix ++ or -- (only valid after a name)
    val))

;; Primary: number, variable (with optional array index), or (expr)
(def (parse-primary state)
  (let ((tok (arith-peek state)))
    (cond
      ((not tok) (error "arithmetic: unexpected end of expression"))
      ((eq? (arith-token-type tok) 'number)
       (arith-advance! state)
       (arith-token-value tok))
      ((eq? (arith-token-type tok) 'name)
       (arith-advance! state)
       (let ((name (arith-token-value tok)))
         ;; Check for array indexing: name[expr]
         (let-values (((effective-name is-array?) (parse-array-index state name)))
           ;; Check for postfix ++ --
           (cond
             ((arith-consume-op! state "++")
              (let* ((target (if is-array? effective-name (arith-resolve-name state effective-name)))
                     (val (arith-get-var state target)))
                (arith-set-var! state target (+ val 1))
                val))  ;; return old value
             ((arith-consume-op! state "--")
              (let* ((target (if is-array? effective-name (arith-resolve-name state effective-name)))
                     (val (arith-get-var state target)))
                (arith-set-var! state target (- val 1))
                val))  ;; return old value
             (else (arith-get-var state effective-name))))))
      ((and (eq? (arith-token-type tok) 'op)
            (string=? (arith-token-value tok) "("))
       (arith-advance! state)
       (let ((result (parse-comma-expr state)))
         (arith-expect-op! state ")")
         result))
      (else
       (error (format "arithmetic: unexpected token ~a" (arith-token-value tok)))))))
