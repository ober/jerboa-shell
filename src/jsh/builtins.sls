#!chezscheme
(library (jsh builtins)
  (export *execute-external-fn* fd-read-char
   port-or-fd-read-char strip-trailing-slash same-directory?
   resolve-logical-path search-cdpath shell-quote-value
   shell-keyword? special-builtin? read-strip-backslashes
   read-ifs-split-raw read-ifs-split ifs-whitespace-chars
   ifs-non-whitespace-chars ifs-ws-member? ifs-nw-member?
   read-ifs-split-from read-ifs-split-bs strip-trailing-ifs-ws
   strip-trailing-ifs-for-read parse-array-compound-elements
   parse-array-compound-raw builtin-declare collect-all-vars
   declare-print-all-vars declare-print-all-vars-simple
   declare-var-flags array-dense? declare-quote-value
   declare-quote-scalar display-declare-var return-from-declare
   *ulimit-resources* builtin-mapfile list-tail* test-eval
   test-eval-or test-eval-and test-find-close-paren
   test-eval-not test-eval-primary test-unary *test-var-fn*
   *test-option-fn* test-parse-integer test-string-trim
   test-binary test-int-cmp display-raw-bytes
   echo-expand-escapes shell-printf printf-format-once
   printf-format-spec parse-printf-number
   u8vector->string-lossy hex-digit-value display-unicode-char
   printf-escape printf-interpret-b-escapes pad-string
   *printf-conversion-error* *printf-stop* string->integer-safe
   parse-leading-integer string->number-safe format-float
   string-contains-char? ensure-decimal-point
   strip-trailing-zeros format-fixed format-scientific
   shell-quote-string apply-set-options! split-by-ifs
   ifs-member? string-join-sp string-find-char* list-head
   last-elem* butlast env-exported-alist-pairs
   arith-eval-wrapper)
  (import
   (except (chezscheme) box box? unbox set-box! andmap ormap
    iota last-pair find \x31;+ \x31;- fx/ fx1+ fx1- error? raise
    with-exception-handler identifier? hash-table?
    make-hash-table sort sort! path-extension printf fprintf
    file-directory? file-exists? getenv close-port void
    open-output-file open-input-file list-head)
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
   (std os signal) (except (std os fdio) write-subu8vector)
   (gsh ast) (gsh ffi) (gsh environment) (gsh expander)
   (gsh functions) (gsh jobs) (gsh signals) (gsh history)
   (gsh util) (gsh registry) (gsh macros) (gsh arithmetic))
  (define *execute-external-fn* (make-parameter #f))
  (define (fd-read-char rfd)
    (let ([bv (ffi-fdread rfd 1)])
      (if (= (u8vector-length bv) 0)
          (%%eof)
          (integer->char (u8vector-ref bv 0)))))
  (define (port-or-fd-read-char in-port pipe-fd)
    (if pipe-fd (fd-read-char pipe-fd) (read-char in-port)))
  (define (strip-trailing-slash path)
    (let ([len (string-length path)])
      (if (and (> len 1) (char=? (string-ref path (- len 1)) #\/))
          (substring path 0 (- len 1))
          path)))
  (define (same-directory? path-a path-b)
    (guard (__exn [#t ((lambda (e) #f) __exn)])
      (let ([a (gambit-file-info path-a)]
            [b (gambit-file-info path-b)])
        (and (= (file-info-device a) (file-info-device b))
             (= (file-info-inode a) (file-info-inode b))))))
  (define (resolve-logical-path base target)
    (let ([path (if (and (> (string-length target) 0)
                         (char=? (string-ref target 0) #\/))
                    target
                    (string-append
                      (strip-trailing-slash base)
                      "/"
                      target))])
      (let* ([parts (let ([str path]
                          [sep (if (char? #\/) #\/ (string-ref #\/ 0))])
                      (let split-lp ([i 0] [start 0] [acc '()])
                        (cond
                          [(= i (string-length str))
                           (reverse (cons (substring str start i) acc))]
                          [(char=? (string-ref str i) sep)
                           (split-lp
                             (+ i 1)
                             (+ i 1)
                             (cons (substring str start i) acc))]
                          [else (split-lp (+ i 1) start acc)])))])
        (let* ([resolved (let loop ([ps parts] [acc (list)])
                           (cond
                             [(null? ps) (reverse acc)]
                             [(string=? (car ps) "")
                              (loop
                                (cdr ps)
                                (if (null? acc) (list "") acc))]
                             [(string=? (car ps) ".") (loop (cdr ps) acc)]
                             [(string=? (car ps) "..")
                              (loop
                                (cdr ps)
                                (if (and (pair? acc)
                                         (not (string=? (car acc) "")))
                                    (cdr acc)
                                    acc))]
                             [else (loop (cdr ps) (cons (car ps) acc))]))])
          (let ([result (let ([strs resolved] [sep "/"])
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
                                        (cdr rest))))))])
            (if (string=? result "") "/" result))))))
  (define (search-cdpath target env)
    (let ([cdpath (env-get env "CDPATH")])
      (if (or (not cdpath)
              (string=? cdpath "")
              (and (> (string-length target) 0)
                   (char=? (string-ref target 0) #\/)))
          #f
          (let loop ([dirs (let ([str cdpath]
                                 [sep (if (char? #\:)
                                          #\:
                                          (string-ref #\: 0))])
                             (let split-lp ([i 0] [start 0] [acc '()])
                               (cond
                                 [(= i (string-length str))
                                  (reverse
                                    (cons (substring str start i) acc))]
                                 [(char=? (string-ref str i) sep)
                                  (split-lp
                                    (+ i 1)
                                    (+ i 1)
                                    (cons (substring str start i) acc))]
                                 [else (split-lp (+ i 1) start acc)])))])
            (if (null? dirs)
                #f
                (let* ([base (if (string=? (car dirs) "") "." (car dirs))])
                  (let* ([full (string-append
                                 (strip-trailing-slash base)
                                 "/"
                                 target)])
                    (if (file-exists? full) full (loop (cdr dirs))))))))))
  (define (shell-quote-value val)
    (let* ([needs-dollar-quote? #f])
      (let* ([needs-any-quote? (if (= (string-length val) 0)
                                   #t
                                   (let loop ([i 0] [needs? #f])
                                     (if (>= i (string-length val))
                                         needs?
                                         (let ([ch (string-ref val i)])
                                           (cond
                                             [(or (char<? ch #\space)
                                                  (char=? ch #\delete))
                                              (set! needs-dollar-quote? #t)
                                              (loop (+ i 1) #t)]
                                             [(or (char-alphabetic? ch)
                                                  (char-numeric? ch)
                                                  (memq
                                                    ch
                                                    '(#\_ #\- #\. #\/ #\:
                                                          #\= #\+ #\@ #\%
                                                          #\^ #\~ #\,)))
                                              (loop (+ i 1) needs?)]
                                             [else
                                              (loop (+ i 1) #t)])))))])
        (cond
          [needs-dollar-quote?
           (let ([buf (open-output-string)])
             (display "$'" buf)
             (let loop ([i 0])
               (when (< i (string-length val))
                 (let ([ch (string-ref val i)])
                   (cond
                     [(char=? ch #\newline) (display "\\n" buf)]
                     [(char=? ch #\tab) (display "\\t" buf)]
                     [(char=? ch #\return) (display "\\r" buf)]
                     [(char=? ch #\\) (display "\\\\" buf)]
                     [(char=? ch #\') (display "\\'" buf)]
                     [(or (char<? ch #\space) (char=? ch #\delete))
                      (let ([oct (number->string (char->integer ch) 8)])
                        (display
                          (string-append
                            "\\"
                            (make-string (- 3 (string-length oct)) #\0)
                            oct)
                          buf))]
                     [else (display ch buf)]))
                 (loop (+ i 1))))
             (display "'" buf)
             (get-output-string buf))]
          [needs-any-quote?
           (string-append
             "'"
             (string-replace-all val "'" "'\\''")
             "'")]
          [else val]))))
  (define (shell-keyword? name)
    (member
      name
      '("if" "then" "else" "elif" "fi" "case" "esac" "for" "while"
        "until" "do" "done" "in" "function" "select" "time" "{" "}"
        "!" "[[" "]]" "coproc")))
  (define (special-builtin? name)
    (and (member
           name
           '("." ":" "break" "continue" "eval" "exec" "exit" "export"
                 "readonly" "return" "set" "shift" "trap" "unset"))
         (builtin? name)))
  (define (read-strip-backslashes s)
    (let ([len (string-length s)] [buf (open-output-string)])
      (let loop ([i 0])
        (cond
          [(>= i len) (get-output-string buf)]
          [(and (char=? (string-ref s i) #\\) (< (+ i 1) len))
           (display (string-ref s (+ i 1)) buf)
           (loop (+ i 2))]
          [(and (char=? (string-ref s i) #\\) (= (+ i 1) len))
           (display #\\ buf)
           (loop (+ i 1))]
          [else (display (string-ref s i) buf) (loop (+ i 1))]))))
  (define (read-ifs-split-raw str ifs max-fields)
    (let ([len (string-length str)])
      (if (= len 0)
          (list)
          (let ([ifs-ws (ifs-whitespace-chars ifs)]
                [ifs-nw (ifs-non-whitespace-chars ifs)])
            (let skip-lead ([i 0])
              (if (and (< i len)
                       (ifs-ws-member? (string-ref str i) ifs-ws))
                  (skip-lead (+ i 1))
                  (read-ifs-split-from str i len ifs-ws ifs-nw
                    max-fields)))))))
  (define (read-ifs-split str ifs max-fields)
    (let ([len (string-length str)])
      (if (= len 0)
          (list)
          (let ([ifs-ws (ifs-whitespace-chars ifs)]
                [ifs-nw (ifs-non-whitespace-chars ifs)])
            (let skip-lead ([i 0])
              (if (and (< i len)
                       (not (char=? (string-ref str i) #\\))
                       (ifs-ws-member? (string-ref str i) ifs-ws))
                  (skip-lead (+ i 1))
                  (read-ifs-split-bs str i len ifs-ws ifs-nw
                    max-fields)))))))
  (define (ifs-whitespace-chars ifs)
    (let ([buf (open-output-string)])
      (let loop ([i 0])
        (if (>= i (string-length ifs))
            (get-output-string buf)
            (let ([ch (string-ref ifs i)])
              (when (or (char=? ch #\space)
                        (char=? ch #\tab)
                        (char=? ch #\newline))
                (display ch buf))
              (loop (+ i 1)))))))
  (define (ifs-non-whitespace-chars ifs)
    (let ([buf (open-output-string)])
      (let loop ([i 0])
        (if (>= i (string-length ifs))
            (get-output-string buf)
            (let ([ch (string-ref ifs i)])
              (when (not (or (char=? ch #\space)
                             (char=? ch #\tab)
                             (char=? ch #\newline)))
                (display ch buf))
              (loop (+ i 1)))))))
  (define (ifs-ws-member? ch ws)
    (let loop ([i 0])
      (and (< i (string-length ws))
           (or (char=? ch (string-ref ws i)) (loop (+ i 1))))))
  (define (ifs-nw-member? ch nw)
    (let loop ([i 0])
      (and (< i (string-length nw))
           (or (char=? ch (string-ref nw i)) (loop (+ i 1))))))
  (define (read-ifs-split-from str start len ifs-ws ifs-nw
           max-fields)
    (let ([buf (open-output-string)]
          [fields (list)]
          [field-count 1])
      (let loop ([i start])
        (cond
          [(>= i len)
           (let* ([s (get-output-string buf)])
             (let* ([s (if (> max-fields 0)
                           (strip-trailing-ifs-ws s ifs-ws)
                           (strip-trailing-ifs-for-read s ifs-ws ifs-nw))])
               (reverse
                 (cond
                   [(> (string-length s) 0) (cons s fields)]
                   [(pair? fields)
                    (if (> max-fields 0) (cons s fields) fields)]
                   [else fields]))))]
          [(ifs-nw-member? (string-ref str i) ifs-nw)
           (if (and (> max-fields 0) (>= field-count max-fields))
               (begin (display (string-ref str i) buf) (loop (+ i 1)))
               (let ([s (get-output-string buf)])
                 (set! fields
                   (cons (strip-trailing-ifs-ws s ifs-ws) fields))
                 (set! buf (open-output-string))
                 (set! field-count (+ field-count 1))
                 (let skip ([j (+ i 1)])
                   (if (and (< j len)
                            (ifs-ws-member? (string-ref str j) ifs-ws))
                       (skip (+ j 1))
                       (if (and (> max-fields 0)
                                (>= field-count max-fields))
                           (begin
                             (display (substring str j len) buf)
                             (let* ([s (get-output-string buf)])
                               (let* ([s (strip-trailing-ifs-for-read
                                           s
                                           ifs-ws
                                           ifs-nw)])
                                 (reverse (cons s fields)))))
                           (loop j))))))]
          [(ifs-ws-member? (string-ref str i) ifs-ws)
           (let ([buf-content (get-output-string buf)])
             (if (and (> max-fields 0)
                      (>= field-count max-fields)
                      (> (string-length buf-content) 0))
                 (let* ([full (string-append
                                buf-content
                                (substring str i len))])
                   (let* ([full (strip-trailing-ifs-ws full ifs-ws)])
                     (reverse (cons full fields))))
                 (let ([had-content? (> (string-length buf-content) 0)])
                   (when had-content?
                     (set! fields (cons buf-content fields))
                     (set! field-count (+ field-count 1)))
                   (set! buf (open-output-string))
                   (let skip ([j (+ i 1)])
                     (if (and (< j len)
                              (ifs-ws-member? (string-ref str j) ifs-ws))
                         (skip (+ j 1))
                         (let ([j (if (and had-content?
                                           (< j len)
                                           (ifs-nw-member?
                                             (string-ref str j)
                                             ifs-nw))
                                      (let skip2 ([k (+ j 1)])
                                        (if (and (< k len)
                                                 (ifs-ws-member?
                                                   (string-ref str k)
                                                   ifs-ws))
                                            (skip2 (+ k 1))
                                            k))
                                      j)])
                           (if (and (> max-fields 0)
                                    (>= field-count max-fields))
                               (begin
                                 (display (substring str j len) buf)
                                 (let* ([s (get-output-string buf)])
                                   (let* ([s (strip-trailing-ifs-for-read
                                               s
                                               ifs-ws
                                               ifs-nw)])
                                     (reverse (cons s fields)))))
                               (loop j))))))))]
          [else (display (string-ref str i) buf) (loop (+ i 1))]))))
  (define (read-ifs-split-bs str start len ifs-ws ifs-nw
           max-fields)
    (define (append-rest j buf)
      (let rloop ([j j])
        (when (< j len)
          (display (string-ref str j) buf)
          (rloop (+ j 1)))))
    (define (finalize-last-field raw-s)
      (read-strip-backslashes
        (strip-trailing-ifs-for-read raw-s ifs-ws ifs-nw)))
    (define (finalize-max-last-field raw-s)
      (read-strip-backslashes
        (strip-trailing-ifs-for-read raw-s ifs-ws ifs-nw)))
    (define (finalize-field raw-s)
      (read-strip-backslashes
        (strip-trailing-ifs-ws raw-s ifs-ws)))
    (let ([buf (open-output-string)]
          [fields (list)]
          [field-count 1])
      (let loop ([i start])
        (cond
          [(>= i len)
           (let* ([raw-s (get-output-string buf)])
             (let* ([s (if (> max-fields 0)
                           (finalize-max-last-field raw-s)
                           (finalize-last-field raw-s))])
               (reverse
                 (cond
                   [(> (string-length s) 0) (cons s fields)]
                   [(pair? fields)
                    (if (> max-fields 0) (cons s fields) fields)]
                   [else fields]))))]
          [(and (char=? (string-ref str i) #\\) (< (+ i 1) len))
           (display #\\ buf)
           (display (string-ref str (+ i 1)) buf)
           (loop (+ i 2))]
          [(and (char=? (string-ref str i) #\\) (= (+ i 1) len))
           (display #\\ buf)
           (loop (+ i 1))]
          [(ifs-nw-member? (string-ref str i) ifs-nw)
           (if (and (> max-fields 0) (>= field-count max-fields))
               (begin (display (string-ref str i) buf) (loop (+ i 1)))
               (let ([raw-s (get-output-string buf)])
                 (set! fields (cons (finalize-field raw-s) fields))
                 (set! buf (open-output-string))
                 (set! field-count (+ field-count 1))
                 (let skip ([j (+ i 1)])
                   (if (and (< j len)
                            (not (char=? (string-ref str j) #\\))
                            (ifs-ws-member? (string-ref str j) ifs-ws))
                       (skip (+ j 1))
                       (if (and (> max-fields 0)
                                (>= field-count max-fields))
                           (begin
                             (append-rest j buf)
                             (let ([s (finalize-max-last-field
                                        (get-output-string buf))])
                               (reverse (cons s fields))))
                           (loop j))))))]
          [(ifs-ws-member? (string-ref str i) ifs-ws)
           (let ([buf-content (get-output-string buf)])
             (if (and (> max-fields 0)
                      (>= field-count max-fields)
                      (> (string-length buf-content) 0))
                 (let ([new-buf (open-output-string)])
                   (display buf-content new-buf)
                   (append-rest i new-buf)
                   (let ([s (finalize-max-last-field
                              (get-output-string new-buf))])
                     (reverse (cons s fields))))
                 (let ([had-content? (> (string-length buf-content) 0)])
                   (when had-content?
                     (set! fields
                       (cons (read-strip-backslashes buf-content) fields))
                     (set! field-count (+ field-count 1)))
                   (set! buf (open-output-string))
                   (let skip ([j (+ i 1)])
                     (if (and (< j len)
                              (not (char=? (string-ref str j) #\\))
                              (ifs-ws-member? (string-ref str j) ifs-ws))
                         (skip (+ j 1))
                         (let ([j (if (and had-content?
                                           (< j len)
                                           (not (char=?
                                                  (string-ref str j)
                                                  #\\))
                                           (ifs-nw-member?
                                             (string-ref str j)
                                             ifs-nw))
                                      (let skip2 ([k (+ j 1)])
                                        (if (and (< k len)
                                                 (not (char=?
                                                        (string-ref str k)
                                                        #\\))
                                                 (ifs-ws-member?
                                                   (string-ref str k)
                                                   ifs-ws))
                                            (skip2 (+ k 1))
                                            k))
                                      j)])
                           (if (and (> max-fields 0)
                                    (>= field-count max-fields))
                               (begin
                                 (append-rest j buf)
                                 (let ([s (finalize-max-last-field
                                            (get-output-string buf))])
                                   (reverse (cons s fields))))
                               (loop j))))))))]
          [else (display (string-ref str i) buf) (loop (+ i 1))]))))
  (define (strip-trailing-ifs-ws s ws)
    (if (= (string-length ws) 0)
        s
        (let loop ([end (string-length s)])
          (if (and (> end 0)
                   (ifs-ws-member? (string-ref s (- end 1)) ws)
                   (not (and (> end 1)
                             (char=? (string-ref s (- end 2)) #\\))))
              (loop (- end 1))
              (substring s 0 end)))))
  (define (strip-trailing-ifs-for-read s ifs-ws ifs-nw)
    (let* ([s1 (strip-trailing-ifs-ws s ifs-ws)])
      (let* ([len (string-length s1)])
        (if (and (> len 0)
                 (ifs-nw-member? (string-ref s1 (- len 1)) ifs-nw)
                 (not (and (> len 1)
                           (char=? (string-ref s1 (- len 2)) #\\)))
                 (or (= len 1)
                     (let ([prev (string-ref s1 (- len 2))])
                       (and (not (ifs-nw-member? prev ifs-nw))
                            (not (ifs-ws-member? prev ifs-ws))))))
            (strip-trailing-ifs-ws (substring s1 0 (- len 1)) ifs-ws)
            s1))))
  (define (parse-array-compound-elements inner)
    (let ([len (string-length inner)]
          [elems (list)]
          [buf (open-output-string)])
      (let loop ([i 0]
                 [in-single? #f]
                 [in-double? #f]
                 [in-elem? #f])
        (cond
          [(>= i len)
           (let ([s (get-output-string buf)])
             (reverse
               (if (or in-elem? (> (string-length s) 0))
                   (cons s elems)
                   elems)))]
          [in-single?
           (if (char=? (string-ref inner i) #\')
               (loop (+ i 1) #f in-double? #t)
               (begin
                 (display (string-ref inner i) buf)
                 (loop (+ i 1) #t in-double? #t)))]
          [in-double?
           (cond
             [(char=? (string-ref inner i) #\")
              (loop (+ i 1) in-single? #f #t)]
             [(and (char=? (string-ref inner i) #\\) (< (+ i 1) len))
              (display (string-ref inner (+ i 1)) buf)
              (loop (+ i 2) in-single? #t #t)]
             [else
              (display (string-ref inner i) buf)
              (loop (+ i 1) in-single? #t #t)])]
          [(char=? (string-ref inner i) #\')
           (loop (+ i 1) #t in-double? #t)]
          [(char=? (string-ref inner i) #\")
           (loop (+ i 1) in-single? #t #t)]
          [(and (char=? (string-ref inner i) #\\) (< (+ i 1) len))
           (display (string-ref inner (+ i 1)) buf)
           (loop (+ i 2) in-single? in-double? #t)]
          [(or (char=? (string-ref inner i) #\space)
               (char=? (string-ref inner i) #\tab)
               (char=? (string-ref inner i) #\newline))
           (let ([s (get-output-string buf)])
             (if (or in-elem? (> (string-length s) 0))
                 (begin
                   (set! elems (cons s elems))
                   (set! buf (open-output-string))
                   (loop (+ i 1) #f #f #f))
                 (loop (+ i 1) #f #f #f)))]
          [else
           (display (string-ref inner i) buf)
           (loop (+ i 1) in-single? in-double? #t)]))))
  (define (parse-array-compound-raw inner)
    (let ([len (string-length inner)] [tokens (list)])
      (let loop ([i 0] [start #f] [in-single? #f] [in-double? #f])
        (cond
          [(>= i len)
           (reverse
             (if start (cons (substring inner start i) tokens) tokens))]
          [in-single?
           (if (char=? (string-ref inner i) #\')
               (loop (+ i 1) start #f in-double?)
               (loop (+ i 1) start #t in-double?))]
          [in-double?
           (cond
             [(char=? (string-ref inner i) #\")
              (loop (+ i 1) start in-single? #f)]
             [(and (char=? (string-ref inner i) #\\) (< (+ i 1) len))
              (loop (+ i 2) start in-single? #t)]
             [else (loop (+ i 1) start in-single? #t)])]
          [(char=? (string-ref inner i) #\')
           (loop (+ i 1) (or start i) #t in-double?)]
          [(char=? (string-ref inner i) #\")
           (loop (+ i 1) (or start i) in-single? #t)]
          [(and (char=? (string-ref inner i) #\\) (< (+ i 1) len))
           (loop (+ i 2) (or start i) in-single? in-double?)]
          [(or (char=? (string-ref inner i) #\space)
               (char=? (string-ref inner i) #\tab)
               (char=? (string-ref inner i) #\newline))
           (if start
               (begin
                 (set! tokens (cons (substring inner start i) tokens))
                 (loop (+ i 1) #f #f #f))
               (loop (+ i 1) #f #f #f))]
          [else (loop (+ i 1) (or start i) in-single? in-double?)]))))
  (define (builtin-declare args env)
    (let ([flags (make-hash-table)]
          [names (list)]
          [print? #f]
          [remove? #f]
          [func-mode #f])
      (let loop ([args args])
        (when (pair? args)
          (let ([arg (car args)])
            (cond
              [(and (> (string-length arg) 1)
                    (or (char=? (string-ref arg 0) #\-)
                        (char=? (string-ref arg 0) #\+))
                    (char-alphabetic? (string-ref arg 1)))
               (let ([remove (char=? (string-ref arg 0) #\+)])
                 (let floop ([i 1])
                   (when (< i (string-length arg))
                     (let ([ch (string-ref arg i)])
                       (case ch
                         [(#\p) (set! print? #t)]
                         [(#\x) (hash-put! flags 'export (not remove))]
                         [(#\r) (hash-put! flags 'readonly (not remove))]
                         [(#\i) (hash-put! flags 'integer (not remove))]
                         [(#\l) (hash-put! flags 'lowercase (not remove))]
                         [(#\u) (hash-put! flags 'uppercase (not remove))]
                         [(#\n) (hash-put! flags 'nameref (not remove))]
                         [(#\a) (hash-put! flags 'array #t)]
                         [(#\A) (hash-put! flags 'assoc #t)]
                         [(#\g) (hash-put! flags 'global #t)]
                         [(#\f) (set! func-mode 'f)]
                         [(#\F) (set! func-mode 'F)]
                         [else (%%void)]))
                     (floop (+ i 1))))
                 (loop (cdr args)))]
              [else (set! names (cons arg names)) (loop (cdr args))]))))
      (when func-mode
        (if (null? names)
            (begin
              (let ([fnames (sort!
                              (hash-keys (shell-environment-functions env))
                              string<?)])
                (for-each
                  (lambda (fname)
                    (if (eq? func-mode 'F)
                        (begin
                          (display (format "declare -f ~a" fname))
                          (newline))
                        (begin
                          (display (format "~a () { ... }" fname))
                          (newline))))
                  fnames))
              ((return-from-declare) 0))
            (let ([status 0])
              (for-each
                (lambda (fname)
                  (let ([func (hash-get
                                (shell-environment-functions env)
                                fname)])
                    (if func
                        (if (eq? func-mode 'F)
                            (if (env-shopt? env "extdebug")
                                (let ([lineno (or (shell-function-lineno
                                                    func)
                                                  "")]
                                      [srcfile (or (shell-function-source-file
                                                     func)
                                                   "")])
                                  (begin
                                    (display
                                      (format
                                        "~a ~a ~a"
                                        fname
                                        lineno
                                        srcfile))
                                    (newline)))
                                (begin (display fname) (newline)))
                            (begin
                              (display (format "~a () { ... }" fname))
                              (newline)))
                        (begin
                          (fprintf
                            (current-error-port)
                            "declare: ~a: not found~n"
                            fname)
                          (set! status 1)))))
                (reverse names))
              ((return-from-declare) status))))
      (when (and print? (null? names))
        (let ([target (if (hash-get flags 'global)
                          (env-root env)
                          env)])
          (declare-print-all-vars target flags))
        ((return-from-declare) 0))
      (when (and (= (hash-length flags) 0)
                 (null? names)
                 (not print?))
        (declare-print-all-vars-simple env)
        ((return-from-declare) 0))
      (let ([status 0])
        (for-each
          (lambda (arg)
            (let* ([eq-pos (string-find-char* arg #\=)])
              (let* ([name (if eq-pos (substring arg 0 eq-pos) arg)])
                (let* ([base-name (let ([bracket (string-find-char*
                                                   name
                                                   #\[)])
                                    (if bracket
                                        (substring name 0 bracket)
                                        name))])
                  (let* ([value (if eq-pos
                                    (substring
                                      arg
                                      (+ eq-pos 1)
                                      (string-length arg))
                                    #f)])
                    (if (not (and (> (string-length base-name) 0)
                                  (let ([ch0 (string-ref base-name 0)])
                                    (or (char-alphabetic? ch0)
                                        (char=? ch0 #\_)))
                                  (let vloop ([vi 1])
                                    (or (>= vi (string-length base-name))
                                        (let ([ch (string-ref
                                                    base-name
                                                    vi)])
                                          (and (or (char-alphabetic? ch)
                                                   (char-numeric? ch)
                                                   (char=? ch #\_))
                                               (vloop (+ vi 1))))))))
                        (begin
                          (fprintf
                            (current-error-port)
                            "declare: `~a': not a valid identifier~n"
                            arg)
                          (set! status 1))
                        (if print?
                            (let* ([lookup-env (if (hash-get flags 'global)
                                                   (env-root env)
                                                   env)])
                              (let* ([var (or (env-get-var lookup-env name)
                                              (env-get-raw-var
                                                lookup-env
                                                name))])
                                (let* ([has-attr-filter? (or (hash-get
                                                               flags
                                                               'nameref)
                                                             (hash-get
                                                               flags
                                                               'readonly)
                                                             (hash-get
                                                               flags
                                                               'export)
                                                             (hash-get
                                                               flags
                                                               'integer)
                                                             (hash-get
                                                               flags
                                                               'array)
                                                             (hash-get
                                                               flags
                                                               'assoc)
                                                             (hash-get
                                                               flags
                                                               'uppercase)
                                                             (hash-get
                                                               flags
                                                               'lowercase))])
                                  (if var
                                      (when (or (not has-attr-filter?)
                                                (and (hash-get
                                                       flags
                                                       'nameref)
                                                     (shell-var-nameref?
                                                       var))
                                                (and (hash-get
                                                       flags
                                                       'readonly)
                                                     (shell-var-readonly?
                                                       var))
                                                (and (hash-get
                                                       flags
                                                       'export)
                                                     (shell-var-exported?
                                                       var))
                                                (and (hash-get
                                                       flags
                                                       'integer)
                                                     (shell-var-integer?
                                                       var))
                                                (and (hash-get
                                                       flags
                                                       'array)
                                                     (shell-var-array?
                                                       var))
                                                (and (hash-get
                                                       flags
                                                       'assoc)
                                                     (shell-var-assoc?
                                                       var))
                                                (and (hash-get
                                                       flags
                                                       'uppercase)
                                                     (shell-var-uppercase?
                                                       var))
                                                (and (hash-get
                                                       flags
                                                       'lowercase)
                                                     (shell-var-lowercase?
                                                       var)))
                                        (display-declare-var name var))
                                      (begin
                                        (fprintf
                                          (current-error-port)
                                          "declare: ~a: not found~n"
                                          name)
                                        (set! status 1))))))
                            (let* ([target-env (if (hash-get flags 'global)
                                                   (env-root env)
                                                   env)])
                              (let* ([var (or (hash-get
                                                (shell-environment-vars
                                                  target-env)
                                                name)
                                              (let ([new-var (make-shell-var
                                                               (or value
                                                                   \x2B;unset-sentinel+)
                                                               #f #f #f #f
                                                               #f #f #f #f
                                                               #f)])
                                                (hash-put!
                                                  (shell-environment-vars
                                                    target-env)
                                                  name
                                                  new-var)
                                                new-var))])
                                (when value
                                  (if (and (> (string-length value) 0)
                                           (char=?
                                             (string-ref value 0)
                                             #\())
                                      (let ([inner (if (and (> (string-length
                                                                 value)
                                                               1)
                                                            (char=?
                                                              (string-ref
                                                                value
                                                                (- (string-length
                                                                     value)
                                                                   1))
                                                              #\)))
                                                       (substring
                                                         value
                                                         1
                                                         (- (string-length
                                                              value)
                                                            1))
                                                       (substring
                                                         value
                                                         1
                                                         (string-length
                                                           value)))])
                                        (let ([tbl (make-hash-table)])
                                          (if (hash-get flags 'assoc)
                                              (begin
                                                (let ([elems (parse-array-compound-elements
                                                               inner)])
                                                  (for-each
                                                    (lambda (elem)
                                                      (let ([bracket-start (string-find-char*
                                                                             elem
                                                                             #\[)])
                                                        (if bracket-start
                                                            (let* ([bracket-end (string-find-char-from
                                                                                  elem
                                                                                  #\]
                                                                                  (+ bracket-start
                                                                                     1))])
                                                              (let* ([key (substring
                                                                            elem
                                                                            (+ bracket-start
                                                                               1)
                                                                            (or bracket-end
                                                                                (string-length
                                                                                  elem)))])
                                                                (let* ([eq-pos (string-find-char-from
                                                                                 elem
                                                                                 #\=
                                                                                 (or bracket-end
                                                                                     0))])
                                                                  (let* ([val (if eq-pos
                                                                                  (substring
                                                                                    elem
                                                                                    (+ eq-pos
                                                                                       1)
                                                                                    (string-length
                                                                                      elem))
                                                                                  "")])
                                                                    (hash-put!
                                                                      tbl
                                                                      key
                                                                      val)))))
                                                            (%%void))))
                                                    elems))
                                                (shell-var-value-set!
                                                  var
                                                  tbl)
                                                (shell-var-assoc?-set!
                                                  var
                                                  #t)
                                                (shell-var-array?-set!
                                                  var
                                                  #f))
                                              (begin
                                                (let ([elems (parse-array-compound-elements
                                                               inner)]
                                                      [idx 0])
                                                  (for-each
                                                    (lambda (elem)
                                                      (if (and (> (string-length
                                                                    elem)
                                                                  0)
                                                               (char=?
                                                                 (string-ref
                                                                   elem
                                                                   0)
                                                                 #\[))
                                                          (let* ([bracket-end (string-find-char*
                                                                                elem
                                                                                #\])])
                                                            (let* ([key-str (substring
                                                                              elem
                                                                              1
                                                                              (or bracket-end
                                                                                  (string-length
                                                                                    elem)))])
                                                              (let* ([eq-pos (string-find-char-from
                                                                               elem
                                                                               #\=
                                                                               (or bracket-end
                                                                                   0))])
                                                                (let* ([val (if eq-pos
                                                                                (substring
                                                                                  elem
                                                                                  (+ eq-pos
                                                                                     1)
                                                                                  (string-length
                                                                                    elem))
                                                                                "")])
                                                                  (let* ([key (or (string->number
                                                                                    key-str)
                                                                                  idx)])
                                                                    (hash-put!
                                                                      tbl
                                                                      key
                                                                      val)
                                                                    (set! idx
                                                                      (+ key
                                                                         1)))))))
                                                          (begin
                                                            (hash-put!
                                                              tbl
                                                              idx
                                                              elem)
                                                            (set! idx
                                                              (+ idx 1)))))
                                                    elems))
                                                (shell-var-value-set!
                                                  var
                                                  tbl)
                                                (shell-var-array?-set!
                                                  var
                                                  #t)))))
                                      (let ([final (apply-var-attrs
                                                     var
                                                     value
                                                     env)])
                                        (shell-var-value-set! var final)
                                        (when (shell-var-exported? var)
                                          (setenv name final)))))
                                (when (and (env-option? env "allexport")
                                           (not (hash-key? flags 'export)))
                                  (shell-var-exported?-set! var #t)
                                  (setenv
                                    name
                                    (or (shell-var-scalar-value var) "")))
                                (when (hash-get flags 'export)
                                  (shell-var-exported?-set! var #t)
                                  (setenv name (shell-var-value var)))
                                (when (hash-key? flags 'export)
                                  (unless (hash-get flags 'export)
                                    (shell-var-exported?-set! var #f)
                                    (ffi-unsetenv name)))
                                (when (hash-get flags 'readonly)
                                  (shell-var-readonly?-set! var #t))
                                (when (hash-key? flags 'readonly)
                                  (unless (hash-get flags 'readonly)
                                    (shell-var-readonly?-set! var #f)))
                                (when (hash-get flags 'integer)
                                  (shell-var-integer?-set! var #t)
                                  (shell-var-value-set!
                                    var
                                    (apply-var-attrs
                                      var
                                      (shell-var-value var)
                                      env)))
                                (when (hash-key? flags 'integer)
                                  (unless (hash-get flags 'integer)
                                    (shell-var-integer?-set! var #f)))
                                (when (hash-get flags 'uppercase)
                                  (shell-var-uppercase?-set! var #t)
                                  (shell-var-lowercase?-set! var #f)
                                  (when (string? (shell-var-value var))
                                    (shell-var-value-set!
                                      var
                                      (string-upcase
                                        (shell-var-value var)))))
                                (when (hash-get flags 'lowercase)
                                  (shell-var-lowercase?-set! var #t)
                                  (shell-var-uppercase?-set! var #f)
                                  (when (string? (shell-var-value var))
                                    (shell-var-value-set!
                                      var
                                      (string-downcase
                                        (shell-var-value var)))))
                                (when (hash-get flags 'nameref)
                                  (shell-var-nameref?-set! var #t))
                                (when (hash-get flags 'array)
                                  (unless (or (shell-var-array? var)
                                              (shell-var-assoc? var))
                                    (let ([old-val (shell-var-value var)])
                                      (let ([tbl (make-hash-table)])
                                        (when (and (string? old-val)
                                                   (> (string-length
                                                        old-val)
                                                      0))
                                          (hash-put! tbl 0 old-val))
                                        (shell-var-value-set! var tbl)
                                        (shell-var-array?-set! var #t)))))
                                (when (hash-get flags 'assoc)
                                  (unless (shell-var-assoc? var)
                                    (shell-var-value-set!
                                      var
                                      (make-hash-table))
                                    (shell-var-assoc?-set! var #t)
                                    (shell-var-array?-set!
                                      var
                                      #f))))))))))))
          (reverse names))
        status)))
  (define (collect-all-vars env)
    (let ([result (make-hash-table)])
      (let walk ([e env])
        (when e
          (walk (shell-environment-parent e))
          (for-each
            (lambda (pair)
              (let ([name (car pair)] [var (cdr pair)])
                (hash-put! result name var)))
            (hash->list (shell-environment-vars e)))))
      result))
  (define (declare-print-all-vars env flags)
    (let* ([all-vars (collect-all-vars env)])
      (let* ([filter-export (hash-get flags 'export)])
        (let* ([filter-readonly (hash-get flags 'readonly)])
          (let* ([filter-nameref (hash-get flags 'nameref)])
            (let* ([filter-array (hash-get flags 'array)])
              (let* ([filter-assoc (hash-get flags 'assoc)])
                (let* ([names (sort! (hash-keys all-vars) string<?)])
                  (for-each
                    (lambda (name)
                      (let ([var (hash-get all-vars name)])
                        (when (and var
                                   (not (eq? (shell-var-value var)
                                             \x2B;unset-sentinel+))
                                   (or (not filter-export)
                                       (shell-var-exported? var))
                                   (or (not filter-readonly)
                                       (shell-var-readonly? var))
                                   (or (not filter-nameref)
                                       (shell-var-nameref? var))
                                   (or (not filter-array)
                                       (shell-var-array? var))
                                   (or (not filter-assoc)
                                       (shell-var-assoc? var)))
                          (display-declare-var name var))))
                    names)))))))))
  (define (declare-print-all-vars-simple env)
    (let* ([all-vars (collect-all-vars env)])
      (let* ([names (sort! (hash-keys all-vars) string<?)])
        (for-each
          (lambda (name)
            (let ([var (hash-get all-vars name)])
              (when (and var
                         (not (eq? (shell-var-value var)
                                   \x2B;unset-sentinel+)))
                (begin
                  (display
                    (format "~a=~a" name (shell-var-scalar-value var)))
                  (newline)))))
          names))))
  (define (declare-var-flags var)
    (string-append (if (shell-var-integer? var) "i" "")
      (if (shell-var-lowercase? var) "l" "")
      (if (shell-var-nameref? var) "n" "")
      (if (shell-var-readonly? var) "r" "")
      (if (shell-var-uppercase? var) "u" "")
      (if (shell-var-exported? var) "x" "")
      (if (shell-var-assoc? var) "A" "")
      (if (shell-var-array? var) "a" "")))
  (define (array-dense? tbl)
    (let ([keys (hash-keys tbl)])
      (and (pair? keys)
           (let ([n (length keys)])
             (let loop ([i 0])
               (if (>= i n) #t (and (hash-key? tbl i) (loop (+ i 1)))))))))
  (define (declare-quote-value val)
    (if (not val)
        "''"
        (let ([s (if (string? val) val (format "~a" val))])
          (shell-quote-value s))))
  (define (declare-quote-scalar val)
    (let* ([s (if (string? val) val (format "~a" val))])
      (let* ([len (string-length s)])
        (let* ([buf (open-output-string)])
          (let* ([has-control? (let loop ([i 0])
                                 (if (>= i len)
                                     #f
                                     (let ([ch (string-ref s i)])
                                       (if (or (char<? ch #\space)
                                               (char=? ch #\delete))
                                           #t
                                           (loop (+ i 1))))))])
            (if has-control?
                (shell-quote-value s)
                (begin
                  (display "\"" buf)
                  (let loop ([i 0])
                    (when (< i len)
                      (let ([ch (string-ref s i)])
                        (case ch
                          [(#\$ #\` #\" #\\)
                           (display "\\" buf)
                           (display ch buf)]
                          [(#\newline) (display "\\n" buf)]
                          [else (display ch buf)]))
                      (loop (+ i 1))))
                  (display "\"" buf)
                  (get-output-string buf))))))))
  (define (display-declare-var name var)
    (let* ([flags (declare-var-flags var)])
      (let* ([flag-str (if (string=? flags "")
                           "--"
                           (string-append "-" flags))])
        (cond
          [(eq? (shell-var-value var) \x2B;unset-sentinel+)
           (begin
             (display (format "declare ~a ~a" flag-str name))
             (newline))]
          [(shell-var-array? var)
           (let ([tbl (shell-var-value var)])
             (display (format "declare ~a ~a=(" flag-str name))
             (when (hash-table? tbl)
               (let* ([keys (sort! (hash-keys tbl) <)])
                 (let* ([last-key (and (pair? keys) (last keys))])
                   (if (array-dense? tbl)
                       (for-each
                         (lambda (k)
                           (display (declare-quote-value (hash-get tbl k)))
                           (unless (equal? k last-key) (display " ")))
                         keys)
                       (for-each
                         (lambda (k)
                           (display
                             (format
                               "[~a]=~a"
                               k
                               (declare-quote-value (hash-get tbl k))))
                           (unless (equal? k last-key) (display " ")))
                         keys)))))
             (begin (display ")") (newline)))]
          [(shell-var-assoc? var)
           (let ([tbl (shell-var-value var)])
             (display (format "declare ~a ~a=(" flag-str name))
             (when (hash-table? tbl)
               (let* ([keys (sort!
                              (map (lambda (k)
                                     (if (string? k) k (format "~a" k)))
                                   (hash-keys tbl))
                              string<?)])
                 (let* ([last-key (and (pair? keys) (last keys))])
                   (for-each
                     (lambda (k)
                       (let ([qk (let ([sv (shell-quote-value k)])
                                   (if (and (> (string-length sv) 0)
                                            (not (char=?
                                                   (string-ref sv 0)
                                                   #\'))
                                            (not (and (> (string-length sv)
                                                         1)
                                                      (char=?
                                                        (string-ref sv 0)
                                                        #\$)
                                                      (char=?
                                                        (string-ref sv 1)
                                                        #\'))))
                                       (string-append "'" sv "'")
                                       sv))])
                         (display
                           (format
                             "[~a]=~a"
                             qk
                             (declare-quote-value (hash-get tbl k)))))
                       (unless (string=? k last-key) (display " ")))
                     keys))))
             (begin (display ")") (newline)))]
          [else
           (begin
             (display
               (format
                 "declare ~a ~a=~a"
                 flag-str
                 name
                 (declare-quote-value (shell-var-scalar-value var))))
             (newline))]))))
  (define return-from-declare (make-parameter #f))
  (define *ulimit-resources*
    '((#\c "core file size          (blocks, -c)" 4 512) (#\d "data seg size           (kbytes, -d)" 2 1024)
       (#\f "file size               (blocks, -f)" 1 512)
       (#\i "pending signals                 (-i)" 11 1)
       (#\l "max locked memory       (kbytes, -l)" 8 1024)
       (#\m "max memory size         (kbytes, -m)" 9 1024)
       (#\n "open files                      (-n)" 7 1)
       (#\q "POSIX message queues     (bytes, -q)" 12 1)
       (#\r "real-time priority              (-r)" 14 1)
       (#\s "stack size              (kbytes, -s)" 3 1024)
       (#\t "cpu time               (seconds, -t)" 0 1)
       (#\u "max user processes              (-u)" 6 1)
       (#\v "virtual memory          (kbytes, -v)" 9 1024)
       (#\x "file locks                      (-x)" 10 1)
       (#\e "scheduling priority             (-e)" 13 1)
       (#\p "pipe size            (512 bytes, -p)" #f 1)))
  (define (builtin-mapfile args env)
    (let loop ([args args]
               [delim "\n"]
               [max-count 0]
               [origin 0]
               [skip 0]
               [trim? #f]
               [arr-name #f])
      (cond
        [(null? args)
         (let* ([name (or arr-name "MAPFILE")])
           (let* ([delim-ch (if (> (string-length delim) 0)
                                (string-ref delim 0)
                                #\newline)])
             (let* ([lines (list)])
               (let rloop ([lines (list)] [count 0])
                 (let ([line (if (char=? delim-ch #\newline)
                                 (get-line (current-input-port))
                                 (let ([buf (open-output-string)])
                                   (let cloop ()
                                     (let ([ch (read-char)])
                                       (cond
                                         [(eof-object? ch)
                                          (let ([s (get-output-string
                                                     buf)])
                                            (if (string=? s "") ch s))]
                                         [(char=? ch delim-ch)
                                          (if trim?
                                              (get-output-string buf)
                                              (begin
                                                (display ch buf)
                                                (get-output-string buf)))]
                                         [else
                                          (display ch buf)
                                          (cloop)])))))])
                   (cond
                     [(eof-object? line)
                      (let ([kept (if (> skip 0)
                                      (list-tail* lines skip)
                                      lines)])
                        (let aloop ([lst kept] [idx origin])
                          (when (pair? lst)
                            (env-array-set! env name idx (car lst))
                            (aloop (cdr lst) (+ idx 1)))))
                      0]
                     [(and (> max-count 0) (>= (- count skip) max-count))
                      (let ([kept (if (> skip 0)
                                      (list-tail* lines skip)
                                      lines)])
                        (let aloop ([lst kept] [idx origin])
                          (when (pair? lst)
                            (env-array-set! env name idx (car lst))
                            (aloop (cdr lst) (+ idx 1)))))
                      0]
                     [else
                      (let ([line (if (and trim?
                                           (char=? delim-ch #\newline))
                                      line
                                      (if (and (not trim?)
                                               (char=? delim-ch #\newline))
                                          (string-append line "\n")
                                          line))])
                        (rloop
                          (append lines (list line))
                          (+ count 1)))]))))))]
        [(string=? (car args) "-d")
         (if (pair? (cdr args))
             (loop (cddr args) (cadr args) max-count origin skip trim?
               arr-name)
             (loop (cdr args) delim max-count origin skip trim?
               arr-name))]
        [(string=? (car args) "-n")
         (if (pair? (cdr args))
             (loop (cddr args) delim (or (string->number (cadr args)) 0)
               origin skip trim? arr-name)
             (loop (cdr args) delim max-count origin skip trim?
               arr-name))]
        [(string=? (car args) "-O")
         (if (pair? (cdr args))
             (loop (cddr args) delim max-count
               (or (string->number (cadr args)) 0) skip trim? arr-name)
             (loop (cdr args) delim max-count origin skip trim?
               arr-name))]
        [(string=? (car args) "-s")
         (if (pair? (cdr args))
             (loop (cddr args) delim max-count origin
               (or (string->number (cadr args)) 0) trim? arr-name)
             (loop (cdr args) delim max-count origin skip trim?
               arr-name))]
        [(string=? (car args) "-t")
         (loop (cdr args) delim max-count origin skip #t arr-name)]
        [else
         (loop (cdr args) delim max-count origin skip trim?
           (car args))])))
  (define (list-tail* lst n)
    (if (or (<= n 0) (null? lst))
        lst
        (list-tail* (cdr lst) (- n 1))))
  (define (test-eval args) (test-eval-or args))
  (define (test-eval-or args)
    (let split ([rest args] [left (list)] [depth 0])
      (cond
        [(null? rest) (test-eval-and (reverse left))]
        [(string=? (car rest) "(")
         (split (cdr rest) (cons (car rest) left) (+ depth 1))]
        [(string=? (car rest) ")")
         (split
           (cdr rest)
           (cons (car rest) left)
           (max 0 (- depth 1)))]
        [(and (= depth 0)
              (string=? (car rest) "-o")
              (pair? left)
              (pair? (cdr rest)))
         (let ([lresult (test-eval-and (reverse left))])
           (if (= lresult 0) 0 (test-eval-or (cdr rest))))]
        [else (split (cdr rest) (cons (car rest) left) depth)])))
  (define (test-eval-and args)
    (let split ([rest args] [left (list)] [depth 0])
      (cond
        [(null? rest) (test-eval-not (reverse left))]
        [(string=? (car rest) "(")
         (split (cdr rest) (cons (car rest) left) (+ depth 1))]
        [(string=? (car rest) ")")
         (split
           (cdr rest)
           (cons (car rest) left)
           (max 0 (- depth 1)))]
        [(and (= depth 0) (string=? (car rest) "-a"))
         (if (and (pair? left) (pair? (cdr rest)))
             (let ([lresult (test-eval-not (reverse left))])
               (if (= lresult 0) (test-eval-and (cdr rest)) 1))
             (split (cdr rest) (cons (car rest) left) depth))]
        [else (split (cdr rest) (cons (car rest) left) depth)])))
  (define (test-find-close-paren args)
    (let loop ([rest (cdr args)] [depth 1] [idx 1])
      (cond
        [(null? rest) #f]
        [(string=? (car rest) "(")
         (loop (cdr rest) (+ depth 1) (+ idx 1))]
        [(string=? (car rest) ")")
         (if (= depth 1)
             idx
             (loop (cdr rest) (- depth 1) (+ idx 1)))]
        [else (loop (cdr rest) depth (+ idx 1))])))
  (define (test-eval-not args)
    (let ([len (length args)])
      (cond
        [(= len 0) 1]
        [(string=? (car args) "!")
         (let ([result (test-eval-not (cdr args))])
           (if (= result 0) 1 (if (= result 1) 0 result)))]
        [(and (string=? (car args) "(") (>= len 3))
         (let ([close-idx (test-find-close-paren args)])
           (if (and close-idx (= close-idx (- len 1)))
               (test-eval (list-head (cdr args) (- close-idx 1)))
               (test-eval-primary args len)))]
        [else (test-eval-primary args len)])))
  (define (test-eval-primary args len)
    (cond
      [(= len 1) (if (> (string-length (car args)) 0) 0 1)]
      [(= len 2) (test-unary (car args) (cadr args))]
      [(= len 3)
       (let ([a (car args)] [b (cadr args)] [c (caddr args)])
         (cond
           [(string=? a "!")
            (let ([r (test-eval-not (cdr args))])
              (if (= r 0) 1 (if (= r 1) 0 r)))]
           [(and (string=? a "(") (string=? c ")"))
            (test-eval-not (list b))]
           [else (test-binary a b c)]))]
      [(= len 4)
       (let ([a (car args)] [d (list-ref args 3)])
         (cond
           [(string=? a "!")
            (let ([r (test-eval (cdr args))])
              (if (= r 0) 1 (if (= r 1) 0 r)))]
           [(and (string=? a "(") (string=? d ")"))
            (test-eval (list (cadr args) (caddr args)))]
           [else 2]))]
      [else
       (if (string=? (car args) "!")
           (let ([r (test-eval (cdr args))])
             (if (= r 0) 1 (if (= r 1) 0 r)))
           2)]))
  (define (test-unary flag arg)
    (case (string->symbol flag)
      [(\x2D;z) (if (= (string-length arg) 0) 0 1)]
      [(\x2D;n) (if (> (string-length arg) 0) 0 1)]
      [(\x2D;e \x2D;a) (if (file-exists? arg) 0 1)]
      [(\x2D;f) (if (file-regular? arg) 0 1)]
      [(\x2D;d) (if (file-directory? arg) 0 1)]
      [(\x2D;r) (if (file-readable? arg) 0 1)]
      [(\x2D;w) (if (file-writable? arg) 0 1)]
      [(\x2D;x) (if (= (ffi-access arg 1) 0) 0 1)]
      [(\x2D;s) (if (file-nonempty? arg) 0 1)]
      [(\x2D;L \x2D;h) (if (file-symlink? arg) 0 1)]
      [(\x2D;t)
       (let ([fd (string->number arg)])
         (if (not fd) 2 (if (= (ffi-isatty fd) 1) 0 1)))]
      [(\x2D;p)
       (if (and (file-exists? arg)
                (eq? (gambit-file-info-type (gambit-file-info arg)) 'fifo))
           0
           1)]
      [(\x2D;b)
       (if (and (file-exists? arg)
                (eq? (gambit-file-info-type (gambit-file-info arg))
                     'block-special))
           0
           1)]
      [(\x2D;c)
       (if (and (file-exists? arg)
                (eq? (gambit-file-info-type (gambit-file-info arg))
                     'character-special))
           0
           1)]
      [(\x2D;v)
       (if (*test-var-fn*) (if ((*test-var-fn*) arg) 0 1) 1)]
      [(\x2D;o)
       (if (*test-option-fn*) (if ((*test-option-fn*) arg) 0 1) 1)]
      [(\x2D;G)
       (guard (__exn [#t ((lambda (e) 1) __exn)])
         (if (and (file-exists? arg)
                  (= (file-info-group (gambit-file-info arg))
                     (ffi-getegid)))
             0
             1))]
      [(\x2D;O)
       (guard (__exn [#t ((lambda (e) 1) __exn)])
         (if (and (file-exists? arg)
                  (= (file-info-owner (gambit-file-info arg))
                     (ffi-geteuid)))
             0
             1))]
      [(\x2D;u)
       (guard (__exn [#t ((lambda (e) 1) __exn)])
         (if (and (file-exists? arg)
                  (not (zero?
                         (bitwise-and
                           (file-info-mode (gambit-file-info arg))
                           2048))))
             0
             1))]
      [(\x2D;g)
       (guard (__exn [#t ((lambda (e) 1) __exn)])
         (if (and (file-exists? arg)
                  (not (zero?
                         (bitwise-and
                           (file-info-mode (gambit-file-info arg))
                           1024))))
             0
             1))]
      [(\x2D;k)
       (guard (__exn [#t ((lambda (e) 1) __exn)])
         (if (and (file-exists? arg)
                  (not (zero?
                         (bitwise-and
                           (file-info-mode (gambit-file-info arg))
                           512))))
             0
             1))]
      [(\x2D;S)
       (if (and (file-exists? arg)
                (eq? (gambit-file-info-type (gambit-file-info arg))
                     'socket))
           0
           1)]
      [(\x2D;N)
       (guard (__exn [#t ((lambda (e) 1) __exn)])
         (if (file-exists? arg)
             (let ([fi (gambit-file-info arg)])
               (if (> (let ([t (file-info-last-modification-time fi)])
                        (if (time? t)
                            (+ (time-second t)
                               (/ (time-nanosecond t) 1000000000.0))
                            t))
                      (let ([t (file-info-last-access-time fi)])
                        (if (time? t)
                            (+ (time-second t)
                               (/ (time-nanosecond t) 1000000000.0))
                            t)))
                   0
                   1))
             1))]
      [else 2]))
  (define *test-var-fn* (make-parameter #f))
  (define *test-option-fn* (make-parameter #f))
  (define (test-parse-integer str)
    (let ([s (test-string-trim str)])
      (cond
        [(string=? s "") #f]
        [(and (> (string-length s) 0) (char=? (string-ref s 0) #\-))
         (let ([n (test-parse-integer
                    (substring s 1 (string-length s)))])
           (and n (- n)))]
        [else (string->number s)])))
  (define (test-string-trim s)
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
  (define (test-binary left op right)
    (case (string->symbol op)
      [(= ==) (if (string=? left right) 0 1)]
      [(!=) (if (not (string=? left right)) 0 1)]
      [(<) (if (string<? left right) 0 1)]
      [(>) (if (string>? left right) 0 1)]
      [(\x2D;eq) (test-int-cmp = left right)]
      [(\x2D;ne)
       (test-int-cmp (lambda (a b) (not (= a b))) left right)]
      [(\x2D;lt) (test-int-cmp < left right)]
      [(\x2D;le) (test-int-cmp <= left right)]
      [(\x2D;gt) (test-int-cmp > left right)]
      [(\x2D;ge) (test-int-cmp >= left right)]
      [(\x2D;nt)
       (if (and (file-exists? left)
                (file-exists? right)
                (> (let ([t (file-info-last-modification-time
                              (gambit-file-info left))])
                     (if (time? t)
                         (+ (time-second t)
                            (/ (time-nanosecond t) 1000000000.0))
                         t))
                   (let ([t (file-info-last-modification-time
                              (gambit-file-info right))])
                     (if (time? t)
                         (+ (time-second t)
                            (/ (time-nanosecond t) 1000000000.0))
                         t))))
           0
           1)]
      [(\x2D;ot)
       (if (and (file-exists? left)
                (file-exists? right)
                (< (let ([t (file-info-last-modification-time
                              (gambit-file-info left))])
                     (if (time? t)
                         (+ (time-second t)
                            (/ (time-nanosecond t) 1000000000.0))
                         t))
                   (let ([t (file-info-last-modification-time
                              (gambit-file-info right))])
                     (if (time? t)
                         (+ (time-second t)
                            (/ (time-nanosecond t) 1000000000.0))
                         t))))
           0
           1)]
      [(\x2D;ef)
       (if (and (file-exists? left)
                (file-exists? right)
                (let ([li (gambit-file-info left)]
                      [ri (gambit-file-info right)])
                  (and (= (file-info-device li) (file-info-device ri))
                       (= (file-info-inode li) (file-info-inode ri)))))
           0
           1)]
      [(\x2D;a)
       (let ([l (if (> (string-length left) 0) 0 1)]
             [r (if (> (string-length right) 0) 0 1)])
         (if (and (= l 0) (= r 0)) 0 1))]
      [(\x2D;o)
       (let ([l (if (> (string-length left) 0) 0 1)]
             [r (if (> (string-length right) 0) 0 1)])
         (if (or (= l 0) (= r 0)) 0 1))]
      [else 2]))
  (define (test-int-cmp cmp left right)
    (let ([l (test-parse-integer left)]
          [r (test-parse-integer right)])
      (cond
        [(not l)
         (fprintf
           (current-error-port)
           "test: ~a: integer expression expected~n"
           left)
         2]
        [(not r)
         (fprintf
           (current-error-port)
           "test: ~a: integer expression expected~n"
           right)
         2]
        [else (if (cmp l r) 0 1)])))
  (define (display-raw-bytes segments)
    (for-each
      (lambda (seg)
        (if (fixnum? seg) (write-u8 seg) (display seg)))
      segments))
  (define (echo-expand-escapes str)
    (let ([len (string-length str)] [buf (open-output-string)])
      (define (flush-buf! acc)
        (let ([s (get-output-string buf)])
          (set! buf (open-output-string))
          (if (string=? s "") acc (cons s acc))))
      (let loop ([i 0] [acc (list)])
        (cond
          [(>= i len) (cons (reverse (flush-buf! acc)) #f)]
          [(and (char=? (string-ref str i) #\\) (< (+ i 1) len))
           (let ([next (string-ref str (+ i 1))])
             (case next
               [(#\n) (display "\n" buf) (loop (+ i 2) acc)]
               [(#\t) (display "\t" buf) (loop (+ i 2) acc)]
               [(#\r) (display "\r" buf) (loop (+ i 2) acc)]
               [(#\a) (display "\a" buf) (loop (+ i 2) acc)]
               [(#\b) (display "\b" buf) (loop (+ i 2) acc)]
               [(#\e #\E)
                (display (string (integer->char 27)) buf)
                (loop (+ i 2) acc)]
               [(#\f) (display "\f" buf) (loop (+ i 2) acc)]
               [(#\v) (display "\v" buf) (loop (+ i 2) acc)]
               [(#\\) (display "\\" buf) (loop (+ i 2) acc)]
               [(#\0)
                (let oloop ([j (+ i 2)] [val 0] [count 0])
                  (if (and (< j len)
                           (< count 3)
                           (char>=? (string-ref str j) #\0)
                           (char<=? (string-ref str j) #\7))
                      (oloop
                        (+ j 1)
                        (+ (* val 8)
                           (- (char->integer (string-ref str j))
                              (char->integer #\0)))
                        (+ count 1))
                      (let ([byte (bitwise-and val 255)])
                        (loop j (cons byte (flush-buf! acc))))))]
               [(#\x)
                (let hloop ([j (+ i 2)] [val 0] [count 0])
                  (if (and (< j len) (< count 2))
                      (let ([hv (hex-digit-value (string-ref str j))])
                        (if hv
                            (hloop (+ j 1) (+ (* val 16) hv) (+ count 1))
                            (if (= count 0)
                                (begin (display "\\x" buf) (loop j acc))
                                (loop
                                  j
                                  (cons
                                    (bitwise-and val 255)
                                    (flush-buf! acc))))))
                      (if (= count 0)
                          (begin (display "\\x" buf) (loop j acc))
                          (loop
                            j
                            (cons
                              (bitwise-and val 255)
                              (flush-buf! acc))))))]
               [(#\u)
                (let hloop ([j (+ i 2)] [val 0] [count 0])
                  (if (and (< j len) (< count 4))
                      (let ([hv (hex-digit-value (string-ref str j))])
                        (if hv
                            (hloop (+ j 1) (+ (* val 16) hv) (+ count 1))
                            (if (= count 0)
                                (begin (display "\\u" buf) (loop j acc))
                                (begin
                                  (display-unicode-char val buf)
                                  (loop j acc)))))
                      (if (= count 0)
                          (begin (display "\\u" buf) (loop j acc))
                          (begin
                            (display-unicode-char val buf)
                            (loop j acc)))))]
               [(#\U)
                (let hloop ([j (+ i 2)] [val 0] [count 0])
                  (if (and (< j len) (< count 8))
                      (let ([hv (hex-digit-value (string-ref str j))])
                        (if hv
                            (hloop (+ j 1) (+ (* val 16) hv) (+ count 1))
                            (if (= count 0)
                                (begin (display "\\U" buf) (loop j acc))
                                (begin
                                  (display-unicode-char val buf)
                                  (loop j acc)))))
                      (if (= count 0)
                          (begin (display "\\U" buf) (loop j acc))
                          (begin
                            (display-unicode-char val buf)
                            (loop j acc)))))]
               [(#\c) (cons (reverse (flush-buf! acc)) #t)]
               [else
                (display "\\" buf)
                (display (string next) buf)
                (loop (+ i 2) acc)]))]
          [else
           (let* ([ch (string-ref str i)])
             (let* ([code (char->integer ch)])
               (if (and (>= code raw-byte-base)
                        (<= code (+ raw-byte-base 255)))
                   (loop
                     (+ i 1)
                     (cons (- code raw-byte-base) (flush-buf! acc)))
                   (begin (display ch buf) (loop (+ i 1) acc)))))]))))
  (define (shell-printf fmt args)
    (let ([buf (open-output-u8vector
                 (list 'char-encoding: 'UTF-8))])
      (parameterize ([*printf-stop* #f])
        (if (null? args)
            (begin
              (printf-format-once fmt (list) buf)
              (get-output-u8vector buf))
            (let loop ([remaining args])
              (if (*printf-stop*)
                  (get-output-u8vector buf)
                  (let ([leftover (printf-format-once fmt remaining buf)])
                    (if (or (null? leftover) (equal? leftover remaining))
                        (get-output-u8vector buf)
                        (loop leftover)))))))))
  (define (printf-format-once fmt args buf)
    (let ([flen (string-length fmt)])
      (let loop ([i 0] [args args])
        (cond
          [(or (>= i flen) (*printf-stop*)) args]
          [(and (char=? (string-ref fmt i) #\%) (< (+ i 1) flen))
           (let-values ([(end consumed-arg)
                         (printf-format-spec fmt (+ i 1) args buf)])
             (loop end consumed-arg))]
          [(and (char=? (string-ref fmt i) #\\) (< (+ i 1) flen))
           (let ([end (printf-escape fmt (+ i 1) buf)])
             (loop end args))]
          [else
           (display (string-ref fmt i) buf)
           (loop (+ i 1) args)]))))
  (define (printf-format-spec fmt i args buf)
    (let ([flen (string-length fmt)])
      (let flag-loop ([i i]
                      [left-align? #f]
                      [plus-sign? #f]
                      [space-sign? #f]
                      [zero-pad? #f]
                      [alt-form? #f])
        (if (>= i flen)
            (values i args)
            (let ([ch (string-ref fmt i)])
              (cond
                [(char=? ch #\-)
                 (flag-loop (+ i 1) #t plus-sign? space-sign? zero-pad?
                   alt-form?)]
                [(char=? ch #\+)
                 (flag-loop (+ i 1) left-align? #t space-sign? zero-pad?
                   alt-form?)]
                [(char=? ch #\space)
                 (flag-loop (+ i 1) left-align? plus-sign? #t zero-pad?
                   alt-form?)]
                [(char=? ch #\0)
                 (flag-loop (+ i 1) left-align? plus-sign? space-sign? #t
                   alt-form?)]
                [(char=? ch #\#)
                 (flag-loop (+ i 1) left-align? plus-sign? space-sign?
                   zero-pad? #t)]
                [(char=? ch #\()
                 (let ([close (string-find-char-from fmt #\) (+ i 1))])
                   (if (and close
                            (< (+ close 1) flen)
                            (char=? (string-ref fmt (+ close 1)) #\T))
                       (let* ([strfmt (substring fmt (+ i 1) close)])
                         (let* ([arg (if (pair? args) (car args) "")])
                           (let* ([rest (if (pair? args)
                                            (cdr args)
                                            (list))])
                             (let* ([epoch (cond
                                             [(string=? arg "")
                                              (inexact->exact
                                                (floor
                                                  (let ([t (current-time)])
                                                    (if (time? t)
                                                        (+ (time-second t)
                                                           (/ (time-nanosecond
                                                                t)
                                                              1000000000.0))
                                                        t))))]
                                             [(string=? arg "-1")
                                              (inexact->exact
                                                (floor
                                                  (let ([t (current-time)])
                                                    (if (time? t)
                                                        (+ (time-second t)
                                                           (/ (time-nanosecond
                                                                t)
                                                              1000000000.0))
                                                        t))))]
                                             [(string=? arg "-2")
                                              (inexact->exact
                                                (floor
                                                  (let ([t (current-time)])
                                                    (if (time? t)
                                                        (+ (time-second t)
                                                           (/ (time-nanosecond
                                                                t)
                                                              1000000000.0))
                                                        t))))]
                                             [else
                                              (or (string->number arg)
                                                  0)])])
                               (let* ([result (ffi-strftime strfmt epoch)])
                                 (display result buf)
                                 (values (+ close 2) rest))))))
                       (values i args)))]
                [else
                 (let-values ([(width i args)
                               (parse-printf-number fmt i args)])
                   (let-values ([(prec i args)
                                 (if (and (< i flen)
                                          (char=? (string-ref fmt i) #\.))
                                     (let-values ([(p j a)
                                                   (parse-printf-number
                                                     fmt
                                                     (+ i 1)
                                                     args)])
                                       (values (or p 0) j a))
                                     (values #f i args))])
                     (if (and (< i flen) (char=? (string-ref fmt i) #\())
                         (let ([close (string-find-char-from
                                        fmt
                                        #\)
                                        (+ i 1))])
                           (if (and close
                                    (< (+ close 1) flen)
                                    (char=?
                                      (string-ref fmt (+ close 1))
                                      #\T))
                               (let* ([strfmt (substring
                                                fmt
                                                (+ i 1)
                                                close)])
                                 (let* ([arg (if (pair? args)
                                                 (car args)
                                                 "")])
                                   (let* ([rest (if (pair? args)
                                                    (cdr args)
                                                    (list))])
                                     (let* ([epoch (cond
                                                     [(string=? arg "")
                                                      (inexact->exact
                                                        (floor
                                                          (let ([t (current-time)])
                                                            (if (time? t)
                                                                (+ (time-second
                                                                     t)
                                                                   (/ (time-nanosecond
                                                                        t)
                                                                      1000000000.0))
                                                                t))))]
                                                     [(string=? arg "-1")
                                                      (inexact->exact
                                                        (floor
                                                          (let ([t (current-time)])
                                                            (if (time? t)
                                                                (+ (time-second
                                                                     t)
                                                                   (/ (time-nanosecond
                                                                        t)
                                                                      1000000000.0))
                                                                t))))]
                                                     [(string=? arg "-2")
                                                      (inexact->exact
                                                        (floor
                                                          (let ([t (current-time)])
                                                            (if (time? t)
                                                                (+ (time-second
                                                                     t)
                                                                   (/ (time-nanosecond
                                                                        t)
                                                                      1000000000.0))
                                                                t))))]
                                                     [else
                                                      (or (string->number
                                                            arg)
                                                          0)])])
                                       (let* ([result (ffi-strftime
                                                        strfmt
                                                        epoch)])
                                         (let* ([result (if prec
                                                            (substring
                                                              result
                                                              0
                                                              (min prec
                                                                   (string-length
                                                                     result)))
                                                            result)])
                                           (display
                                             (pad-string
                                               result
                                               (or width 0)
                                               left-align?
                                               #\space)
                                             buf)
                                           (values (+ close 2) rest)))))))
                               (values i args)))
                         (if (>= i flen)
                             (values i args)
                             (let ([spec (string-ref fmt i)]
                                   [arg (if (pair? args) (car args) "")]
                                   [rest (if (pair? args)
                                             (cdr args)
                                             (list))])
                               (case spec
                                 [(#\%)
                                  (display "%" buf)
                                  (values (+ i 1) args)]
                                 [(#\s)
                                  (let* ([s (if (string? arg)
                                                arg
                                                (if (pair? args) arg ""))])
                                    (let* ([s (if prec
                                                  (substring
                                                    s
                                                    0
                                                    (min prec
                                                         (string-length
                                                           s)))
                                                  s)])
                                      (display
                                        (pad-string
                                          s
                                          (or width 0)
                                          left-align?
                                          #\space)
                                        buf)
                                      (values (+ i 1) rest)))]
                                 [(#\d #\i)
                                  (let* ([n (string->integer-safe arg)])
                                    (let* ([n (cond
                                                [(> n 9223372036854775807)
                                                 9223372036854775807]
                                                [(< n -9223372036854775808)
                                                 -9223372036854775808]
                                                [else n])])
                                      (let* ([neg? (< n 0)])
                                        (let* ([digits (number->string
                                                         (abs n))])
                                          (let* ([digits (if (and prec
                                                                  (> prec
                                                                     (string-length
                                                                       digits)))
                                                             (string-append
                                                               (make-string
                                                                 (- prec
                                                                    (string-length
                                                                      digits))
                                                                 #\0)
                                                               digits)
                                                             digits)])
                                            (let* ([digits (if (and prec
                                                                    (= prec
                                                                       0)
                                                                    (= n
                                                                       0))
                                                               ""
                                                               digits)])
                                              (let* ([s (if neg?
                                                            (string-append
                                                              "-"
                                                              digits)
                                                            digits)])
                                                (let* ([s (if (and plus-sign?
                                                                   (not neg?))
                                                              (string-append
                                                                "+"
                                                                s)
                                                              s)])
                                                  (let* ([s (if (and space-sign?
                                                                     (not neg?)
                                                                     (not plus-sign?))
                                                                (string-append
                                                                  " "
                                                                  s)
                                                                s)])
                                                    (let* ([pad-ch (if (and zero-pad?
                                                                            (not left-align?)
                                                                            (not prec))
                                                                       #\0
                                                                       #\space)])
                                                      (display
                                                        (pad-string
                                                          s
                                                          (or width 0)
                                                          left-align?
                                                          pad-ch)
                                                        buf)
                                                      (values
                                                        (+ i 1)
                                                        rest)))))))))))]
                                 [(#\u)
                                  (let* ([n (string->integer-safe arg)])
                                    (let* ([neg? (< n 0)])
                                      (let* ([n (if neg?
                                                    (+ (expt 2 64) n)
                                                    n)])
                                        (let* ([n (cond
                                                    [(and neg? (<= n 0))
                                                     18446744073709551615]
                                                    [(> n
                                                        18446744073709551615)
                                                     18446744073709551615]
                                                    [else n])])
                                          (let* ([digits (number->string
                                                           n)])
                                            (let* ([digits (if (and prec
                                                                    (> prec
                                                                       (string-length
                                                                         digits)))
                                                               (string-append
                                                                 (make-string
                                                                   (- prec
                                                                      (string-length
                                                                        digits))
                                                                   #\0)
                                                                 digits)
                                                               digits)])
                                              (let* ([digits (if (and prec
                                                                      (= prec
                                                                         0)
                                                                      (= n
                                                                         0))
                                                                 ""
                                                                 digits)])
                                                (let* ([pad-ch (if (and zero-pad?
                                                                        (not left-align?)
                                                                        (not prec))
                                                                   #\0
                                                                   #\space)])
                                                  (display
                                                    (pad-string
                                                      digits
                                                      (or width 0)
                                                      left-align?
                                                      pad-ch)
                                                    buf)
                                                  (values
                                                    (+ i 1)
                                                    rest)))))))))]
                                 [(#\o)
                                  (let* ([n (string->integer-safe arg)])
                                    (let* ([n (if (< n 0)
                                                  (+ (expt 2 64) n)
                                                  n)])
                                      (let* ([n (if (> n
                                                       18446744073709551615)
                                                    18446744073709551615
                                                    n)])
                                        (let* ([digits (number->string
                                                         n
                                                         8)])
                                          (let* ([digits (if (and prec
                                                                  (> prec
                                                                     (string-length
                                                                       digits)))
                                                             (string-append
                                                               (make-string
                                                                 (- prec
                                                                    (string-length
                                                                      digits))
                                                                 #\0)
                                                               digits)
                                                             digits)])
                                            (let* ([digits (if (and prec
                                                                    (= prec
                                                                       0)
                                                                    (= n
                                                                       0))
                                                               ""
                                                               digits)])
                                              (let* ([s (if (and alt-form?
                                                                 (not (string=?
                                                                        digits
                                                                        ""))
                                                                 (not (char=?
                                                                        (string-ref
                                                                          digits
                                                                          0)
                                                                        #\0)))
                                                            (string-append
                                                              "0"
                                                              digits)
                                                            digits)])
                                                (let* ([pad-ch (if (and zero-pad?
                                                                        (not left-align?)
                                                                        (not prec))
                                                                   #\0
                                                                   #\space)])
                                                  (display
                                                    (pad-string
                                                      s
                                                      (or width 0)
                                                      left-align?
                                                      pad-ch)
                                                    buf)
                                                  (values
                                                    (+ i 1)
                                                    rest)))))))))]
                                 [(#\x #\X)
                                  (let* ([n (string->integer-safe arg)])
                                    (let* ([n (if (< n 0)
                                                  (+ (expt 2 64) n)
                                                  n)])
                                      (let* ([n (if (> n
                                                       18446744073709551615)
                                                    18446744073709551615
                                                    n)])
                                        (let* ([raw (string-downcase
                                                      (number->string
                                                        n
                                                        16))])
                                          (let* ([raw (if (char=? spec #\X)
                                                          (string-upcase
                                                            raw)
                                                          raw)])
                                            (let* ([digits (if (and prec
                                                                    (> prec
                                                                       (string-length
                                                                         raw)))
                                                               (string-append
                                                                 (make-string
                                                                   (- prec
                                                                      (string-length
                                                                        raw))
                                                                   #\0)
                                                                 raw)
                                                               raw)])
                                              (let* ([digits (if (and prec
                                                                      (= prec
                                                                         0)
                                                                      (= n
                                                                         0))
                                                                 ""
                                                                 digits)])
                                                (let* ([s (if (and alt-form?
                                                                   (not (= n
                                                                           0)))
                                                              (string-append
                                                                (if (char=?
                                                                      spec
                                                                      #\X)
                                                                    "0X"
                                                                    "0x")
                                                                digits)
                                                              digits)])
                                                  (let* ([pad-ch (if (and zero-pad?
                                                                          (not left-align?)
                                                                          (not prec))
                                                                     #\0
                                                                     #\space)])
                                                    (display
                                                      (pad-string
                                                        s
                                                        (or width 0)
                                                        left-align?
                                                        pad-ch)
                                                      buf)
                                                    (values
                                                      (+ i 1)
                                                      rest))))))))))]
                                 [(#\c)
                                  (when (and (string? arg)
                                             (> (string-length arg) 0))
                                    (let* ([ch (string-ref arg 0)])
                                      (let* ([cp (char->integer ch)])
                                        (if (< cp 128)
                                            (write-u8 cp buf)
                                            (let ([tmp (open-output-u8vector
                                                         (list
                                                           'char-encoding:
                                                           'UTF-8))])
                                              (display ch tmp)
                                              (let ([bytes (get-output-u8vector
                                                             tmp)])
                                                (when (> (u8vector-length
                                                           bytes)
                                                         0)
                                                  (write-u8
                                                    (u8vector-ref bytes 0)
                                                    buf))))))))
                                  (values (+ i 1) rest)]
                                 [(#\b)
                                  (let ([stopped? (printf-interpret-b-escapes
                                                    (if (string? arg)
                                                        arg
                                                        "")
                                                    buf)])
                                    (when stopped? (*printf-stop* #t))
                                    (values (+ i 1) rest))]
                                 [(#\q)
                                  (let ([quoted (shell-quote-string
                                                  (if (string? arg)
                                                      arg
                                                      ""))])
                                    (display
                                      (pad-string
                                        quoted
                                        (or width 0)
                                        left-align?
                                        #\space)
                                      buf))
                                  (values (+ i 1) rest)]
                                 [(#\f #\F #\e #\g #\E #\G)
                                  (let* ([n (string->number-safe arg)])
                                    (let* ([p (or prec 6)])
                                      (let* ([s (format-float
                                                  n
                                                  spec
                                                  p
                                                  alt-form?)])
                                        (display
                                          (pad-string
                                            s
                                            (or width 0)
                                            left-align?
                                            (if (and zero-pad?
                                                     (not left-align?))
                                                #\0
                                                #\space))
                                          buf)
                                        (values (+ i 1) rest))))]
                                 [else
                                  (fprintf
                                    (current-error-port)
                                    "printf: %~a: invalid format character~n"
                                    (string spec))
                                  (*printf-conversion-error* #t)
                                  (values (+ i 1) args)]))))))]))))))
  (define (parse-printf-number fmt i args)
    (let ([flen (string-length fmt)])
      (if (>= i flen)
          (values #f i args)
          (if (char=? (string-ref fmt i) #\*)
              (let ([n (if (pair? args)
                           (string->integer-safe (car args))
                           0)]
                    [rest (if (pair? args) (cdr args) (list))])
                (values (abs n) (+ i 1) rest))
              (let digit-loop ([j i] [n 0] [found? #f])
                (if (and (< j flen) (char-numeric? (string-ref fmt j)))
                    (digit-loop
                      (+ j 1)
                      (+ (* n 10)
                         (- (char->integer (string-ref fmt j)) 48))
                      #t)
                    (values (if found? n #f) j args)))))))
  (define (u8vector->string-lossy vec)
    (guard (__exn
             [#t
              ((lambda (_)
                 (list->string (map integer->char (u8vector->list vec))))
                __exn)])
      (let ([p (open-input-u8vector
                 (list 'init: vec 'char-encoding: 'UTF-8))])
        (let loop ([chars '()])
          (let ([ch (read-char p)])
            (if (eof-object? ch)
                (list->string (reverse chars))
                (loop (cons ch chars))))))))
  (define (hex-digit-value ch)
    (cond
      [(and (char>=? ch #\0) (char<=? ch #\9))
       (- (char->integer ch) 48)]
      [(and (char>=? ch #\a) (char<=? ch #\f))
       (- (char->integer ch) 87)]
      [(and (char>=? ch #\A) (char<=? ch #\F))
       (- (char->integer ch) 55)]
      [else #f]))
  (define (display-unicode-char n buf)
    (if (and (>= n 0) (<= n 1114111))
        (display (string (integer->char n)) buf)
        (display (string (integer->char 65533)) buf)))
  (define (printf-escape fmt i buf)
    (let ([flen (string-length fmt)])
      (if (>= i flen)
          (begin (display "\\" buf) i)
          (let ([ch (string-ref fmt i)])
            (case ch
              [(#\n) (display "\n" buf) (+ i 1)]
              [(#\t) (display "\t" buf) (+ i 1)]
              [(#\r) (display "\r" buf) (+ i 1)]
              [(#\a) (display "\a" buf) (+ i 1)]
              [(#\b) (display "\b" buf) (+ i 1)]
              [(#\f) (display "\f" buf) (+ i 1)]
              [(#\v) (display "\v" buf) (+ i 1)]
              [(#\\) (display "\\" buf) (+ i 1)]
              [(#\') (display "'" buf) (+ i 1)]
              [(#\") (display "\"" buf) (+ i 1)]
              [(#\0)
               (let octal-loop ([j (+ i 1)] [n 0] [count 0])
                 (if (and (< j flen)
                          (< count 2)
                          (char>=? (string-ref fmt j) #\0)
                          (char<=? (string-ref fmt j) #\7))
                     (octal-loop
                       (+ j 1)
                       (+ (* n 8)
                          (- (char->integer (string-ref fmt j)) 48))
                       (+ count 1))
                     (begin (write-u8 (modulo n 256) buf) j)))]
              [(#\e #\E)
               (display (string (integer->char 27)) buf)
               (+ i 1)]
              [(#\x)
               (let hex-loop ([j (+ i 1)] [n 0] [count 0])
                 (if (and (< j flen) (< count 2))
                     (let ([hch (string-ref fmt j)])
                       (cond
                         [(and (char>=? hch #\0) (char<=? hch #\9))
                          (hex-loop
                            (+ j 1)
                            (+ (* n 16) (- (char->integer hch) 48))
                            (+ count 1))]
                         [(and (char>=? hch #\a) (char<=? hch #\f))
                          (hex-loop
                            (+ j 1)
                            (+ (* n 16) (- (char->integer hch) 87))
                            (+ count 1))]
                         [(and (char>=? hch #\A) (char<=? hch #\F))
                          (hex-loop
                            (+ j 1)
                            (+ (* n 16) (- (char->integer hch) 55))
                            (+ count 1))]
                         [else (write-u8 n buf) j]))
                     (begin (write-u8 n buf) j)))]
              [(#\u)
               (let hex-loop ([j (+ i 1)] [n 0] [count 0])
                 (if (and (< j flen) (< count 4))
                     (let* ([hch (string-ref fmt j)])
                       (let* ([hv (hex-digit-value hch)])
                         (if hv
                             (hex-loop (+ j 1) (+ (* n 16) hv) (+ count 1))
                             (begin (display-unicode-char n buf) j))))
                     (begin (display-unicode-char n buf) j)))]
              [(#\U)
               (let hex-loop ([j (+ i 1)] [n 0] [count 0])
                 (if (and (< j flen) (< count 8))
                     (let* ([hch (string-ref fmt j)])
                       (let* ([hv (hex-digit-value hch)])
                         (if hv
                             (hex-loop (+ j 1) (+ (* n 16) hv) (+ count 1))
                             (begin (display-unicode-char n buf) j))))
                     (begin (display-unicode-char n buf) j)))]
              [(#\1 #\2 #\3 #\4 #\5 #\6 #\7)
               (let octal-loop ([j i] [n 0] [count 0])
                 (if (and (< j flen)
                          (< count 3)
                          (char>=? (string-ref fmt j) #\0)
                          (char<=? (string-ref fmt j) #\7))
                     (octal-loop
                       (+ j 1)
                       (+ (* n 8)
                          (- (char->integer (string-ref fmt j)) 48))
                       (+ count 1))
                     (begin (write-u8 (bitwise-and n 255) buf) j)))]
              [(#\c) (+ i 1)]
              [else
               (display "\\" buf)
               (display (string ch) buf)
               (+ i 1)])))))
  (define (printf-interpret-b-escapes str buf)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (if (>= i len)
            #f
            (let ([ch (string-ref str i)])
              (if (char=? ch #\\)
                  (if (< (+ i 1) len)
                      (let ([next-ch (string-ref str (+ i 1))])
                        (cond
                          [(char=? next-ch #\c) #t]
                          [(char=? next-ch #\0)
                           (let oloop ([j (+ i 2)] [val 0] [count 0])
                             (if (and (< j len)
                                      (< count 3)
                                      (char>=? (string-ref str j) #\0)
                                      (char<=? (string-ref str j) #\7))
                                 (oloop
                                   (+ j 1)
                                   (+ (* val 8)
                                      (- (char->integer (string-ref str j))
                                         48))
                                   (+ count 1))
                                 (begin
                                   (write-u8 (modulo val 256) buf)
                                   (loop j))))]
                          [(and (char>=? next-ch #\1)
                                (char<=? next-ch #\7))
                           (let oloop ([j (+ i 1)] [val 0] [count 0])
                             (if (and (< j len)
                                      (< count 3)
                                      (char>=? (string-ref str j) #\0)
                                      (char<=? (string-ref str j) #\7))
                                 (oloop
                                   (+ j 1)
                                   (+ (* val 8)
                                      (- (char->integer (string-ref str j))
                                         48))
                                   (+ count 1))
                                 (begin
                                   (write-u8 (modulo val 256) buf)
                                   (loop j))))]
                          [else
                           (let ([next (printf-escape str (+ i 1) buf)])
                             (loop next))]))
                      (begin (display "\\" buf) (loop (+ i 1))))
                  (begin (display ch buf) (loop (+ i 1)))))))))
  (define (pad-string s width left-align? pad-ch)
    (let ([len (string-length s)])
      (if (<= width len)
          s
          (let ([padding (make-string (- width len) pad-ch)])
            (if left-align?
                (string-append s padding)
                (if (and (char=? pad-ch #\0)
                         (> (string-length s) 0)
                         (or (char=? (string-ref s 0) #\-)
                             (char=? (string-ref s 0) #\+)))
                    (string-append
                      (substring s 0 1)
                      padding
                      (substring s 1 (string-length s)))
                    (string-append padding s)))))))
  (define *printf-conversion-error* (make-parameter #f))
  (define *printf-stop* (make-parameter #f))
  (define (string->integer-safe s)
    (if (or (not (string? s)) (string=? s ""))
        0
        (let* ([s (let trim ([i 0])
                    (if (and (< i (string-length s))
                             (or (char=? (string-ref s i) #\space)
                                 (char=? (string-ref s i) #\tab)))
                        (trim (+ i 1))
                        (substring s i (string-length s))))])
          (cond
            [(string=? s "") 0]
            [(or (char=? (string-ref s 0) #\')
                 (char=? (string-ref s 0) #\"))
             (if (>= (string-length s) 2)
                 (char->integer (string-ref s 1))
                 0)]
            [(let loop ([i 0])
               (and (< i (string-length s))
                    (or (char=? (string-ref s i) #\#) (loop (+ i 1)))))
             (fprintf
               (current-error-port)
               "printf: ~a: invalid number~n"
               s)
             (*printf-conversion-error* #t)
             (parse-leading-integer s)]
            [else
             (let* ([neg? (and (> (string-length s) 0)
                               (char=? (string-ref s 0) #\-))])
               (let* ([pos? (and (> (string-length s) 0)
                                 (char=? (string-ref s 0) #\+))])
                 (let* ([s2 (if (or neg? pos?)
                                (substring s 1 (string-length s))
                                s)])
                   (let ([strict-val (cond
                                       [(string=? s2 "") #f]
                                       [(and (>= (string-length s2) 2)
                                             (char=? (string-ref s2 0) #\0)
                                             (or (char=?
                                                   (string-ref s2 1)
                                                   #\x)
                                                 (char=?
                                                   (string-ref s2 1)
                                                   #\X)))
                                        (string->number
                                          (substring
                                            s2
                                            2
                                            (string-length s2))
                                          16)]
                                       [(and (> (string-length s2) 1)
                                             (char=?
                                               (string-ref s2 0)
                                               #\0))
                                        (or (string->number s2 8)
                                            (string->number s2))]
                                       [else (string->number s2)])])
                     (if strict-val
                         (if neg? (- strict-val) strict-val)
                         (let ([partial (parse-leading-integer s2)])
                           (fprintf
                             (current-error-port)
                             "printf: ~a: invalid number~n"
                             s)
                           (*printf-conversion-error* #t)
                           (if neg? (- partial) partial)))))))]))))
  (define (parse-leading-integer s)
    (let loop ([i 0] [n 0])
      (if (and (< i (string-length s))
               (char>=? (string-ref s i) #\0)
               (char<=? (string-ref s i) #\9))
          (loop
            (+ i 1)
            (+ (* n 10) (- (char->integer (string-ref s i)) 48)))
          n)))
  (define (string->number-safe s)
    (if (string? s) (or (string->number s) 0.0) 0.0))
  (define format-float
    (case-lambda
      [(n spec precision)
       (let* ([alt-form? #f])
         (let ([n (inexact->exact (if (number? n) n 0))])
           (let ([n (exact->inexact n)])
             (case spec
               [(#\f #\F)
                (let ([s (format-fixed n precision)])
                  (if alt-form? (ensure-decimal-point s) s))]
               [(#\e #\E)
                (format-scientific n precision (char=? spec #\E))]
               [(#\g #\G)
                (let* ([p (max 1 (or precision 6))])
                  (let* ([e (if (= n 0.0)
                                0
                                (inexact->exact
                                  (floor (/ (log (abs n)) (log 10)))))])
                    (if alt-form?
                        (if (or (< e -4) (>= e p))
                            (ensure-decimal-point
                              (format-scientific
                                n
                                (- p 1)
                                (char=? spec #\G)))
                            (ensure-decimal-point
                              (format-fixed
                                n
                                (max 0 (- p 1 (inexact->exact e))))))
                        (if (or (< e -4) (>= e p))
                            (strip-trailing-zeros
                              (format-scientific
                                n
                                (- p 1)
                                (char=? spec #\G)))
                            (strip-trailing-zeros
                              (format-fixed
                                n
                                (max 0 (- p 1 (inexact->exact e)))))))))]
               [else (number->string n)]))))]
      [(n spec precision alt-form?)
       (let ([n (inexact->exact (if (number? n) n 0))])
         (let ([n (exact->inexact n)])
           (case spec
             [(#\f #\F)
              (let ([s (format-fixed n precision)])
                (if alt-form? (ensure-decimal-point s) s))]
             [(#\e #\E)
              (format-scientific n precision (char=? spec #\E))]
             [(#\g #\G)
              (let* ([p (max 1 (or precision 6))])
                (let* ([e (if (= n 0.0)
                              0
                              (inexact->exact
                                (floor (/ (log (abs n)) (log 10)))))])
                  (if alt-form?
                      (if (or (< e -4) (>= e p))
                          (ensure-decimal-point
                            (format-scientific
                              n
                              (- p 1)
                              (char=? spec #\G)))
                          (ensure-decimal-point
                            (format-fixed
                              n
                              (max 0 (- p 1 (inexact->exact e))))))
                      (if (or (< e -4) (>= e p))
                          (strip-trailing-zeros
                            (format-scientific
                              n
                              (- p 1)
                              (char=? spec #\G)))
                          (strip-trailing-zeros
                            (format-fixed
                              n
                              (max 0 (- p 1 (inexact->exact e)))))))))]
             [else (number->string n)])))]))
  (define (string-contains-char? s ch)
    (let loop ([i 0])
      (and (< i (string-length s))
           (or (char=? (string-ref s i) ch) (loop (+ i 1))))))
  (define (ensure-decimal-point s)
    (if (string-contains-char? s #\.) s (string-append s ".")))
  (define (strip-trailing-zeros s)
    (if (string-contains-char? s #\.)
        (let* ([s (let loop ([i (- (string-length s) 1)])
                    (if (and (>= i 0) (char=? (string-ref s i) #\0))
                        (loop (- i 1))
                        (substring s 0 (+ i 1))))])
          (let* ([s (if (and (> (string-length s) 0)
                             (char=?
                               (string-ref s (- (string-length s) 1))
                               #\.))
                        (substring s 0 (- (string-length s) 1))
                        s)])
            s))
        s))
  (define (format-fixed n precision)
    (let* ([s (number->string (exact->inexact n))])
      (let* ([dot-pos (string-find-char* s #\.)])
        (if dot-pos
            (let* ([int-part (substring s 0 dot-pos)])
              (let* ([frac-part (substring
                                  s
                                  (+ dot-pos 1)
                                  (string-length s))])
                (let* ([frac-len (string-length frac-part)])
                  (cond
                    [(= precision 0) int-part]
                    [(<= frac-len precision)
                     (string-append
                       int-part
                       "."
                       frac-part
                       (make-string (- precision frac-len) #\0))]
                    [else
                     (string-append
                       int-part
                       "."
                       (substring frac-part 0 precision))]))))
            (if (= precision 0)
                s
                (string-append s "." (make-string precision #\0)))))))
  (define (format-scientific n precision upper?)
    (if (= n 0.0)
        (string-append
          "0."
          (make-string (or precision 6) #\0)
          (if upper? "E+00" "e+00"))
        (let* ([sign (if (< n 0) -1 1)])
          (let* ([abs-n (abs n)])
            (let* ([exp (floor (/ (log abs-n) (log 10)))])
              (let* ([mantissa (* sign (/ abs-n (expt 10.0 exp)))])
                (let* ([mstr (format-fixed
                               (exact->inexact mantissa)
                               (or precision 6))])
                  (let* ([echar (if upper? "E" "e")])
                    (let* ([esign (if (>= exp 0) "+" "-")])
                      (let* ([estr (number->string
                                     (inexact->exact (abs exp)))])
                        (string-append
                          mstr
                          echar
                          esign
                          (if (< (string-length estr) 2)
                              (string-append "0" estr)
                              estr))))))))))))
  (define (shell-quote-string s)
    (if (string=? s "")
        "''"
        (let* ([has-control? (let loop ([i 0])
                               (if (>= i (string-length s))
                                   #f
                                   (let ([ch (string-ref s i)])
                                     (if (or (< (char->integer ch) 32)
                                             (char=? ch #\delete))
                                         #t
                                         (loop (+ i 1))))))])
          (let* ([needs-quoting? (let loop ([i 0])
                                   (if (>= i (string-length s))
                                       #f
                                       (let ([ch (string-ref s i)])
                                         (if (or (char-alphabetic? ch)
                                                 (char-numeric? ch)
                                                 (char=? ch #\_)
                                                 (char=? ch #\/)
                                                 (char=? ch #\.)
                                                 (char=? ch #\-)
                                                 (char=? ch #\+)
                                                 (char=? ch #\,)
                                                 (char=? ch #\:)
                                                 (char=? ch #\@)
                                                 (char=? ch #\%))
                                             (loop (+ i 1))
                                             #t))))])
            (cond
              [(not needs-quoting?) s]
              [has-control?
               (let ([buf (open-output-string)])
                 (display "$'" buf)
                 (let loop ([i 0])
                   (when (< i (string-length s))
                     (let ([ch (string-ref s i)])
                       (cond
                         [(char=? ch #\') (display "\\'" buf)]
                         [(char=? ch #\\) (display "\\\\" buf)]
                         [(char=? ch #\newline) (display "\\n" buf)]
                         [(char=? ch #\tab) (display "\\t" buf)]
                         [(char=? ch #\return) (display "\\r" buf)]
                         [(char=? ch (integer->char 7))
                          (display "\\a" buf)]
                         [(char=? ch (integer->char 8))
                          (display "\\b" buf)]
                         [(char=? ch (integer->char 27))
                          (display "\\e" buf)]
                         [(or (char=? ch #\delete)
                              (< (char->integer ch) 32))
                          (let ([oct (number->string
                                       (char->integer ch)
                                       8)])
                            (display
                              (string-append
                                "\\"
                                (make-string (- 3 (string-length oct)) #\0)
                                oct)
                              buf))]
                         [else (display ch buf)]))
                     (loop (+ i 1))))
                 (display "'" buf)
                 (get-output-string buf))]
              [else
               (let ([buf (open-output-string)])
                 (let loop ([i 0])
                   (when (< i (string-length s))
                     (let ([ch (string-ref s i)])
                       (if (or (char-alphabetic? ch)
                               (char-numeric? ch)
                               (char=? ch #\_)
                               (char=? ch #\/)
                               (char=? ch #\.)
                               (char=? ch #\-)
                               (char=? ch #\+)
                               (char=? ch #\,)
                               (char=? ch #\:)
                               (char=? ch #\@)
                               (char=? ch #\%))
                           (display ch buf)
                           (begin (display #\\ buf) (display ch buf))))
                     (loop (+ i 1))))
                 (get-output-string buf))])))))
  (define (apply-set-options! env flag-str enable?)
    (let ([len (string-length flag-str)])
      (let loop ([i 1])
        (when (< i len)
          (let ([ch (string-ref flag-str i)])
            (case ch
              [(#\e) (env-option-set! env "errexit" enable?)]
              [(#\f) (env-option-set! env "noglob" enable?)]
              [(#\h) (env-option-set! env "hashall" enable?)]
              [(#\m) (env-option-set! env "monitor" enable?)]
              [(#\C) (env-option-set! env "noclobber" enable?)]
              [(#\u) (env-option-set! env "nounset" enable?)]
              [(#\x) (env-option-set! env "xtrace" enable?)]
              [(#\v) (env-option-set! env "verbose" enable?)]
              [(#\n) (env-option-set! env "noexec" enable?)]
              [(#\a) (env-option-set! env "allexport" enable?)]
              [else (%%void)])
            (loop (+ i 1)))))))
  (define (split-by-ifs str ifs max-fields)
    (let ([len (string-length str)])
      (let loop ([i 0] [start 0] [fields (list)] [count 1])
        (cond
          [(>= i len)
           (reverse
             (if (> i start)
                 (cons (substring str start i) fields)
                 fields))]
          [(and (< count max-fields)
                (ifs-member? (string-ref str i) ifs))
           (loop
             (+ i 1)
             (+ i 1)
             (if (> i start)
                 (cons (substring str start i) fields)
                 fields)
             (+ count 1))]
          [else (loop (+ i 1) start fields count)]))))
  (define (ifs-member? ch ifs)
    (let loop ([i 0])
      (and (< i (string-length ifs))
           (or (char=? ch (string-ref ifs i)) (loop (+ i 1))))))
  (define (string-join-sp lst)
    (if (null? lst)
        ""
        (call-with-output-string
          (lambda (port)
            (display (car lst) port)
            (for-each
              (lambda (s) (display " " port) (display s port))
              (cdr lst))))))
  (define (string-find-char* str ch)
    (let loop ([i 0])
      (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))])))
  (define (list-head lst n)
    (if (or (<= n 0) (null? lst))
        (list)
        (cons (car lst) (list-head (cdr lst) (- n 1)))))
  (define (last-elem* lst)
    (if (null? (cdr lst)) (car lst) (last-elem* (cdr lst))))
  (define (butlast lst)
    (if (null? (cdr lst))
        (list)
        (cons (car lst) (butlast (cdr lst)))))
  (define (env-exported-alist-pairs env)
    (let ([result (list)])
      (for-each
        (lambda (pair)
          (let ([name (car pair)] [var (cdr pair)])
            (when (shell-var-exported? var)
              (set! result
                (cons
                  (cons name (or (shell-var-scalar-value var) ""))
                  result)))))
        (hash->list (shell-environment-vars env)))
      result))
  (define (arith-eval-wrapper expr env)
    (guard (__exn [#t ((lambda (e) 0) __exn)])
      (arith-eval
        expr
        (arith-env-getter env)
        (arith-env-setter env))))
  (defbuiltin ":" 0)
  (defbuiltin "true" 0)
  (defbuiltin "false" 1)
  (defbuiltin
    "echo"
    (let loop ([args args] [newline? #t] [escape? #f])
      (if (and (pair? args)
               (> (string-length (car args)) 1)
               (char=? (string-ref (car args) 0) #\-)
               (let ([s (car args)])
                 (let valid? ([j 1])
                   (if (>= j (string-length s))
                       #t
                       (let ([c (string-ref s j)])
                         (and (or (char=? c #\n)
                                  (char=? c #\e)
                                  (char=? c #\E))
                              (valid? (+ j 1))))))))
          (let* ([s (car args)])
            (let* ([has-n? (let find ([j 1])
                             (if (>= j (string-length s))
                                 #f
                                 (or (char=? (string-ref s j) #\n)
                                     (find (+ j 1)))))])
              (let* ([has-e? (let find ([j 1])
                               (if (>= j (string-length s))
                                   #f
                                   (or (char=? (string-ref s j) #\e)
                                       (find (+ j 1)))))])
                (let* ([has-E? (let find ([j 1])
                                 (if (>= j (string-length s))
                                     #f
                                     (or (char=? (string-ref s j) #\E)
                                         (find (+ j 1)))))])
                  (loop
                    (cdr args)
                    (if has-n? #f newline?)
                    (cond [has-E? #f] [has-e? #t] [else escape?]))))))
          (call/cc
            (lambda (stop)
              (let arg-loop ([rest args] [first? #t])
                (when (pair? rest)
                  (unless first? (display " "))
                  (if escape?
                      (let ([result (echo-expand-escapes (car rest))])
                        (display-raw-bytes (car result))
                        (when (cdr result)
                          (flush-output-port (current-output-port))
                          (stop 0)))
                      (shell-display (car rest)))
                  (arg-loop (cdr rest) #f)))
              (when newline? (newline))
              (flush-output-port (current-output-port))
              0)))))
  (defbuiltin
    "printf"
    (if (null? args)
        (begin
          (fprintf
            (current-error-port)
            "printf: usage: printf [-v var] format [arguments]~n")
          2)
        (let loop ([rest args] [var-name #f])
          (cond
            [(null? rest)
             (begin
               (fprintf
                 (current-error-port)
                 "printf: usage: printf [-v var] format [arguments]~n")
               2)]
            [(string=? (car rest) "--") (loop (cdr rest) var-name)]
            [(and (not var-name)
                  (>= (length rest) 2)
                  (string=? (car rest) "-v"))
             (loop (cddr rest) (cadr rest))]
            [else
             (let ([fmt (car rest)] [fmt-args (cdr rest)])
               (parameterize ([*printf-conversion-error* #f])
                 (let ([output (shell-printf fmt fmt-args)])
                   (if var-name
                       (let ([str-output (u8vector->string-lossy output)])
                         (let ([bracket (string-index var-name #\[)])
                           (if bracket
                               (if (and (> (string-length var-name)
                                           (+ bracket 1))
                                        (char=?
                                          (string-ref
                                            var-name
                                            (- (string-length var-name) 1))
                                          #\]))
                                   (let ([arr-name (substring
                                                     var-name
                                                     0
                                                     bracket)]
                                         [idx-str (substring
                                                    var-name
                                                    (+ bracket 1)
                                                    (- (string-length
                                                         var-name)
                                                       1))])
                                     (env-array-set!
                                       env
                                       arr-name
                                       idx-str
                                       str-output)
                                     (if (*printf-conversion-error*) 1 0))
                                   (begin
                                     (fprintf
                                       (current-error-port)
                                       "printf: `~a': not a valid identifier~n"
                                       var-name)
                                     2))
                               (begin
                                 (env-set! env var-name str-output)
                                 (if (*printf-conversion-error*) 1 0)))))
                       (begin
                         (write-subu8vector
                           output
                           0
                           (u8vector-length output)
                           (current-output-port))
                         (flush-output-port (current-output-port))
                         (if (*printf-conversion-error*) 1 0))))))]))))
  (defbuiltin
    "cd"
    (let loop ([rest args] [physical? #f])
      (cond
        [(or (null? rest)
             (string=? (car rest) "--")
             (not (and (> (string-length (car rest)) 0)
                       (char=? (string-ref (car rest) 0) #\-)))
             (string=? (car rest) "-"))
         (let* ([remaining (if (and (pair? rest)
                                    (string=? (car rest) "--"))
                               (cdr rest)
                               rest)])
           (if (and (pair? remaining) (pair? (cdr remaining)))
               (begin
                 (fprintf (current-error-port) "cd: too many arguments~n")
                 1)
               (let* ([dir-arg (cond
                                 [(null? remaining)
                                  (let ([home (env-get env "HOME")])
                                    (if (or (not home) (string=? home ""))
                                        (begin
                                          (fprintf
                                            (current-error-port)
                                            "cd: HOME not set~n")
                                          #f)
                                        home))]
                                 [(string=? (car remaining) "-")
                                  (let ([oldpwd (env-get env "OLDPWD")])
                                    (if (not oldpwd)
                                        (begin
                                          (fprintf
                                            (current-error-port)
                                            "cd: OLDPWD not set~n")
                                          #f)
                                        oldpwd))]
                                 [else (car remaining)])])
                 (if (not dir-arg)
                     1
                     (let* ([expanded (expand-word-nosplit dir-arg env)])
                       (let* ([actual-dir (or (and (not (and (> (string-length
                                                                  expanded)
                                                                0)
                                                             (char=?
                                                               (string-ref
                                                                 expanded
                                                                 0)
                                                               #\/)))
                                                   (search-cdpath
                                                     expanded
                                                     env))
                                              expanded)])
                         (let* ([print-dir? (or (and (pair? remaining)
                                                     (string=?
                                                       (car remaining)
                                                       "-"))
                                                (and (not (string=?
                                                            actual-dir
                                                            expanded))
                                                     #t))])
                           (guard (__exn
                                    [#t
                                     ((lambda (e)
                                        (fprintf
                                          (current-error-port)
                                          "cd: ~a: No such file or directory~n"
                                          expanded)
                                        1)
                                       __exn)])
                             (let ([old-pwd (or (*internal-pwd*)
                                                (strip-trailing-slash
                                                  (or (env-get env "PWD")
                                                      (strip-trailing-slash
                                                        (current-directory)))))])
                               (let ([new-pwd (if physical?
                                                  (begin
                                                    (current-directory
                                                      actual-dir)
                                                    (strip-trailing-slash
                                                      (current-directory)))
                                                  (let ([logical (strip-trailing-slash
                                                                   (resolve-logical-path
                                                                     old-pwd
                                                                     actual-dir))])
                                                    (current-directory
                                                      actual-dir)
                                                    (current-directory
                                                      logical)
                                                    logical))])
                                 (env-set! env "OLDPWD" old-pwd)
                                 (env-export! env "OLDPWD")
                                 (env-set! env "PWD" new-pwd)
                                 (env-export! env "PWD")
                                 (*internal-pwd* new-pwd)
                                 (when print-dir?
                                   (begin (display new-pwd) (newline)))
                                 0))))))))))]
        [(string=? (car rest) "-L") (loop (cdr rest) #f)]
        [(string=? (car rest) "-P") (loop (cdr rest) #t)]
        [else (loop (cons "--" rest) physical?)])))
  (defbuiltin
    "pwd"
    (let ([physical? (and (pair? args)
                          (string=? (car args) "-P"))])
      (begin
        (display
          (strip-trailing-slash
            (if physical?
                (current-directory)
                (or (*internal-pwd*)
                    (strip-trailing-slash (current-directory))))))
        (newline)))
    0)
  (defbuiltin
    "export"
    (let ([print? #f] [remove? #f] [names (list)])
      (let loop ([args args])
        (when (pair? args)
          (let ([arg (car args)])
            (cond
              [(and (> (string-length arg) 1)
                    (char=? (string-ref arg 0) #\-)
                    (not (string=? arg "--")))
               (let floop ([i 1])
                 (when (< i (string-length arg))
                   (case (string-ref arg i)
                     [(#\p) (set! print? #t)]
                     [(#\n) (set! remove? #t)]
                     [else (void)])
                   (floop (+ i 1))))
               (loop (cdr args))]
              [(string=? arg "--") (set! names (append names (cdr args)))]
              [else
               (set! names (append names (list arg)))
               (loop (cdr args))]))))
      (cond
        [(null? names)
         (let* ([all-vars (collect-all-vars env)])
           (let* ([sorted (sort! (hash-keys all-vars) string<?)])
             (for-each
               (lambda (name)
                 (let ([var (hash-get all-vars name)])
                   (when (and var
                              (shell-var-exported? var)
                              (not (eq? (shell-var-value var)
                                        \x2B;unset-sentinel+)))
                     (display-declare-var name var))))
               sorted)))
         0]
        [print?
         (for-each
           (lambda (name)
             (let ([var (env-get-var env name)])
               (when (and var
                          (shell-var-exported? var)
                          (not (eq? (shell-var-value var)
                                    \x2B;unset-sentinel+)))
                 (display-declare-var name var))))
           names)
         0]
        [else
         (let ([status 0])
           (for-each
             (lambda (arg)
               (let ([eq-pos (string-find-char* arg #\=)])
                 (if eq-pos
                     (let* ([name (substring arg 0 eq-pos)])
                       (let* ([value (substring
                                       arg
                                       (+ eq-pos 1)
                                       (string-length arg))])
                         (if remove?
                             (begin
                               (fprintf
                                 (current-error-port)
                                 "gsh: export: ~a: not a valid identifier~n"
                                 arg)
                               (set! status 2))
                             (env-export! env name value))))
                     (if remove?
                         (let ([var (env-get-raw-var env arg)])
                           (when var
                             (shell-var-exported?-set! var #f)
                             (ffi-unsetenv arg)))
                         (env-export! env arg)))))
             names)
           status)])))
  (defbuiltin
    "unset"
    (let loop ([args args] [unset-func? #f] [unset-nameref? #f])
      (cond
        [(null? args) 0]
        [(string=? (car args) "-f") (loop (cdr args) #t #f)]
        [(string=? (car args) "-v") (loop (cdr args) #f #f)]
        [(string=? (car args) "-n") (loop (cdr args) #f #t)]
        [else
         (let ([status 0])
           (for-each
             (lambda (name)
               (cond
                 [unset-func? (function-unset! env name)]
                 [unset-nameref?
                  (if (not (valid-name? name))
                      (begin
                        (fprintf
                          (current-error-port)
                          "gsh: unset: `~a': not a valid identifier~n"
                          name)
                        (set! status 1))
                      (guard (__exn
                               [#t
                                ((lambda (e)
                                   (fprintf
                                     (current-error-port)
                                     "gsh: unset: ~a: cannot unset~n"
                                     name)
                                   (set! status 1))
                                  __exn)])
                        (env-unset-nameref! env name)))]
                 [else
                  (let* ([bracket-pos (string-find-char* name #\[)])
                    (let* ([base-name (if bracket-pos
                                          (substring name 0 bracket-pos)
                                          name)])
                      (if (not (valid-name? base-name))
                          (begin
                            (fprintf
                              (current-error-port)
                              "gsh: unset: `~a': not a valid identifier~n"
                              name)
                            (set! status 1))
                          (guard (__exn
                                   [#t
                                    ((lambda (e)
                                       (fprintf
                                         (current-error-port)
                                         "gsh: unset: ~a: cannot unset: readonly variable~n"
                                         name)
                                       (set! status 1))
                                      __exn)])
                            (if (and bracket-pos
                                     (> bracket-pos 0)
                                     (let ([close (string-find-char*
                                                    name
                                                    #\])])
                                       (and close
                                            (= close
                                               (- (string-length name)
                                                  1)))))
                                (let ([var-name (substring
                                                  name
                                                  0
                                                  bracket-pos)]
                                      [index (substring
                                               name
                                               (+ bracket-pos 1)
                                               (- (string-length name)
                                                  1))])
                                  (env-array-unset-element!
                                    env
                                    var-name
                                    index))
                                (let ([var (env-get-raw-var env name)])
                                  (if var
                                      (env-unset! env name)
                                      (function-unset! env name))))))))]))
             args)
           status)])))
  (defbuiltin
    "readonly"
    (let ([print? #f] [array? #f] [assoc? #f] [names (list)])
      (let loop ([args args])
        (if (null? args)
            (void)
            (let ([arg (car args)])
              (cond
                [(and (> (string-length arg) 1)
                      (char=? (string-ref arg 0) #\-))
                 (let floop ([i 1])
                   (when (< i (string-length arg))
                     (case (string-ref arg i)
                       [(#\p) (set! print? #t)]
                       [(#\a) (set! array? #t)]
                       [(#\A) (set! assoc? #t)]
                       [else (void)])
                     (floop (+ i 1))))
                 (loop (cdr args))]
                [else
                 (set! names (append names (list arg)))
                 (loop (cdr args))]))))
      (cond
        [(null? names)
         (let* ([all-vars (collect-all-vars env)])
           (let* ([sorted (sort! (hash-keys all-vars) string<?)])
             (for-each
               (lambda (name)
                 (let ([var (hash-get all-vars name)])
                   (when (and var
                              (shell-var-readonly? var)
                              (not (eq? (shell-var-value var)
                                        \x2B;unset-sentinel+)))
                     (display-declare-var name var))))
               sorted)))
         0]
        [print?
         (for-each
           (lambda (name)
             (let ([var (env-get-var env name)])
               (when (and var
                          (shell-var-readonly? var)
                          (not (eq? (shell-var-value var)
                                    \x2B;unset-sentinel+)))
                 (display-declare-var name var))))
           names)
         0]
        [else
         (for-each
           (lambda (arg)
             (let ([eq-pos (string-find-char* arg #\=)])
               (if eq-pos
                   (let* ([name (substring arg 0 eq-pos)])
                     (let* ([value (substring
                                     arg
                                     (+ eq-pos 1)
                                     (string-length arg))])
                       (if (and (or array? assoc?)
                                (> (string-length value) 0)
                                (char=? (string-ref value 0) #\())
                           (let* ([inner (if (and (> (string-length value)
                                                     1)
                                                  (char=?
                                                    (string-ref
                                                      value
                                                      (- (string-length
                                                           value)
                                                         1))
                                                    #\)))
                                             (substring
                                               value
                                               1
                                               (- (string-length value) 1))
                                             (substring
                                               value
                                               1
                                               (string-length value)))])
                             (let* ([tbl (make-hash-table)])
                               (let* ([existing (or (hash-get
                                                      (shell-environment-vars
                                                        env)
                                                      name)
                                                    (let ([v (make-shell-var tbl #f #f #t
                                                               #f #f #f #f
                                                               array?
                                                               assoc?)])
                                                      (hash-put!
                                                        (shell-environment-vars
                                                          env)
                                                        name
                                                        v)
                                                      v))])
                                 (when (> (string-length inner) 0)
                                   (let ([elems (parse-array-compound-elements
                                                  inner)])
                                     (if assoc?
                                         (for-each
                                           (lambda (elem)
                                             (let ([bracket-start (string-find-char*
                                                                    elem
                                                                    #\[)])
                                               (when bracket-start
                                                 (let* ([bracket-end (string-find-char-from
                                                                       elem
                                                                       #\]
                                                                       (+ bracket-start
                                                                          1))])
                                                   (let* ([key (substring
                                                                 elem
                                                                 (+ bracket-start
                                                                    1)
                                                                 (or bracket-end
                                                                     (string-length
                                                                       elem)))])
                                                     (let* ([eq (string-find-char-from
                                                                  elem
                                                                  #\=
                                                                  (or bracket-end
                                                                      0))])
                                                       (let* ([val (if eq
                                                                       (substring
                                                                         elem
                                                                         (+ eq
                                                                            1)
                                                                         (string-length
                                                                           elem))
                                                                       "")])
                                                         (hash-put!
                                                           tbl
                                                           key
                                                           val))))))))
                                           elems)
                                         (let loop ([es elems] [idx 0])
                                           (when (pair? es)
                                             (hash-put! tbl idx (car es))
                                             (loop (cdr es) (+ idx 1)))))))
                                 (shell-var-value-set! existing tbl)
                                 (shell-var-array?-set!
                                   existing
                                   (not assoc?))
                                 (shell-var-assoc?-set! existing assoc?)
                                 (shell-var-readonly?-set! existing #t))))
                           (begin
                             (env-readonly! env name value)
                             (when (or array? assoc?)
                               (let ([var (env-get-raw-var env name)])
                                 (when var
                                   (when array?
                                     (shell-var-array?-set! var #t))
                                   (when assoc?
                                     (shell-var-assoc?-set! var #t)
                                     (shell-var-array?-set! var #f)))))))))
                   (let ([existing (hash-get
                                     (shell-environment-vars env)
                                     arg)])
                     (if existing
                         (begin
                           (shell-var-readonly?-set! existing #t)
                           (when (and array?
                                      (not (shell-var-array? existing))
                                      (not (shell-var-assoc? existing)))
                             (shell-var-array?-set! existing #t)
                             (when (eq? (shell-var-value existing)
                                        \x2B;unset-sentinel+)
                               (shell-var-value-set!
                                 existing
                                 (make-hash-table))))
                           (when (and assoc?
                                      (not (shell-var-assoc? existing)))
                             (shell-var-assoc?-set! existing #t)
                             (shell-var-array?-set! existing #f)
                             (when (eq? (shell-var-value existing)
                                        \x2B;unset-sentinel+)
                               (shell-var-value-set!
                                 existing
                                 (make-hash-table)))))
                         (hash-put!
                           (shell-environment-vars env)
                           arg
                           (make-shell-var
                             (if (or array? assoc?)
                                 (make-hash-table)
                                 \x2B;unset-sentinel+)
                             #f #t #t #f #f #f #f array? assoc?)))))))
           names)
         0])))
  (defbuiltin
    "exit"
    (if (and (pair? args) (pair? (cdr args)))
        (begin
          (fprintf
            (current-error-port)
            "gsh: exit: too many arguments~n")
          (if (*in-subshell*)
              (raise (make-subshell-exit-exception 2))
              (exit 2)))
        (let ([code (if (pair? args)
                        (let ([n (string->number (car args))])
                          (cond
                            [(not n)
                             (fprintf
                               (current-error-port)
                               "gsh: exit: ~a: numeric argument required~n"
                               (car args))
                             2]
                            [(or (> n 2147483647) (< n -2147483648))
                             (fprintf
                               (current-error-port)
                               "gsh: exit: ~a: expected a small integer~n"
                               (car args))
                             1]
                            [else (bitwise-and n 255)]))
                        (shell-environment-last-status env))])
          (if (*in-subshell*)
              (raise (make-subshell-exit-exception code))
              (begin
                (let ([exit-trap (trap-get "EXIT")])
                  (when (and exit-trap (string? exit-trap))
                    (trap-set! "EXIT" 'default)
                    (let ([exec-fn (*execute-input*)])
                      (when exec-fn (exec-fn exit-trap env)))))
                (history-save!)
                (exit code))))))
  (defbuiltin
    "return"
    (let ([code (if (pair? args)
                    (let ([n (string->number (car args))])
                      (cond
                        [(string=? (car args) "") 0]
                        [(not n)
                         (fprintf
                           (current-error-port)
                           "gsh: return: ~a: numeric argument required~n"
                           (car args))
                         2]
                        [(or (> n 2147483647) (< n -2147483648))
                         (fprintf
                           (current-error-port)
                           "gsh: return: ~a: expected a small integer~n"
                           (car args))
                         1]
                        [else (bitwise-and n 255)]))
                    (shell-environment-last-status env))])
      (shell-return! code)))
  (defbuiltin
    "break"
    (cond
      [(> (length args) 1)
       (fprintf
         (current-error-port)
         "gsh: break: too many arguments~n")
       (flush-output-port (current-error-port))
       (if (*in-subshell*)
           (raise (make-subshell-exit-exception 1))
           (exit 1))]
      [(pair? args)
       (let ([n (string->number (car args))])
         (cond
           [(not n)
            (fprintf
              (current-error-port)
              "gsh: break: ~a: numeric argument required~n"
              (car args))
            (env-set-last-status! env 1)
            (shell-break! 1)]
           [(<= n 0)
            (fprintf
              (current-error-port)
              "gsh: break: ~a: loop count out of range~n"
              (car args))
            (env-set-last-status! env 1)
            (shell-break! 1)]
           [else (shell-break! n)]))]
      [else (shell-break! 1)]))
  (defbuiltin
    "continue"
    (cond
      [(> (length args) 1)
       (fprintf
         (current-error-port)
         "gsh: continue: too many arguments~n")
       (flush-output-port (current-error-port))
       (if (*in-subshell*)
           (raise (make-subshell-exit-exception 1))
           (exit 1))]
      [(pair? args)
       (let ([n (string->number (car args))])
         (cond
           [(not n)
            (fprintf
              (current-error-port)
              "gsh: continue: ~a: numeric argument required~n"
              (car args))
            (env-set-last-status! env 1)
            (shell-continue! 1)]
           [(<= n 0)
            (fprintf
              (current-error-port)
              "gsh: continue: ~a: loop count out of range~n"
              (car args))
            (env-set-last-status! env 1)
            (shell-continue! 1)]
           [else (shell-continue! n)]))]
      [else (shell-continue! 1)]))
  (defbuiltin
    "set"
    (if (null? args)
        (begin
          (for-each
            (lambda (pair)
              (begin
                (display
                  (format
                    "~a=~a"
                    (car pair)
                    (shell-quote-value (cdr pair))))
                (newline)))
            (env-all-variables env))
          0)
        (let loop ([args args])
          (cond
            [(null? args) 0]
            [(string=? (car args) "--")
             (env-set-positional! env (cdr args))
             0]
            [(string=? (car args) "-")
             (env-option-set! env "xtrace" #f)
             (env-option-set! env "verbose" #f)
             (when (pair? (cdr args))
               (env-set-positional! env (cdr args)))
             0]
            [(or (string=? (car args) "-o") (string=? (car args) "+o"))
             (let ([enable? (char=? (string-ref (car args) 0) #\-)])
               (if (and (pair? (cdr args))
                        (not (let ([pfx "-"] [str (cadr args)])
                               (let ([plen (string-length pfx)])
                                 (and (<= plen (string-length str))
                                      (string=?
                                        pfx
                                        (substring str 0 plen))))))
                        (not (let ([pfx "+"] [str (cadr args)])
                               (let ([plen (string-length pfx)])
                                 (and (<= plen (string-length str))
                                      (string=?
                                        pfx
                                        (substring str 0 plen)))))))
                   (begin
                     (env-option-set! env (cadr args) enable?)
                     (loop (cddr args)))
                   (begin
                     (for-each
                       (lambda (opt)
                         (let ([name (car opt)] [on? (cdr opt)])
                           (if enable?
                               (begin
                                 (display
                                   (format
                                     "~a\t~a"
                                     name
                                     (if on? "on" "off")))
                                 (newline))
                               (begin
                                 (display
                                   (format
                                     "set ~ao ~a"
                                     (if on? "-" "+")
                                     name))
                                 (newline)))))
                       (env-all-options env))
                     0)))]
            [(string=? (car args) "+") (loop (cdr args))]
            [(and (> (string-length (car args)) 1)
                  (char=? (string-ref (car args) 0) #\-))
             (apply-set-options! env (car args) #t)
             (loop (cdr args))]
            [(and (> (string-length (car args)) 1)
                  (char=? (string-ref (car args) 0) #\+))
             (apply-set-options! env (car args) #f)
             (loop (cdr args))]
            [else (env-set-positional! env args) 0]))))
  (defbuiltin
    "shift"
    (if (pair? args)
        (let ([n (string->number (car args))])
          (cond
            [(not n)
             (fprintf
               (current-error-port)
               "gsh: shift: ~a: numeric argument required~n"
               (car args))
             2]
            [(< n 0)
             (fprintf
               (current-error-port)
               "gsh: shift: ~a: shift count out of range~n"
               (car args))
             1]
            [else
             (let ([pos (env-positional-list env)])
               (if (> n (length pos))
                   1
                   (begin
                     (env-set-positional! env (list-tail pos n))
                     0)))]))
        (let ([pos (env-positional-list env)])
          (if (> 1 (length pos))
              1
              (begin (env-set-positional! env (list-tail pos 1)) 0)))))
  (defbuiltin
    "eval"
    (let ([args (if (and (pair? args)
                         (string=? (car args) "--"))
                    (cdr args)
                    args)])
      (cond
        [(null? args) 0]
        [(and (> (string-length (car args)) 1)
              (char=? (string-ref (car args) 0) #\-)
              (not (string=? (car args) "--")))
         (fprintf
           (current-error-port)
           "gsh: eval: ~a: invalid option~n"
           (car args))
         2]
        [else
         (let ([input (string-join-sp args)]
               [exec-fn (*execute-input*)])
           (if exec-fn
               (exec-fn input env)
               (begin
                 (fprintf
                   (current-error-port)
                   "gsh: eval: executor not initialized~n")
                 1)))])))
  (defbuiltin
    "test"
    (parameterize ([*test-var-fn*
                    (lambda (name) (env-get env name))]
                   [*test-option-fn*
                    (lambda (name) (env-option? env name))])
      (test-eval args)))
  (defbuiltin
    "["
    (if (and (pair? args) (string=? (last-elem* args) "]"))
        (let ([inner (butlast args)])
          (parameterize ([*test-var-fn*
                          (lambda (name) (env-get env name))]
                         [*test-option-fn*
                          (lambda (name) (env-option? env name))])
            (guard (__exn [#t ((lambda (e) 2) __exn)])
              (test-eval inner))))
        (begin
          (fprintf (current-error-port) "[: missing `]'~n")
          2)))
  (defbuiltin
    "type"
    (let parse-flags ([args args] [flags (list)])
      (if (and (pair? args)
               (> (string-length (car args)) 0)
               (char=? (string-ref (car args) 0) #\-))
          (parse-flags (cdr args) (cons (car args) flags))
          (let loop ([args args] [status 0])
            (if (null? args)
                status
                (let ([name (car args)])
                  (cond
                    [(shell-keyword? name)
                     (begin
                       (display (format "~a is a shell keyword" name))
                       (newline))
                     (loop (cdr args) status)]
                    [(alias-get env name) =>
                     (lambda (expansion)
                       (begin
                         (display
                           (format "~a is aliased to `~a'" name expansion))
                         (newline))
                       (loop (cdr args) status))]
                    [(function-lookup env name)
                     (begin
                       (display (format "~a is a function" name))
                       (newline))
                     (loop (cdr args) status)]
                    [(special-builtin? name)
                     (begin
                       (display
                         (format "~a is a special shell builtin" name))
                       (newline))
                     (loop (cdr args) status)]
                    [(builtin? name)
                     (begin
                       (display (format "~a is a shell builtin" name))
                       (newline))
                     (loop (cdr args) status)]
                    [(which name) =>
                     (lambda (path)
                       (begin
                         (display (format "~a is ~a" name path))
                         (newline))
                       (loop (cdr args) status))]
                    [else
                     (fprintf (current-error-port) "~a: not found~n" name)
                     (loop (cdr args) 1)])))))))
  (defbuiltin
    "command"
    (cond
      [(null? args) 0]
      [(string=? (car args) "-v")
       (if (pair? (cdr args))
           (let ([name (cadr args)])
             (cond
               [(builtin-lookup name) (begin (display name) (newline)) 0]
               [(function-lookup env name)
                (begin (display name) (newline))
                0]
               [(alias-get env name) =>
                (lambda (v)
                  (begin
                    (display (format "alias ~a='~a'" name v))
                    (newline))
                  0)]
               [(which name) =>
                (lambda (p) (begin (display p) (newline)) 0)]
               [else
                (fprintf
                  (current-error-port)
                  "gsh: command: ~a: not found~n"
                  name)
                1]))
           1)]
      [(string=? (car args) "-V")
       (if (pair? (cdr args))
           (let ([name (cadr args)])
             (cond
               [(builtin-lookup name)
                (begin
                  (display (format "~a is a shell builtin" name))
                  (newline))
                0]
               [(function-lookup env name)
                (begin
                  (display (format "~a is a function" name))
                  (newline))
                0]
               [(which name) =>
                (lambda (p)
                  (begin (display (format "~a is ~a" name p)) (newline))
                  0)]
               [else
                (fprintf
                  (current-error-port)
                  "gsh: command: ~a: not found~n"
                  name)
                1]))
           1)]
      [(string=? (car args) "-p")
       (if (pair? (cdr args))
           ((builtin-lookup "command") (cdr args) env)
           0)]
      [else
       (let* ([cmd-name (car args)])
         (let* ([cmd-args (cdr args)])
           (let* ([handler (builtin-lookup cmd-name)])
             (if handler
                 (handler cmd-args env)
                 (let ([exec-ext (*execute-external-fn*)])
                   (if exec-ext
                       (exec-ext cmd-name cmd-args env)
                       (begin
                         (fprintf
                           (current-error-port)
                           "gsh: ~a: command not found~n"
                           cmd-name)
                         127)))))))]))
  (defbuiltin
    "builtin"
    (if (null? args)
        0
        (let ([handler (builtin-lookup (car args))])
          (if handler
              (handler (cdr args) env)
              (begin
                (fprintf
                  (current-error-port)
                  "gsh: builtin: ~a: not a shell builtin~n"
                  (car args))
                1)))))
  (defbuiltin
    "alias"
    (let ([real-args (if (and (pair? args)
                              (string=? (car args) "--"))
                         (cdr args)
                         args)])
      (if (null? real-args)
          (begin
            (for-each
              (lambda (pair)
                (begin
                  (display (format "alias ~a='~a'" (car pair) (cdr pair)))
                  (newline)))
              (sort
                (alias-list env)
                (lambda (a b) (string<? (car a) (car b)))))
            0)
          (let ([status 0])
            (for-each
              (lambda (arg)
                (let ([eq-pos (string-find-char* arg #\=)])
                  (if eq-pos
                      (alias-set!
                        env
                        (substring arg 0 eq-pos)
                        (substring arg (+ eq-pos 1) (string-length arg)))
                      (let ([val (alias-get env arg)])
                        (if val
                            (begin
                              (display (format "alias ~a='~a'" arg val))
                              (newline))
                            (begin
                              (fprintf
                                (current-error-port)
                                "alias: ~a: not found~n"
                                arg)
                              (set! status 1)))))))
              real-args)
            status))))
  (defbuiltin
    "unalias"
    (cond
      [(null? args)
       (fprintf
         (current-error-port)
         "unalias: usage: unalias [-a] name [name ...]~n")
       2]
      [(and (pair? args) (string=? (car args) "-a"))
       (alias-clear! env)
       0]
      [else
       (let ([real-args (if (and (pair? args)
                                 (string=? (car args) "--"))
                            (cdr args)
                            args)]
             [status 0])
         (for-each
           (lambda (name)
             (if (alias-get env name)
                 (alias-unset! env name)
                 (begin
                   (fprintf
                     (current-error-port)
                     "unalias: ~a: not found~n"
                     name)
                   (set! status 1))))
           real-args)
         status)]))
  (defbuiltin
    "read"
    (let loop ([args args]
               [raw? #f]
               [silent? #f]
               [prompt ""]
               [nchars #f]
               [nchars-raw? #f]
               [delim #f]
               [timeout #f]
               [fd #f]
               [array-name #f]
               [vars (list)])
      (cond
        [(null? args)
         (when (and timeout (< timeout 0))
           (fprintf
             (current-error-port)
             "read: ~a: invalid timeout specification~n"
             timeout)
           (set! timeout #f))
         (when (and (> (string-length prompt) 0)
                    (= (ffi-isatty (or fd 0)) 1))
           (display prompt (current-error-port))
           (flush-output-port (current-error-port)))
         (let* ([in-port (if fd
                             (open-input-file
                               (string-append
                                 "/dev/fd/"
                                 (number->string fd)))
                             (current-input-port))])
           (let* ([pipe-fd (and (not fd) (*pipeline-stdin-fd*))])
             (let* ([tty? (and silent?
                               (not fd)
                               (guard (__exn [#t ((lambda (e) #f) __exn)])
                                 (tty-mode-set! in-port #t #f #f #f 0)
                                 #t))])
               (dynamic-wind
                 (lambda () (%%void))
                 (lambda ()
                   (if (and timeout (= timeout 0))
                       (begin
                         (input-port-timeout-set! in-port 0)
                         (if (guard (__exn [#t ((lambda (e) #f) __exn)])
                               (peek-char in-port)
                               #t)
                             0
                             1))
                       (begin
                         (when timeout
                           (input-port-timeout-set! in-port timeout))
                         (let* ([got-eof? #f])
                           (let* ([line (cond
                                          [(and nchars nchars-raw?)
                                           (let ([buf (open-output-string)])
                                             (let rloop ([count 0])
                                               (if (>= count nchars)
                                                   (get-output-string buf)
                                                   (let ([ch (port-or-fd-read-char
                                                               in-port
                                                               pipe-fd)])
                                                     (if (eof-object? ch)
                                                         (begin
                                                           (set! got-eof?
                                                             #t)
                                                           (let ([s (get-output-string
                                                                      buf)])
                                                             (if (string=?
                                                                   s
                                                                   "")
                                                                 ch
                                                                 s)))
                                                         (begin
                                                           (display ch buf)
                                                           (rloop
                                                             (+ count
                                                                1))))))))]
                                          [nchars
                                           (let ([delim-ch (if delim
                                                               (if (string=?
                                                                     delim
                                                                     "")
                                                                   (integer->char
                                                                     0)
                                                                   (string-ref
                                                                     delim
                                                                     0))
                                                               #\newline)]
                                                 [buf (open-output-string)])
                                             (if raw?
                                                 (let rloop ([count 0])
                                                   (if (>= count nchars)
                                                       (get-output-string
                                                         buf)
                                                       (let ([ch (port-or-fd-read-char
                                                                   in-port
                                                                   pipe-fd)])
                                                         (cond
                                                           [(eof-object?
                                                              ch)
                                                            (set! got-eof?
                                                              #t)
                                                            (let ([s (get-output-string
                                                                       buf)])
                                                              (if (string=?
                                                                    s
                                                                    "")
                                                                  ch
                                                                  s))]
                                                           [(char=?
                                                              ch
                                                              delim-ch)
                                                            (get-output-string
                                                              buf)]
                                                           [else
                                                            (display
                                                              ch
                                                              buf)
                                                            (rloop
                                                              (+ count
                                                                 1))]))))
                                                 (let rloop ([count 0])
                                                   (if (>= count nchars)
                                                       (get-output-string
                                                         buf)
                                                       (let ([ch (port-or-fd-read-char
                                                                   in-port
                                                                   pipe-fd)])
                                                         (cond
                                                           [(eof-object?
                                                              ch)
                                                            (set! got-eof?
                                                              #t)
                                                            (let ([s (get-output-string
                                                                       buf)])
                                                              (if (string=?
                                                                    s
                                                                    "")
                                                                  ch
                                                                  s))]
                                                           [(char=?
                                                              ch
                                                              delim-ch)
                                                            (get-output-string
                                                              buf)]
                                                           [(char=? ch #\\)
                                                            (let ([next (port-or-fd-read-char
                                                                          in-port
                                                                          pipe-fd)])
                                                              (cond
                                                                [(eof-object?
                                                                   next)
                                                                 (set! got-eof?
                                                                   #t)
                                                                 (get-output-string
                                                                   buf)]
                                                                [(char=?
                                                                   next
                                                                   #\newline)
                                                                 (rloop
                                                                   count)]
                                                                [else
                                                                 (display
                                                                   next
                                                                   buf)
                                                                 (rloop
                                                                   (+ count
                                                                      1))]))]
                                                           [else
                                                            (display
                                                              ch
                                                              buf)
                                                            (rloop
                                                              (+ count
                                                                 1))]))))))]
                                          [delim
                                           (let ([delim-ch (if (string=?
                                                                 delim
                                                                 "")
                                                               (integer->char
                                                                 0)
                                                               (string-ref
                                                                 delim
                                                                 0))]
                                                 [buf (open-output-string)])
                                             (if raw?
                                                 (let rloop ()
                                                   (let ([ch (port-or-fd-read-char
                                                               in-port
                                                               pipe-fd)])
                                                     (cond
                                                       [(eof-object? ch)
                                                        (set! got-eof? #t)
                                                        (let ([s (get-output-string
                                                                   buf)])
                                                          (if (string=?
                                                                s
                                                                "")
                                                              ch
                                                              s))]
                                                       [(char=?
                                                          ch
                                                          delim-ch)
                                                        (get-output-string
                                                          buf)]
                                                       [else
                                                        (display ch buf)
                                                        (rloop)])))
                                                 (let rloop ()
                                                   (let ([ch (port-or-fd-read-char
                                                               in-port
                                                               pipe-fd)])
                                                     (cond
                                                       [(eof-object? ch)
                                                        (set! got-eof? #t)
                                                        (let ([s (get-output-string
                                                                   buf)])
                                                          (if (string=?
                                                                s
                                                                "")
                                                              ch
                                                              s))]
                                                       [(char=?
                                                          ch
                                                          delim-ch)
                                                        (get-output-string
                                                          buf)]
                                                       [(char=? ch #\\)
                                                        (let ([next (port-or-fd-read-char
                                                                      in-port
                                                                      pipe-fd)])
                                                          (cond
                                                            [(eof-object?
                                                               next)
                                                             (set! got-eof?
                                                               #t)
                                                             (get-output-string
                                                               buf)]
                                                            [(char=?
                                                               next
                                                               #\newline)
                                                             (rloop)]
                                                            [else
                                                             (display
                                                               next
                                                               buf)
                                                             (rloop)]))]
                                                       [else
                                                        (display ch buf)
                                                        (rloop)])))))]
                                          [else
                                           (if raw?
                                               (let ([buf (open-output-string)])
                                                 (let rloop ()
                                                   (let ([ch (port-or-fd-read-char
                                                               in-port
                                                               pipe-fd)])
                                                     (cond
                                                       [(eof-object? ch)
                                                        (set! got-eof? #t)
                                                        (let ([s (get-output-string
                                                                   buf)])
                                                          (if (string=?
                                                                s
                                                                "")
                                                              ch
                                                              s))]
                                                       [(char=?
                                                          ch
                                                          #\newline)
                                                        (get-output-string
                                                          buf)]
                                                       [else
                                                        (display ch buf)
                                                        (rloop)]))))
                                               (let ([buf (open-output-string)])
                                                 (let rloop ()
                                                   (let ([ch (port-or-fd-read-char
                                                               in-port
                                                               pipe-fd)])
                                                     (cond
                                                       [(eof-object? ch)
                                                        (set! got-eof? #t)
                                                        (let ([s (get-output-string
                                                                   buf)])
                                                          (if (string=?
                                                                s
                                                                "")
                                                              ch
                                                              s))]
                                                       [(char=?
                                                          ch
                                                          #\newline)
                                                        (get-output-string
                                                          buf)]
                                                       [(char=? ch #\\)
                                                        (let ([next (port-or-fd-read-char
                                                                      in-port
                                                                      pipe-fd)])
                                                          (cond
                                                            [(eof-object?
                                                               next)
                                                             (set! got-eof?
                                                               #t)
                                                             (get-output-string
                                                               buf)]
                                                            [(char=?
                                                               next
                                                               #\newline)
                                                             (rloop)]
                                                            [else
                                                             (display
                                                               #\\
                                                               buf)
                                                             (display
                                                               next
                                                               buf)
                                                             (rloop)]))]
                                                       [else
                                                        (display ch buf)
                                                        (rloop)])))))])])
                             (when timeout
                               (input-port-timeout-set! in-port +inf.0))
                             (if (eof-object? line)
                                 1
                                 (let* ([var-names (cond
                                                     [array-name
                                                      (list array-name)]
                                                     [(null? vars)
                                                      (list "REPLY")]
                                                     [else
                                                      (reverse vars)])])
                                   (let* ([use-reply? (and (null? vars)
                                                           (not array-name))])
                                     (let* ([ifs (or (env-get env "IFS")
                                                     " \t\n")])
                                       (cond
                                         [nchars-raw?
                                          (env-set!
                                            env
                                            (car var-names)
                                            line)
                                          (for-each
                                            (lambda (v)
                                              (env-set! env v ""))
                                            (cdr var-names))
                                          (if got-eof? 1 0)]
                                         [use-reply?
                                          (let ([val (if raw?
                                                         line
                                                         (read-strip-backslashes
                                                           line))])
                                            (env-set! env "REPLY" val)
                                            (if got-eof? 1 0))]
                                         [array-name
                                          (let ([fields (if raw?
                                                            (read-ifs-split-raw
                                                              line
                                                              ifs
                                                              0)
                                                            (read-ifs-split
                                                              line
                                                              ifs
                                                              0))])
                                            (env-array-set-compound!
                                              env
                                              array-name
                                              fields
                                              #f)
                                            (if got-eof? 1 0))]
                                         [else
                                          (let ([fields (if raw?
                                                            (read-ifs-split-raw
                                                              line
                                                              ifs
                                                              (length
                                                                var-names))
                                                            (read-ifs-split
                                                              line
                                                              ifs
                                                              (length
                                                                var-names)))])
                                            (let field-loop ([names var-names]
                                                             [fields fields])
                                              (cond
                                                [(null? names)
                                                 (if got-eof? 1 0)]
                                                [(null? fields)
                                                 (env-set!
                                                   env
                                                   (car names)
                                                   "")
                                                 (field-loop
                                                   (cdr names)
                                                   (list))]
                                                [(null? (cdr names))
                                                 (env-set!
                                                   env
                                                   (car names)
                                                   (string-join-sp fields))
                                                 (if got-eof? 1 0)]
                                                [else
                                                 (env-set!
                                                   env
                                                   (car names)
                                                   (car fields))
                                                 (field-loop
                                                   (cdr names)
                                                   (cdr fields))])))]))))))))))
                 (lambda ()
                   (when tty?
                     (guard (__exn [#t (void __exn)])
                       (tty-mode-set! in-port #t #t #f #f 0)))
                   (when fd (close-input-port in-port)))))))]
        [(and (> (string-length (car args)) 1)
              (char=? (string-ref (car args) 0) #\-))
         (let* ([arg (car args)])
           (let* ([rest (cdr args)])
             (let parse-flags ([i 1])
               (if (>= i (string-length arg))
                   (loop rest raw? silent? prompt nchars nchars-raw? delim
                     timeout fd array-name vars)
                   (let ([ch (string-ref arg i)])
                     (case ch
                       [(#\r) (set! raw? #t) (parse-flags (+ i 1))]
                       [(#\s) (set! silent? #t) (parse-flags (+ i 1))]
                       [(#\e) (parse-flags (+ i 1))]
                       [(#\n)
                        (let ([num-str (substring
                                         arg
                                         (+ i 1)
                                         (string-length arg))])
                          (if (> (string-length num-str) 0)
                              (let ([n (string->number num-str)])
                                (cond
                                  [(not n)
                                   (fprintf
                                     (current-error-port)
                                     "read: ~a: invalid number~n"
                                     num-str)
                                   2]
                                  [(< n 0)
                                   (fprintf
                                     (current-error-port)
                                     "read: ~a: invalid number~n"
                                     num-str)
                                   2]
                                  [else
                                   (set! nchars n)
                                   (loop rest raw? silent? prompt nchars
                                     nchars-raw? delim timeout fd
                                     array-name vars)]))
                              (if (pair? rest)
                                  (let ([n (string->number (car rest))])
                                    (cond
                                      [(not n)
                                       (fprintf
                                         (current-error-port)
                                         "read: ~a: invalid number~n"
                                         (car rest))
                                       2]
                                      [(< n 0)
                                       (fprintf
                                         (current-error-port)
                                         "read: ~a: invalid number~n"
                                         (car rest))
                                       2]
                                      [else
                                       (set! nchars n)
                                       (loop (cdr rest) raw? silent? prompt
                                         nchars nchars-raw? delim timeout
                                         fd array-name vars)]))
                                  (loop rest raw? silent? prompt nchars
                                    nchars-raw? delim timeout fd array-name
                                    vars))))]
                       [(#\N)
                        (let ([num-str (substring
                                         arg
                                         (+ i 1)
                                         (string-length arg))])
                          (set! nchars-raw? #t)
                          (if (> (string-length num-str) 0)
                              (begin
                                (set! nchars
                                  (or (string->number num-str) 1))
                                (loop rest raw? silent? prompt nchars
                                  nchars-raw? delim timeout fd array-name
                                  vars))
                              (if (pair? rest)
                                  (begin
                                    (set! nchars
                                      (or (string->number (car rest)) 1))
                                    (loop (cdr rest) raw? silent? prompt nchars
                                      nchars-raw? delim timeout fd
                                      array-name vars))
                                  (loop rest raw? silent? prompt nchars
                                    nchars-raw? delim timeout fd array-name
                                    vars))))]
                       [(#\p)
                        (let ([p-str (substring
                                       arg
                                       (+ i 1)
                                       (string-length arg))])
                          (if (> (string-length p-str) 0)
                              (begin
                                (set! prompt p-str)
                                (loop rest raw? silent? prompt nchars
                                  nchars-raw? delim timeout fd array-name
                                  vars))
                              (if (pair? rest)
                                  (begin
                                    (set! prompt (car rest))
                                    (loop (cdr rest) raw? silent? prompt nchars
                                      nchars-raw? delim timeout fd
                                      array-name vars))
                                  (loop rest raw? silent? prompt nchars
                                    nchars-raw? delim timeout fd array-name
                                    vars))))]
                       [(#\d)
                        (let ([d-str (substring
                                       arg
                                       (+ i 1)
                                       (string-length arg))])
                          (if (> (string-length d-str) 0)
                              (begin
                                (set! delim d-str)
                                (loop rest raw? silent? prompt nchars
                                  nchars-raw? delim timeout fd array-name
                                  vars))
                              (if (pair? rest)
                                  (begin
                                    (set! delim (car rest))
                                    (loop (cdr rest) raw? silent? prompt nchars
                                      nchars-raw? delim timeout fd
                                      array-name vars))
                                  (loop rest raw? silent? prompt nchars
                                    nchars-raw? delim timeout fd array-name
                                    vars))))]
                       [(#\t)
                        (let ([t-str (substring
                                       arg
                                       (+ i 1)
                                       (string-length arg))])
                          (if (> (string-length t-str) 0)
                              (let ([t (string->number t-str)])
                                (if (and t (>= t 0))
                                    (begin
                                      (set! timeout t)
                                      (loop rest raw? silent? prompt nchars
                                        nchars-raw? delim timeout fd
                                        array-name vars))
                                    (begin
                                      (fprintf
                                        (current-error-port)
                                        "read: ~a: invalid timeout specification~n"
                                        t-str)
                                      2)))
                              (if (pair? rest)
                                  (let ([t (string->number (car rest))])
                                    (if (and t (>= t 0))
                                        (begin
                                          (set! timeout t)
                                          (loop (cdr rest) raw? silent? prompt
                                            nchars nchars-raw? delim
                                            timeout fd array-name vars))
                                        (begin
                                          (fprintf
                                            (current-error-port)
                                            "read: ~a: invalid timeout specification~n"
                                            (car rest))
                                          2)))
                                  (loop rest raw? silent? prompt nchars
                                    nchars-raw? delim timeout fd array-name
                                    vars))))]
                       [(#\u)
                        (let ([u-str (substring
                                       arg
                                       (+ i 1)
                                       (string-length arg))])
                          (if (> (string-length u-str) 0)
                              (let ([n (string->number u-str)])
                                (if (and n (>= n 0))
                                    (begin
                                      (set! fd n)
                                      (loop rest raw? silent? prompt nchars
                                        nchars-raw? delim timeout fd
                                        array-name vars))
                                    (begin
                                      (fprintf
                                        (current-error-port)
                                        "read: ~a: invalid file descriptor specification~n"
                                        u-str)
                                      2)))
                              (if (pair? rest)
                                  (let ([n (string->number (car rest))])
                                    (if (and n (>= n 0))
                                        (begin
                                          (set! fd n)
                                          (loop (cdr rest) raw? silent? prompt
                                            nchars nchars-raw? delim
                                            timeout fd array-name vars))
                                        (begin
                                          (fprintf
                                            (current-error-port)
                                            "read: ~a: invalid file descriptor specification~n"
                                            (car rest))
                                          2)))
                                  (loop rest raw? silent? prompt nchars
                                    nchars-raw? delim timeout fd array-name
                                    vars))))]
                       [(#\a)
                        (let ([a-str (substring
                                       arg
                                       (+ i 1)
                                       (string-length arg))])
                          (if (> (string-length a-str) 0)
                              (begin
                                (set! array-name a-str)
                                (loop rest raw? silent? prompt nchars
                                  nchars-raw? delim timeout fd array-name
                                  vars))
                              (if (pair? rest)
                                  (begin
                                    (set! array-name (car rest))
                                    (loop (cdr rest) raw? silent? prompt nchars
                                      nchars-raw? delim timeout fd
                                      array-name vars))
                                  (loop rest raw? silent? prompt nchars
                                    nchars-raw? delim timeout fd array-name
                                    vars))))]
                       [else
                        (fprintf
                          (current-error-port)
                          "read: -~a: invalid option~n"
                          ch)
                        2]))))))]
        [else
         (loop (cdr args) raw? silent? prompt nchars nchars-raw?
           delim timeout fd array-name (cons (car args) vars))])))
  (defbuiltin "trap"
    (define (print-trap short-name action)
      (let ([display-name (signal-display-name short-name)])
        (if (string? action)
            (begin
              (display (format "trap -- '~a' ~a" action display-name))
              (newline))
            (begin
              (display (format "trap -- '' ~a" display-name))
              (newline)))))
    (define (normalize-or-error sig)
      (let ([n (normalize-signal-arg sig)])
        (unless n
          (fprintf
            (current-error-port)
            "trap: ~a: invalid signal specification~n"
            sig))
        n))
    (define (unsigned-integer? s)
      (and (> (string-length s) 0)
           (let loop ([i 0])
             (if (>= i (string-length s))
                 #t
                 (and (char-numeric? (string-ref s i)) (loop (+ i 1)))))))
    (define (trap-set-action action signals)
      (let ([status 0])
        (for-each
          (lambda (sig)
            (let ([norm (normalize-or-error sig)])
              (if norm
                  (cond
                    [(string=? action "-") (trap-set! norm 'default)]
                    [(string=? action "") (trap-set! norm 'ignore)]
                    [else (trap-set! norm action)])
                  (set! status 1))))
          signals)
        status))
    (cond
      [(null? args)
       (for-each
         (lambda (pair) (print-trap (car pair) (cdr pair)))
         (trap-list))
       0]
      [(string=? (car args) "-l")
       (for-each displayln (signal-name-list))
       0]
      [(string=? (car args) "-p")
       (let ([sigs (cdr args)])
         (for-each
           (lambda (sig)
             (let ([norm (normalize-or-error sig)])
               (when norm
                 (let ([action (trap-get norm)])
                   (when action (print-trap norm action))))))
           (if (pair? sigs) sigs (map car (trap-list)))))
       0]
      [(and (> (string-length (car args)) 1)
            (char=? (string-ref (car args) 0) #\-)
            (not (string=? (car args) "-"))
            (not (string=? (car args) "--"))
            (not (string=? (car args) "-l"))
            (not (string=? (car args) "-p")))
       (fprintf
         (current-error-port)
         "trap: ~a: invalid signal specification~n"
         (car args))
       1]
      [(string=? (car args) "--")
       (let ([rest (cdr args)])
         (if (null? rest)
             (begin
               (for-each
                 (lambda (pair) (print-trap (car pair) (cdr pair)))
                 (trap-list))
               0)
             (if (= (length rest) 1)
                 (let ([norm (normalize-or-error (car rest))])
                   (if norm (begin (trap-set! norm 'default) 0) 1))
                 (trap-set-action (car rest) (cdr rest)))))]
      [(= (length args) 1)
       (let ([norm (normalize-or-error (car args))])
         (if norm (begin (trap-set! norm 'default) 0) 1))]
      [(>= (length args) 2)
       (if (unsigned-integer? (car args))
           (let ([status 0])
             (for-each
               (lambda (sig)
                 (let ([norm (normalize-or-error sig)])
                   (if norm (trap-set! norm 'default) (set! status 1))))
               args)
             status)
           (trap-set-action (car args) (cdr args)))]
      [else 0]))
  (defbuiltin
    "jobs"
    (job-update-status!)
    (let ([print-pids? (and (pair? args)
                            (string=? (car args) "-p"))])
      (for-each
        (lambda (job)
          (if print-pids?
              (begin (display (job-pgid job)) (newline))
              (begin
                (display
                  (format
                    "[~a] ~a ~a"
                    (job-id job)
                    (case (job-status job)
                      [(running) "Running"]
                      [(stopped) "Stopped"]
                      [(done) "Done"]
                      [(killed) "Killed"]
                      [else "???"])
                    (job-command-text job)))
                (newline))))
        (job-table-list))
      (job-table-cleanup!)
      0))
  (defbuiltin
    "fg"
    (let ([spec (if (pair? args) (car args) "%%")])
      (let ([job (job-table-get spec)])
        (if job
            (begin
              (begin (display (job-command-text job)) (newline))
              (job-foreground! job))
            (begin
              (fprintf (current-error-port) "fg: ~a: no such job~n" spec)
              1)))))
  (defbuiltin
    "bg"
    (let ([specs (if (null? args) (list "%%") args)])
      (for-each
        (lambda (spec)
          (let ([job (job-table-get spec)])
            (if job
                (job-background! job)
                (fprintf
                  (current-error-port)
                  "bg: ~a: no such job~n"
                  spec))))
        specs)
      0))
  (defbuiltin
    "wait"
    (let ([result (cond
                    [(null? args)
                     (for-each
                       (lambda (job) (job-wait job))
                       (job-table-list))
                     0]
                    [(string=? (car args) "-n")
                     (let* ([rest-args (cdr args)])
                       (let* ([valid? (let vloop ([a rest-args])
                                        (or (null? a)
                                            (let ([arg (car a)])
                                              (if (or (string->number arg)
                                                      (and (> (string-length
                                                                arg)
                                                              0)
                                                           (char=?
                                                             (string-ref
                                                               arg
                                                               0)
                                                             #\%)))
                                                  (vloop (cdr a))
                                                  (begin
                                                    (fprintf
                                                      (current-error-port)
                                                      "wait: `~a': not a pid or valid job spec~n"
                                                      arg)
                                                    #f)))))])
                         (if (not valid?)
                             127
                             (let ([jobs (if (null? rest-args)
                                             (job-table-list)
                                             (filter
                                               identity
                                               (map (lambda (a)
                                                      (job-table-get a))
                                                    rest-args)))])
                               (if (null? jobs)
                                   127
                                   (job-wait-any jobs))))))]
                    [else
                     (let loop ([args args] [status 0])
                       (if (null? args)
                           status
                           (let* ([arg (car args)])
                             (let* ([valid-arg? (or (string->number arg)
                                                    (and (> (string-length
                                                              arg)
                                                            0)
                                                         (char=?
                                                           (string-ref
                                                             arg
                                                             0)
                                                           #\%)))])
                               (if (not valid-arg?)
                                   (begin
                                     (fprintf
                                       (current-error-port)
                                       "wait: `~a': not a pid or valid job spec~n"
                                       arg)
                                     (loop (cdr args) 2))
                                   (let ([job (job-table-get arg)])
                                     (if job
                                         (loop (cdr args) (job-wait job))
                                         (loop (cdr args) 127))))))))])])
      (job-table-cleanup!)
      result))
  (defbuiltin
    "kill"
    (if (null? args)
        (begin
          (fprintf
            (current-error-port)
            "kill: usage: kill [-signal] pid|jobspec~n")
          1)
        (let loop ([args args] [sig SIGTERM])
          (cond
            [(null? args) 0]
            [(and (> (string-length (car args)) 1)
                  (char=? (string-ref (car args) 0) #\-))
             (let* ([sig-name (substring
                                (car args)
                                1
                                (string-length (car args)))])
               (let* ([sig-num (or (signal-name->number sig-name)
                                   (string->number sig-name)
                                   SIGTERM)])
                 (loop (cdr args) sig-num)))]
            [else
             (let ([self-pid (ffi-getpid)] [sent-to-self? #f])
               (for-each
                 (lambda (target)
                   (cond
                     [(char=? (string-ref target 0) #\%)
                      (let ([job (job-table-get target)])
                        (when job
                          (for-each
                            (lambda (proc)
                              (kill (job-process-pid proc) sig))
                            (job-processes job))))]
                     [else
                      (let ([pid (string->number target)])
                        (when pid
                          (when (= pid self-pid) (set! sent-to-self? #t))
                          (kill pid sig)))]))
                 args)
               (when sent-to-self? (thread-sleep! 0.001))
               0)]))))
  (defbuiltin
    "history"
    (cond
      [(and (pair? args) (pair? (cdr args)))
       (fprintf
         (current-error-port)
         "gsh: history: too many arguments~n")
       2]
      [(and (pair? args)
            (> (string-length (car args)) 0)
            (char=? (string-ref (car args) 0) #\-))
       (fprintf
         (current-error-port)
         "gsh: history: ~a: invalid option~n"
         (car args))
       2]
      [(and (pair? args) (not (string->number (car args))))
       (fprintf
         (current-error-port)
         "gsh: history: ~a: numeric argument required~n"
         (car args))
       2]
      [else
       (let* ([entries (history-list)])
         (let* ([n (if (pair? args)
                       (or (string->number (car args)) (length entries))
                       (length entries))])
           (let* ([to-show (if (< n (length entries))
                               (list-tail entries (- (length entries) n))
                               entries)])
             (let loop ([entries to-show]
                        [num (- (length entries) (length to-show) -1)])
               (when (pair? entries)
                 (begin
                   (display (format "  ~a  ~a" num (car entries)))
                   (newline))
                 (loop (cdr entries) (+ num 1))))
             0)))]))
  (defbuiltin
    "local"
    (if (and (pair? args) (string=? (car args) "-p"))
        (let ([specific-names (cdr args)])
          (if (pair? specific-names)
              (begin
                (for-each
                  (lambda (name)
                    (let ([var (hash-get
                                 (shell-environment-vars env)
                                 name)])
                      (when (and var
                                 (shell-var-local? var)
                                 (not (eq? (shell-var-value var)
                                           \x2B;unset-sentinel+)))
                        (display-declare-var name var))))
                  specific-names)
                0)
              (begin
                (let ([names (sort!
                               (hash-keys (shell-environment-vars env))
                               string<?)])
                  (for-each
                    (lambda (name)
                      (let ([var (hash-get
                                   (shell-environment-vars env)
                                   name)])
                        (when (and var
                                   (shell-var-local? var)
                                   (not (eq? (shell-var-value var)
                                             \x2B;unset-sentinel+)))
                          (display-declare-var name var))))
                    names))
                0)))
        (if (null? args)
            (begin
              (let ([names (sort!
                             (hash-keys (shell-environment-vars env))
                             string<?)])
                (for-each
                  (lambda (name)
                    (let ([var (hash-get
                                 (shell-environment-vars env)
                                 name)])
                      (when (and var
                                 (shell-var-local? var)
                                 (not (eq? (shell-var-value var)
                                           \x2B;unset-sentinel+)))
                        (begin
                          (display
                            (format
                              "~a=~a"
                              name
                              (shell-var-scalar-value var)))
                          (newline)))))
                  names))
              0)
            (let loop ([args args]
                       [nameref? #f]
                       [integer? #f]
                       [readonly? #f]
                       [export? #f]
                       [array? #f]
                       [assoc? #f])
              (cond
                [(null? args) 0]
                [(and (> (string-length (car args)) 1)
                      (char=? (string-ref (car args) 0) #\-)
                      (char-alphabetic? (string-ref (car args) 1)))
                 (let floop ([i 1]
                             [nr? nameref?]
                             [int? integer?]
                             [ro? readonly?]
                             [ex? export?]
                             [ar? array?]
                             [as? assoc?])
                   (if (>= i (string-length (car args)))
                       (loop (cdr args) nr? int? ro? ex? ar? as?)
                       (let ([ch (string-ref (car args) i)])
                         (case ch
                           [(#\n) (floop (+ i 1) #t int? ro? ex? ar? as?)]
                           [(#\i) (floop (+ i 1) nr? #t ro? ex? ar? as?)]
                           [(#\r) (floop (+ i 1) nr? int? #t ex? ar? as?)]
                           [(#\x) (floop (+ i 1) nr? int? ro? #t ar? as?)]
                           [(#\a) (floop (+ i 1) nr? int? ro? ex? #t as?)]
                           [(#\A) (floop (+ i 1) nr? int? ro? ex? ar? #t)]
                           [else
                            (floop (+ i 1) nr? int? ro? ex? ar? as?)]))))]
                [else
                 (let ([effective-export? (or export?
                                              (env-option?
                                                env
                                                "allexport"))])
                   (for-each
                     (lambda (arg)
                       (let ([eq-pos (string-find-char* arg #\=)])
                         (if eq-pos
                             (let* ([name (substring arg 0 eq-pos)])
                               (let* ([value (substring
                                               arg
                                               (+ eq-pos 1)
                                               (string-length arg))])
                                 (if (and (> (string-length value) 0)
                                          (char=?
                                            (string-ref value 0)
                                            #\())
                                     (let* ([inner (if (and (> (string-length
                                                                 value)
                                                               1)
                                                            (char=?
                                                              (string-ref
                                                                value
                                                                (- (string-length
                                                                     value)
                                                                   1))
                                                              #\)))
                                                       (substring
                                                         value
                                                         1
                                                         (- (string-length
                                                              value)
                                                            1))
                                                       (substring
                                                         value
                                                         1
                                                         (string-length
                                                           value)))])
                                       (let* ([tbl (make-hash-table)])
                                         (if assoc?
                                             (let ([elems (parse-array-compound-elements
                                                            inner)])
                                               (for-each
                                                 (lambda (elem)
                                                   (let ([bracket-start (string-find-char*
                                                                          elem
                                                                          #\[)])
                                                     (if bracket-start
                                                         (let* ([bracket-end (string-find-char-from
                                                                               elem
                                                                               #\]
                                                                               (+ bracket-start
                                                                                  1))])
                                                           (let* ([key (substring
                                                                         elem
                                                                         (+ bracket-start
                                                                            1)
                                                                         (or bracket-end
                                                                             (string-length
                                                                               elem)))])
                                                             (let* ([eq2 (string-find-char-from
                                                                           elem
                                                                           #\=
                                                                           (or bracket-end
                                                                               0))])
                                                               (let* ([val (if eq2
                                                                               (substring
                                                                                 elem
                                                                                 (+ eq2
                                                                                    1)
                                                                                 (string-length
                                                                                   elem))
                                                                               "")])
                                                                 (hash-put!
                                                                   tbl
                                                                   key
                                                                   val)))))
                                                         (hash-put!
                                                           tbl
                                                           elem
                                                           ""))))
                                                 elems)
                                               (hash-put!
                                                 (shell-environment-vars
                                                   env)
                                                 name
                                                 (make-shell-var tbl effective-export?
                                                   readonly? #t integer? #f
                                                   #f nameref? #f #t)))
                                             (let ([elems (parse-array-compound-elements
                                                            inner)])
                                               (let eloop ([es elems]
                                                           [idx 0])
                                                 (when (pair? es)
                                                   (let ([elem (car es)])
                                                     (let ([bracket-start (string-find-char*
                                                                            elem
                                                                            #\[)])
                                                       (if bracket-start
                                                           (let* ([bracket-end (string-find-char-from
                                                                                 elem
                                                                                 #\]
                                                                                 (+ bracket-start
                                                                                    1))])
                                                             (let* ([key (or (string->number
                                                                               (substring
                                                                                 elem
                                                                                 (+ bracket-start
                                                                                    1)
                                                                                 (or bracket-end
                                                                                     (string-length
                                                                                       elem))))
                                                                             idx)])
                                                               (let* ([eq2 (string-find-char-from
                                                                             elem
                                                                             #\=
                                                                             (or bracket-end
                                                                                 0))])
                                                                 (let* ([val (if eq2
                                                                                 (substring
                                                                                   elem
                                                                                   (+ eq2
                                                                                      1)
                                                                                   (string-length
                                                                                     elem))
                                                                                 "")])
                                                                   (hash-put!
                                                                     tbl
                                                                     key
                                                                     val)
                                                                   (eloop
                                                                     (cdr es)
                                                                     (+ key
                                                                        1))))))
                                                           (begin
                                                             (hash-put!
                                                               tbl
                                                               idx
                                                               elem)
                                                             (eloop
                                                               (cdr es)
                                                               (+ idx
                                                                  1))))))))
                                               (hash-put!
                                                 (shell-environment-vars
                                                   env)
                                                 name
                                                 (make-shell-var tbl effective-export?
                                                   readonly? #t integer? #f
                                                   #f nameref? #t #f))))))
                                     (let ([existing (hash-get
                                                       (shell-environment-vars
                                                         env)
                                                       name)])
                                       (when (and existing
                                                  (shell-var-readonly?
                                                    existing))
                                         (error 'gerbil
                                           (format
                                             "~a: readonly variable"
                                             name)))
                                       (hash-put!
                                         (shell-environment-vars env)
                                         name
                                         (make-shell-var value effective-export?
                                           readonly? #t integer? #f #f
                                           nameref? array? assoc?))))))
                             (let ([existing (hash-get
                                               (shell-environment-vars env)
                                               arg)])
                               (if existing
                                   (begin
                                     (when nameref?
                                       (shell-var-nameref?-set!
                                         existing
                                         #t))
                                     (when integer?
                                       (shell-var-integer?-set!
                                         existing
                                         #t))
                                     (when readonly?
                                       (shell-var-readonly?-set!
                                         existing
                                         #t))
                                     (when effective-export?
                                       (shell-var-exported?-set!
                                         existing
                                         #t))
                                     (when array?
                                       (shell-var-array?-set! existing #t))
                                     (when assoc?
                                       (shell-var-assoc?-set! existing #t))
                                     (shell-var-local?-set! existing #t))
                                   (hash-put!
                                     (shell-environment-vars env)
                                     arg
                                     (make-shell-var
                                       (if (or array? assoc?)
                                           (make-hash-table)
                                           \x2B;unset-sentinel+)
                                       effective-export? readonly? #t
                                       integer? #f #f nameref? array?
                                       assoc?)))))))
                     args)
                   0)])))))
  (defbuiltin
    "declare"
    (call/cc
      (lambda (return)
        (parameterize ([return-from-declare return])
          (builtin-declare args env)))))
  (defbuiltin
    "typeset"
    (call/cc
      (lambda (return)
        (parameterize ([return-from-declare return])
          (builtin-declare args env)))))
  (defbuiltin
    "let"
    (let loop ([args args] [result 0])
      (if (null? args)
          (if (= result 0) 1 0)
          (let ([val (arith-eval-wrapper (car args) env)])
            (loop (cdr args) val)))))
  (defbuiltin
    "shopt"
    (cond
      [(null? args) 0]
      [(string=? (car args) "-s")
       (for-each
         (lambda (name) (env-shopt-set! env name #t))
         (cdr args))
       0]
      [(string=? (car args) "-u")
       (for-each
         (lambda (name) (env-shopt-set! env name #f))
         (cdr args))
       0]
      [else 0]))
  (defbuiltin
    "help"
    (if (null? args)
        (begin
          (begin (display "gsh - Gerbil Shell") (newline))
          (begin (display "Built-in commands:") (newline))
          (for-each
            (lambda (name)
              (display "  ")
              (begin (display name) (newline)))
            (builtin-list))
          0)
        0))
  (defbuiltin
    "umask"
    (if (null? args)
        (let ([mask (ffi-umask 0)])
          (ffi-umask mask)
          (begin (display (format "~4,'0o" mask)) (newline))
          0)
        (let ([mode (string->number (car args) 8)])
          (if mode
              (begin (ffi-umask mode) 0)
              (begin
                (fprintf
                  (current-error-port)
                  "umask: ~a: invalid octal number~n"
                  (car args))
                1)))))
  (defbuiltin
    "ulimit"
    (let ([soft? #f]
          [hard? #f]
          [show-all? #f]
          [resource-flag #f]
          [value #f]
          [explicit-sh? #f])
      (let loop ([rest args] [seen-resource #f])
        (cond
          [(null? rest)
           (let ([flag (or resource-flag #\f)])
             (if show-all?
                 (begin
                   (for-each
                     (lambda (entry)
                       (let* ([ch (car entry)])
                         (let* ([desc (cadr entry)])
                           (let* ([res-const (caddr entry)])
                             (let* ([block-size (cadddr entry)])
                               (if (not res-const)
                                   (begin
                                     (display (format "~a 8" desc))
                                     (newline))
                                   (let* ([raw (if (and hard? (not soft?))
                                                   (ffi-getrlimit-hard
                                                     res-const)
                                                   (ffi-getrlimit-soft
                                                     res-const))])
                                     (let* ([display-val (cond
                                                           [(= raw -1)
                                                            "unlimited"]
                                                           [(= raw -2)
                                                            "error"]
                                                           [else
                                                            (number->string
                                                              (quotient
                                                                raw
                                                                block-size))])])
                                       (begin
                                         (display
                                           (format
                                             "~a ~a"
                                             desc
                                             display-val))
                                         (newline))))))))))
                     *ulimit-resources*)
                   0)
                 (let* ([entry (assoc flag *ulimit-resources*)])
                   (let* ([res-const (and entry (caddr entry))])
                     (let* ([block-size (and entry (cadddr entry))])
                       (cond
                         [(not entry)
                          (fprintf
                            (current-error-port)
                            "ulimit: invalid option '~a'~n"
                            flag)
                          1]
                         [(not value)
                          (if (not res-const)
                              (begin (begin (display "8") (newline)) 0)
                              (let ([raw (if (and hard? (not soft?))
                                             (ffi-getrlimit-hard res-const)
                                             (ffi-getrlimit-soft
                                               res-const))])
                                (cond
                                  [(= raw -1)
                                   (begin (display "unlimited") (newline))
                                   0]
                                  [(= raw -2)
                                   (fprintf
                                     (current-error-port)
                                     "ulimit: error getting limit~n")
                                   1]
                                  [else
                                   (begin
                                     (display (quotient raw block-size))
                                     (newline))
                                   0])))]
                         [(not res-const)
                          (fprintf
                            (current-error-port)
                            "ulimit: -p: cannot modify limit~n")
                          1]
                         [else
                          (let* ([raw-val (cond
                                            [(string=? value "unlimited")
                                             -1]
                                            [(string=? value "hard")
                                             (ffi-getrlimit-hard
                                               res-const)]
                                            [(string=? value "soft")
                                             (ffi-getrlimit-soft
                                               res-const)]
                                            [else
                                             (let ([n (string->number
                                                        value)])
                                               (and n
                                                    (* n block-size)))])])
                            (let* ([only-soft (and explicit-sh?
                                                   soft?
                                                   (not hard?))])
                              (let* ([only-hard (and explicit-sh?
                                                     hard?
                                                     (not soft?))])
                                (if (not raw-val)
                                    (begin
                                      (fprintf
                                        (current-error-port)
                                        "ulimit: ~a: invalid number~n"
                                        value)
                                      1)
                                    (let ([rc (ffi-setrlimit res-const raw-val raw-val
                                                only-soft only-hard)])
                                      (if (= rc 0)
                                          0
                                          (begin
                                            (fprintf
                                              (current-error-port)
                                              "ulimit: error setting limit~n")
                                            1)))))))]))))))]
          [(string=? (car rest) "-S")
           (set! soft? #t)
           (set! explicit-sh? #t)
           (loop (cdr rest) seen-resource)]
          [(string=? (car rest) "-H")
           (set! hard? #t)
           (set! explicit-sh? #t)
           (loop (cdr rest) seen-resource)]
          [(or (string=? (car rest) "-a")
               (string=? (car rest) "--all"))
           (if seen-resource
               (begin
                 (fprintf
                   (current-error-port)
                   "ulimit: ~a: too many arguments~n"
                   (car rest))
                 2)
               (begin (set! show-all? #t) (loop (cdr rest) #t)))]
          [(and (> (string-length (car rest)) 1)
                (char=? (string-ref (car rest) 0) #\-))
           (if seen-resource
               (begin
                 (fprintf
                   (current-error-port)
                   "ulimit: ~a: too many arguments~n"
                   (car rest))
                 2)
               (let ([ch (string-ref (car rest) 1)])
                 (set! resource-flag ch)
                 (loop (cdr rest) #t)))]
          [else
           (if show-all?
               (begin
                 (fprintf
                   (current-error-port)
                   "ulimit: ~a: too many arguments~n"
                   (car rest))
                 2)
               (begin
                 (set! value (car rest))
                 (loop (cdr rest) seen-resource)))]))))
  (defbuiltin
    "dirs"
    (for-each
      displayln
      (cons
        (current-directory)
        (shell-environment-dir-stack env)))
    0)
  (defbuiltin
    "pushd"
    (let ([dir (if (pair? args)
                   (car args)
                   (if (pair? (shell-environment-dir-stack env))
                       (car (shell-environment-dir-stack env))
                       (begin
                         (fprintf
                           (current-error-port)
                           "pushd: no other directory~n")
                         #f)))])
      (if dir
          (let ([old (current-directory)])
            (guard (__exn
                     [#t
                      ((lambda (e)
                         (fprintf
                           (current-error-port)
                           "pushd: ~a: No such file or directory~n"
                           dir)
                         1)
                        __exn)])
              (current-directory (expand-word-nosplit dir env))
              (shell-environment-dir-stack-set!
                env
                (cons old (shell-environment-dir-stack env)))
              (let ([new-pwd (strip-trailing-slash (current-directory))])
                (env-set! env "PWD" new-pwd)
                (*internal-pwd* new-pwd))
              (display (current-directory))
              (for-each
                (lambda (d) (display " ") (display d))
                (shell-environment-dir-stack env))
              (newline)
              0))
          1)))
  (defbuiltin
    "popd"
    (let ([stack (shell-environment-dir-stack env)])
      (if (null? stack)
          (begin
            (fprintf
              (current-error-port)
              "popd: directory stack empty~n")
            1)
          (let ([dir (car stack)])
            (shell-environment-dir-stack-set! env (cdr stack))
            (current-directory dir)
            (env-set! env "OLDPWD" (env-get env "PWD"))
            (let ([new-pwd (strip-trailing-slash (current-directory))])
              (env-set! env "PWD" new-pwd)
              (*internal-pwd* new-pwd))
            (display (current-directory))
            (for-each
              (lambda (d) (display " ") (display d))
              (shell-environment-dir-stack env))
            (newline)
            0))))
  (builtin-register! "mapfile" builtin-mapfile)
  (builtin-register! "readarray" builtin-mapfile))
