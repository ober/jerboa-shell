#!chezscheme
(library (jsh lexer)
  (export lexer::t make-lexer lexer? lexer-input lexer-pos
   lexer-len lexer-tokens lexer-peeked lexer-heredocs
   lexer-want-more? lexer-line lexer-col lexer-extglob?
   lexer-input-set! lexer-pos-set! lexer-len-set!
   lexer-tokens-set! lexer-peeked-set! lexer-heredocs-set!
   lexer-want-more?-set! lexer-line-set! lexer-col-set!
   lexer-extglob?-set! &lexer-input &lexer-pos &lexer-len
   &lexer-tokens &lexer-peeked &lexer-heredocs
   &lexer-want-more? &lexer-line &lexer-col &lexer-extglob?
   &lexer-input-set! &lexer-pos-set! &lexer-len-set!
   &lexer-tokens-set! &lexer-peeked-set! &lexer-heredocs-set!
   &lexer-want-more?-set! &lexer-line-set! &lexer-col-set!
   &lexer-extglob?-set! make-shell-lexer lexer-next! lexer-peek
   lexer-needs-more? lexer-prepend-text! tokenize
   lexer-read-token! current-char char-at advance! at-end?
   skip-whitespace! skip-comment! operator-start?
   read-procsub-body! read-operator! extglob-prefix-before?
   read-extglob-body! read-word! finish-word current-lexer
   read-word-with-context! lexer-read-token-impl!
   read-single-quoted! read-single-quoted-content!
   read-double-quoted! read-double-quoted-content!
   ansi-c-emit-char! read-ansi-c-quoted-content!
   read-ansi-c-quoted! read-dollar! read-backtick!
   collect-heredocs! collect-heredoc-body!
   read-line-from-lexer! string-trim-tabs all-digits?
   assignment-word? string-find-char-in-name valid-shell-name?
   string-find-char read-hex-digits read-octal-digits
   read-octal-digits-with-init hex-val octal-char?
   *reserved-words* reserved-word?)
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
    (std sugar) (jsh ast)
    (except (jsh util) string-index string-join file-directory?
      string-join string-index string-downcase file-regular?
      string-upcase))
  (begin
    (define lexer::t
      (make-class-type 'gerbil\x23;lexer::t 'lexer (list object::t)
        '(input pos len tokens peeked heredocs want-more? line col
           extglob?)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-lexer . args)
      (let* ([type lexer::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (lexer? obj)
      (\x23;\x23;structure-instance-of? obj 'gerbil\x23;lexer::t))
    (define (lexer-input obj) (unchecked-slot-ref obj 'input))
    (define (lexer-pos obj) (unchecked-slot-ref obj 'pos))
    (define (lexer-len obj) (unchecked-slot-ref obj 'len))
    (define (lexer-tokens obj) (unchecked-slot-ref obj 'tokens))
    (define (lexer-peeked obj) (unchecked-slot-ref obj 'peeked))
    (define (lexer-heredocs obj)
      (unchecked-slot-ref obj 'heredocs))
    (define (lexer-want-more? obj)
      (unchecked-slot-ref obj 'want-more?))
    (define (lexer-line obj) (unchecked-slot-ref obj 'line))
    (define (lexer-col obj) (unchecked-slot-ref obj 'col))
    (define (lexer-extglob? obj)
      (unchecked-slot-ref obj 'extglob?))
    (define (lexer-input-set! obj val)
      (unchecked-slot-set! obj 'input val))
    (define (lexer-pos-set! obj val)
      (unchecked-slot-set! obj 'pos val))
    (define (lexer-len-set! obj val)
      (unchecked-slot-set! obj 'len val))
    (define (lexer-tokens-set! obj val)
      (unchecked-slot-set! obj 'tokens val))
    (define (lexer-peeked-set! obj val)
      (unchecked-slot-set! obj 'peeked val))
    (define (lexer-heredocs-set! obj val)
      (unchecked-slot-set! obj 'heredocs val))
    (define (lexer-want-more?-set! obj val)
      (unchecked-slot-set! obj 'want-more? val))
    (define (lexer-line-set! obj val)
      (unchecked-slot-set! obj 'line val))
    (define (lexer-col-set! obj val)
      (unchecked-slot-set! obj 'col val))
    (define (lexer-extglob?-set! obj val)
      (unchecked-slot-set! obj 'extglob? val))
    (define (&lexer-input obj) (unchecked-slot-ref obj 'input))
    (define (&lexer-pos obj) (unchecked-slot-ref obj 'pos))
    (define (&lexer-len obj) (unchecked-slot-ref obj 'len))
    (define (&lexer-tokens obj)
      (unchecked-slot-ref obj 'tokens))
    (define (&lexer-peeked obj)
      (unchecked-slot-ref obj 'peeked))
    (define (&lexer-heredocs obj)
      (unchecked-slot-ref obj 'heredocs))
    (define (&lexer-want-more? obj)
      (unchecked-slot-ref obj 'want-more?))
    (define (&lexer-line obj) (unchecked-slot-ref obj 'line))
    (define (&lexer-col obj) (unchecked-slot-ref obj 'col))
    (define (&lexer-extglob? obj)
      (unchecked-slot-ref obj 'extglob?))
    (define (&lexer-input-set! obj val)
      (unchecked-slot-set! obj 'input val))
    (define (&lexer-pos-set! obj val)
      (unchecked-slot-set! obj 'pos val))
    (define (&lexer-len-set! obj val)
      (unchecked-slot-set! obj 'len val))
    (define (&lexer-tokens-set! obj val)
      (unchecked-slot-set! obj 'tokens val))
    (define (&lexer-peeked-set! obj val)
      (unchecked-slot-set! obj 'peeked val))
    (define (&lexer-heredocs-set! obj val)
      (unchecked-slot-set! obj 'heredocs val))
    (define (&lexer-want-more?-set! obj val)
      (unchecked-slot-set! obj 'want-more? val))
    (define (&lexer-line-set! obj val)
      (unchecked-slot-set! obj 'line val))
    (define (&lexer-col-set! obj val)
      (unchecked-slot-set! obj 'col val))
    (define (&lexer-extglob?-set! obj val)
      (unchecked-slot-set! obj 'extglob? val)))
  (define make-shell-lexer
    (case-lambda
      [(input)
       (let* ([extglob? #f])
         (make-lexer input 0 (string-length input) (list) #f (list)
           #f 1 0 extglob?))]
      [(input extglob?)
       (make-lexer input 0 (string-length input) (list) #f (list)
         #f 1 0 extglob?)]))
  (define (lexer-next! lex)
    (if (lexer-peeked lex)
        (let ([tok (lexer-peeked lex)])
          (lexer-peeked-set! lex #f)
          tok)
        (lexer-read-token! lex)))
  (define (lexer-peek lex)
    (unless (lexer-peeked lex)
      (lexer-peeked-set! lex (lexer-read-token! lex)))
    (lexer-peeked lex))
  (define (lexer-needs-more? lex) (lexer-want-more? lex))
  (define (lexer-prepend-text! lex text)
    (let* ([remaining (substring
                        (lexer-input lex)
                        (lexer-pos lex)
                        (lexer-len lex))])
      (let* ([new-input (string-append text remaining)])
        (lexer-input-set! lex new-input)
        (lexer-pos-set! lex 0)
        (lexer-len-set! lex (string-length new-input))
        (lexer-peeked-set! lex #f))))
  (define (tokenize input)
    (let ([lex (make-shell-lexer input)])
      (let loop ([tokens (list)])
        (let ([tok (lexer-next! lex)])
          (if (eq? tok 'eof)
              (reverse tokens)
              (loop (cons tok tokens)))))))
  (define lexer-read-token!-cell
    (vector
      (lambda (lex)
        (skip-whitespace! lex)
        (if (>= (lexer-pos lex) (lexer-len lex))
            (if (pair? (lexer-heredocs lex))
                (begin (lexer-want-more?-set! lex #t) 'eof)
                'eof)
            (let ([ch (current-char lex)])
              (cond
                [(char=? ch #\#)
                 (skip-comment! lex)
                 (lexer-read-token! lex)]
                [(char=? ch #\newline)
                 (advance! lex)
                 (when (pair? (lexer-heredocs lex))
                   (collect-heredocs! lex))
                 (make-token
                   'NEWLINE
                   "\n"
                   (cons (lexer-line lex) (lexer-col lex)))]
                [(operator-start? ch) (read-operator! lex)]
                [(char=? ch #\') (read-single-quoted! lex)]
                [(char=? ch #\") (read-double-quoted! lex)]
                [(and (char=? ch #\$)
                      (< (+ (lexer-pos lex) 1) (lexer-len lex))
                      (char=? (char-at lex (+ (lexer-pos lex) 1)) #\'))
                 (read-ansi-c-quoted! lex)]
                [(and (char=? ch #\$)
                      (< (+ (lexer-pos lex) 1) (lexer-len lex))
                      (char=? (char-at lex (+ (lexer-pos lex) 1)) #\"))
                 (advance! lex)
                 (read-double-quoted! lex)]
                [(char=? ch #\\)
                 (if (and (< (+ (lexer-pos lex) 1) (lexer-len lex))
                          (char=?
                            (char-at lex (+ (lexer-pos lex) 1))
                            #\newline))
                     (begin
                       (advance! lex)
                       (advance! lex)
                       (lexer-read-token! lex))
                     (read-word! lex))]
                [else (read-word! lex)]))))))
  (define-syntax lexer-read-token!
    (identifier-syntax
      [id (vector-ref lexer-read-token!-cell 0)]
      [(set! id v) (vector-set! lexer-read-token!-cell 0 v)]))
  (define (current-char lex)
    (string-ref (lexer-input lex) (lexer-pos lex)))
  (define (char-at lex pos)
    (string-ref (lexer-input lex) pos))
  (define (advance! lex)
    (when (< (lexer-pos lex) (lexer-len lex))
      (when (char=?
              (string-ref (lexer-input lex) (lexer-pos lex))
              #\newline)
        (lexer-line-set! lex (+ 1 (lexer-line lex)))
        (lexer-col-set! lex 0))
      (lexer-pos-set! lex (+ 1 (lexer-pos lex)))
      (lexer-col-set! lex (+ 1 (lexer-col lex)))))
  (define (at-end? lex) (>= (lexer-pos lex) (lexer-len lex)))
  (define (skip-whitespace! lex)
    (let loop ()
      (when (and (not (at-end? lex))
                 (let ([ch (current-char lex)])
                   (and (char-whitespace? ch)
                        (not (char=? ch #\newline)))))
        (advance! lex)
        (loop))))
  (define (skip-comment! lex)
    (let loop ()
      (when (and (not (at-end? lex))
                 (not (char=? (current-char lex) #\newline)))
        (advance! lex)
        (loop))))
  (define (operator-start? ch)
    (memq ch '(#\| #\& #\; #\( #\) #\< #\> #\!)))
  (define (read-procsub-body! lex)
    (let ([buf (open-output-string)])
      (let loop ([depth 1])
        (cond
          [(at-end? lex) (lexer-want-more?-set! lex #t)]
          [(char=? (current-char lex) #\()
           (display #\( buf)
           (advance! lex)
           (loop (+ depth 1))]
          [(char=? (current-char lex) #\))
           (if (= depth 1)
               (advance! lex)
               (begin
                 (display #\) buf)
                 (advance! lex)
                 (loop (- depth 1))))]
          [(char=? (current-char lex) #\')
           (display #\' buf)
           (advance! lex)
           (let inner ()
             (cond
               [(at-end? lex) (%%void)]
               [(char=? (current-char lex) #\')
                (display #\' buf)
                (advance! lex)]
               [else
                (display (current-char lex) buf)
                (advance! lex)
                (inner)]))
           (loop depth)]
          [(char=? (current-char lex) #\\)
           (display #\\ buf)
           (advance! lex)
           (unless (at-end? lex)
             (display (current-char lex) buf)
             (advance! lex))
           (loop depth)]
          [(char=? (current-char lex) #\")
           (display #\" buf)
           (advance! lex)
           (let inner ()
             (cond
               [(at-end? lex) (%%void)]
               [(char=? (current-char lex) #\")
                (display #\" buf)
                (advance! lex)]
               [(char=? (current-char lex) #\\)
                (display #\\ buf)
                (advance! lex)
                (unless (at-end? lex)
                  (display (current-char lex) buf)
                  (advance! lex))
                (inner)]
               [else
                (display (current-char lex) buf)
                (advance! lex)
                (inner)]))
           (loop depth)]
          [else
           (display (current-char lex) buf)
           (advance! lex)
           (loop depth)]))
      (get-output-string buf)))
  (define (read-operator! lex)
    (let* ([pos (cons (lexer-line lex) (lexer-col lex))])
      (let* ([ch (current-char lex)])
        (advance! lex)
        (case ch
          [(#\|)
           (cond
             [(and (not (at-end? lex)) (char=? (current-char lex) #\|))
              (advance! lex)
              (make-token 'OR_IF "||" pos)]
             [(and (not (at-end? lex)) (char=? (current-char lex) #\&))
              (advance! lex)
              (make-token 'PIPEAMP "|&" pos)]
             [else (make-token 'PIPE "|" pos)])]
          [(#\&)
           (cond
             [(and (not (at-end? lex)) (char=? (current-char lex) #\&))
              (advance! lex)
              (make-token 'AND_IF "&&" pos)]
             [(and (not (at-end? lex)) (char=? (current-char lex) #\>))
              (advance! lex)
              (if (and (not (at-end? lex))
                       (char=? (current-char lex) #\>))
                  (begin
                    (advance! lex)
                    (make-token 'AMPGREAT_GREAT "&>>" pos))
                  (make-token 'AMPGREAT "&>" pos))]
             [else (make-token 'AMP "&" pos)])]
          [(#\;)
           (cond
             [(and (not (at-end? lex)) (char=? (current-char lex) #\;))
              (advance! lex)
              (if (and (not (at-end? lex))
                       (char=? (current-char lex) #\&))
                  (begin (advance! lex) (make-token 'DSEMI_AMP ";;&" pos))
                  (make-token 'DSEMI ";;" pos))]
             [(and (not (at-end? lex)) (char=? (current-char lex) #\&))
              (advance! lex)
              (make-token 'SEMI_AMP ";&" pos)]
             [else (make-token 'SEMI ";" pos)])]
          [(#\() (make-token 'LPAREN "(" pos)]
          [(#\)) (make-token 'RPAREN ")" pos)]
          [(#\!) (make-token 'BANG "!" pos)]
          [(#\<)
           (cond
             [(and (not (at-end? lex)) (char=? (current-char lex) #\<))
              (advance! lex)
              (if (and (not (at-end? lex))
                       (char=? (current-char lex) #\-))
                  (begin (advance! lex) (make-token 'DLESSDASH "<<-" pos))
                  (if (and (not (at-end? lex))
                           (char=? (current-char lex) #\<))
                      (begin (advance! lex) (make-token 'TLESS "<<<" pos))
                      (make-token 'DLESS "<<" pos)))]
             [(and (not (at-end? lex)) (char=? (current-char lex) #\&))
              (advance! lex)
              (make-token 'LESSAND "<&" pos)]
             [(and (not (at-end? lex)) (char=? (current-char lex) #\>))
              (advance! lex)
              (make-token 'LESSGREAT "<>" pos)]
             [(and (not (at-end? lex)) (char=? (current-char lex) #\())
              (advance! lex)
              (make-token 'PROCSUB_IN (read-procsub-body! lex) pos)]
             [else (make-token 'LESS "<" pos)])]
          [(#\>)
           (cond
             [(and (not (at-end? lex)) (char=? (current-char lex) #\>))
              (advance! lex)
              (make-token 'DGREAT ">>" pos)]
             [(and (not (at-end? lex)) (char=? (current-char lex) #\&))
              (advance! lex)
              (make-token 'GREATAND ">&" pos)]
             [(and (not (at-end? lex)) (char=? (current-char lex) #\|))
              (advance! lex)
              (make-token 'CLOBBER ">|" pos)]
             [(and (not (at-end? lex)) (char=? (current-char lex) #\())
              (advance! lex)
              (make-token 'PROCSUB_OUT (read-procsub-body! lex) pos)]
             [else (make-token 'GREAT ">" pos)])]
          [else (make-token 'WORD (string ch) pos)]))))
  (define (extglob-prefix-before? lex)
    (let ([p (lexer-pos lex)])
      (and (> p 0)
           (let ([prev-ch (char-at lex (- p 1))])
             (or (char=? prev-ch #\?)
                 (char=? prev-ch #\*)
                 (char=? prev-ch #\+)
                 (char=? prev-ch #\@)
                 (char=? prev-ch #\!))))))
  (define (read-extglob-body! lex buf)
    (display (current-char lex) buf)
    (advance! lex)
    (let loop ([depth 1])
      (cond
        [(at-end? lex) (lexer-want-more?-set! lex #t)]
        [else
         (let ([ch (current-char lex)])
           (cond
             [(char=? ch #\\)
              (display ch buf)
              (advance! lex)
              (unless (at-end? lex)
                (display (current-char lex) buf)
                (advance! lex))
              (loop depth)]
             [(char=? ch #\')
              (let ([quoted (read-single-quoted-content! lex)])
                (display "'" buf)
                (display quoted buf)
                (display "'" buf))
              (loop depth)]
             [(char=? ch #\")
              (let ([quoted (read-double-quoted-content! lex)])
                (display "\"" buf)
                (display quoted buf)
                (display "\"" buf))
              (loop depth)]
             [(char=? ch #\()
              (display ch buf)
              (advance! lex)
              (loop (+ depth 1))]
             [(char=? ch #\))
              (display ch buf)
              (advance! lex)
              (when (> depth 1) (loop (- depth 1)))]
             [(char=? ch #\|)
              (display ch buf)
              (advance! lex)
              (loop depth)]
             [(char=? ch #\$)
              (display (read-dollar! lex) buf)
              (loop depth)]
             [(char=? ch #\`)
              (display (read-backtick! lex) buf)
              (loop depth)]
             [else (display ch buf) (advance! lex) (loop depth)]))])))
  (define (read-word! lex)
    (let* ([pos (cons (lexer-line lex) (lexer-col lex))])
      (let* ([buf (open-output-string)])
        (let loop ([has-content? #f])
          (if (at-end? lex)
              (finish-word buf pos)
              (let ([ch (current-char lex)])
                (cond
                  [(or (char-whitespace? ch)
                       (and (operator-start? ch)
                            (not (and (char=? ch #\!) has-content?))
                            (not (and (char=? ch #\!)
                                      (not has-content?)
                                      (< (+ (lexer-pos lex) 1)
                                         (lexer-len lex))
                                      (let ([next (char-at
                                                    lex
                                                    (+ (lexer-pos lex)
                                                       1))])
                                        (not (or (char-whitespace? next)
                                                 (memq
                                                   next
                                                   '(#\; #\| #\& #\( #\)
                                                         #\< #\>
                                                         #\newline)))))))
                            (not (and (lexer-extglob? lex)
                                      (char=? ch #\!)
                                      (< (+ (lexer-pos lex) 1)
                                         (lexer-len lex))
                                      (char=?
                                        (char-at lex (+ (lexer-pos lex) 1))
                                        #\()))
                            (not (and (lexer-extglob? lex)
                                      (char=? ch #\()
                                      (extglob-prefix-before? lex)))))
                   (finish-word buf pos)]
                  [(and (lexer-extglob? lex)
                        (char=? ch #\()
                        (extglob-prefix-before? lex))
                   (read-extglob-body! lex buf)
                   (loop #t)]
                  [(char=? ch #\')
                   (let ([quoted (read-single-quoted-content! lex)])
                     (display "'" buf)
                     (display quoted buf)
                     (display "'" buf)
                     (loop #t))]
                  [(char=? ch #\")
                   (let ([quoted (read-double-quoted-content! lex)])
                     (display "\"" buf)
                     (display quoted buf)
                     (display "\"" buf)
                     (loop #t))]
                  [(char=? ch #\\)
                   (advance! lex)
                   (if (at-end? lex)
                       (begin (display "\\" buf) (finish-word buf pos))
                       (if (char=? (current-char lex) #\newline)
                           (begin (advance! lex) (loop has-content?))
                           (begin
                             (display "\\" buf)
                             (display (current-char lex) buf)
                             (advance! lex)
                             (loop #t))))]
                  [(char=? ch #\$)
                   (if (< (+ (lexer-pos lex) 1) (lexer-len lex))
                       (let ([next (char-at lex (+ (lexer-pos lex) 1))])
                         (cond
                           [(char=? next #\')
                            (display (read-ansi-c-quoted-content! lex) buf)
                            (loop #t)]
                           [(char=? next #\")
                            (advance! lex)
                            (let ([quoted (read-double-quoted-content!
                                            lex)])
                              (display "\"" buf)
                              (display quoted buf)
                              (display "\"" buf)
                              (loop #t))]
                           [else
                            (display (read-dollar! lex) buf)
                            (loop #t)]))
                       (begin (display (read-dollar! lex) buf) (loop #t)))]
                  [(char=? ch #\`)
                   (display (read-backtick! lex) buf)
                   (loop #t)]
                  [else (display ch buf) (advance! lex) (loop #t)])))))))
  (define (finish-word buf pos)
    (let ([word (get-output-string buf)])
      (if (= (string-length word) 0)
          (error 'gerbil "lexer: empty word")
          (cond
            [(and (> (string-length word) 0)
                  (let check-digits ([i 0])
                    (if (>= i (string-length word))
                        #t
                        (and (char-numeric? (string-ref word i))
                             (check-digits (+ i 1)))))
                  (not (at-end? (current-lexer)))
                  (let ([ch (current-char (current-lexer))])
                    (or (char=? ch #\<) (char=? ch #\>))))
             (make-token 'IO_NUMBER word pos)]
            [(and (> (string-length word) 2)
                  (char=? (string-ref word 0) #\{)
                  (char=? (string-ref word (- (string-length word) 1)) #\})
                  (let* ([name (substring
                                 word
                                 1
                                 (- (string-length word) 1))])
                    (let* ([len (string-length name)])
                      (and (> len 0)
                           (let ([c0 (string-ref name 0)])
                             (or (char-alphabetic? c0) (char=? c0 #\_)))
                           (let check-name ([i 1])
                             (if (>= i len)
                                 #t
                                 (let ([c (string-ref name i)])
                                   (and (or (char-alphabetic? c)
                                            (char-numeric? c)
                                            (char=? c #\_))
                                        (check-name (+ i 1)))))))))
                  (not (at-end? (current-lexer)))
                  (let ([ch (current-char (current-lexer))])
                    (or (char=? ch #\<) (char=? ch #\>))))
             (make-token
               'IO_VARNAME
               (substring word 1 (- (string-length word) 1))
               pos)]
            [(assignment-word? word)
             (make-token 'ASSIGNMENT_WORD word pos)]
            [else (make-token 'WORD word pos)]))))
  (define current-lexer (make-parameter #f))
  (define (read-word-with-context! lex)
    (parameterize ([current-lexer lex]) (read-word! lex)))
  (define (lexer-read-token-impl! lex)
    (parameterize ([current-lexer lex])
      (if (pair? (lexer-tokens lex))
          (let ([tok (car (lexer-tokens lex))])
            (lexer-tokens-set! lex (cdr (lexer-tokens lex)))
            tok)
          (begin
            (skip-whitespace! lex)
            (if (>= (lexer-pos lex) (lexer-len lex))
                (if (pair? (lexer-heredocs lex))
                    (begin (lexer-want-more?-set! lex #t) 'eof)
                    'eof)
                (let ([ch (current-char lex)])
                  (cond
                    [(char=? ch #\#)
                     (skip-comment! lex)
                     (lexer-read-token-impl! lex)]
                    [(char=? ch #\newline)
                     (advance! lex)
                     (when (pair? (lexer-heredocs lex))
                       (collect-heredocs! lex))
                     (make-token
                       'NEWLINE
                       "\n"
                       (cons (lexer-line lex) (lexer-col lex)))]
                    [(and (lexer-extglob? lex)
                          (char=? ch #\!)
                          (< (+ (lexer-pos lex) 1) (lexer-len lex))
                          (char=? (char-at lex (+ (lexer-pos lex) 1)) #\())
                     (read-word! lex)]
                    [(and (char=? ch #\!)
                          (or (>= (+ (lexer-pos lex) 1) (lexer-len lex))
                              (let ([next (char-at
                                            lex
                                            (+ (lexer-pos lex) 1))])
                                (or (char-whitespace? next)
                                    (memq
                                      next
                                      '(#\; #\| #\& #\( #\) #\< #\>
                                            #\newline))))))
                     (read-operator! lex)]
                    [(and (char=? ch #\!)
                          (< (+ (lexer-pos lex) 1) (lexer-len lex)))
                     (read-word! lex)]
                    [(operator-start? ch) (read-operator! lex)]
                    [(char=? ch #\') (read-word! lex)]
                    [(char=? ch #\") (read-word! lex)]
                    [(and (char=? ch #\$)
                          (< (+ (lexer-pos lex) 1) (lexer-len lex))
                          (char=? (char-at lex (+ (lexer-pos lex) 1)) #\'))
                     (read-word! lex)]
                    [(and (char=? ch #\$)
                          (< (+ (lexer-pos lex) 1) (lexer-len lex))
                          (char=? (char-at lex (+ (lexer-pos lex) 1)) #\"))
                     (read-word! lex)]
                    [(char=? ch #\\)
                     (if (and (< (+ (lexer-pos lex) 1) (lexer-len lex))
                              (char=?
                                (char-at lex (+ (lexer-pos lex) 1))
                                #\newline))
                         (begin
                           (advance! lex)
                           (advance! lex)
                           (lexer-read-token-impl! lex))
                         (read-word! lex))]
                    [else (read-word! lex)])))))))
  (define (read-single-quoted! lex)
    (let ([pos (cons (lexer-line lex) (lexer-col lex))]
          [content (read-single-quoted-content! lex)])
      (make-token 'WORD (string-append "'" content "'") pos)))
  (define (read-single-quoted-content! lex)
    (advance! lex)
    (let ([buf (open-output-string)])
      (let loop ()
        (cond
          [(at-end? lex)
           (lexer-want-more?-set! lex #t)
           (get-output-string buf)]
          [(char=? (current-char lex) #\')
           (advance! lex)
           (get-output-string buf)]
          [else
           (display (current-char lex) buf)
           (advance! lex)
           (loop)]))))
  (define (read-double-quoted! lex)
    (let ([pos (cons (lexer-line lex) (lexer-col lex))]
          [content (read-double-quoted-content! lex)])
      (make-token 'WORD (string-append "\"" content "\"") pos)))
  (define (read-double-quoted-content! lex)
    (advance! lex)
    (let ([buf (open-output-string)])
      (let loop ()
        (cond
          [(at-end? lex)
           (lexer-want-more?-set! lex #t)
           (get-output-string buf)]
          [(char=? (current-char lex) #\")
           (advance! lex)
           (get-output-string buf)]
          [(char=? (current-char lex) #\\)
           (advance! lex)
           (if (at-end? lex)
               (begin (display "\\" buf) (get-output-string buf))
               (let ([next (current-char lex)])
                 (if (memq next '(#\$ #\` #\" #\\ #\newline))
                     (if (char=? next #\newline)
                         (begin (advance! lex) (loop))
                         (begin
                           (display "\\" buf)
                           (display next buf)
                           (advance! lex)
                           (loop)))
                     (begin
                       (display "\\" buf)
                       (display next buf)
                       (advance! lex)
                       (loop)))))]
          [(char=? (current-char lex) #\$)
           (display (read-dollar! lex) buf)
           (loop)]
          [(char=? (current-char lex) #\`)
           (display (read-backtick! lex) buf)
           (loop)]
          [else
           (display (current-char lex) buf)
           (advance! lex)
           (loop)]))))
  (define (ansi-c-emit-char! ch buf)
    (if (memq ch '(#\" #\$ #\` #\\))
        (begin (display "\\" buf) (display ch buf))
        (display ch buf)))
  (define (read-ansi-c-quoted-content! lex)
    (advance! lex)
    (advance! lex)
    (let ([buf (open-output-string)])
      (display "\"" buf)
      (let loop ()
        (cond
          [(at-end? lex)
           (lexer-want-more?-set! lex #t)
           (display "\"" buf)
           (get-output-string buf)]
          [(char=? (current-char lex) #\')
           (advance! lex)
           (display "\"" buf)
           (get-output-string buf)]
          [(char=? (current-char lex) #\\)
           (advance! lex)
           (if (at-end? lex)
               (begin
                 (display "\\\\" buf)
                 (display "\"" buf)
                 (get-output-string buf))
               (let ([esc (current-char lex)])
                 (advance! lex)
                 (case esc
                   [(#\n) (ansi-c-emit-char! #\newline buf)]
                   [(#\t) (ansi-c-emit-char! #\tab buf)]
                   [(#\r) (ansi-c-emit-char! #\return buf)]
                   [(#\a) (display (string (integer->char 7)) buf)]
                   [(#\b) (display (string (integer->char 8)) buf)]
                   [(#\e #\E) (display (string (integer->char 27)) buf)]
                   [(#\f) (display (string (integer->char 12)) buf)]
                   [(#\v) (display (string (integer->char 11)) buf)]
                   [(#\\) (display "\\\\" buf)]
                   [(#\') (display "'" buf)]
                   [(#\") (display "\\\"" buf)]
                   [(#\x)
                    (let-values ([(val cnt) (read-hex-digits lex 2)])
                      (if (> cnt 0)
                          (ansi-c-emit-char!
                            (byte->raw-char (bitwise-and val 255))
                            buf)
                          (begin (display "\\\\x" buf))))]
                   [(#\u #\U)
                    (let* ([max-digits (if (char=? esc #\u) 4 8)])
                      (let-values ([(val cnt)
                                    (read-hex-digits lex max-digits)])
                        (if (> cnt 0)
                            (ansi-c-emit-char! (integer->char val) buf)
                            (begin
                              (display "\\\\" buf)
                              (display esc buf)))))]
                   [(#\c)
                    (if (at-end? lex)
                        (begin (display "\\\\c" buf))
                        (let ([ctrl-ch (current-char lex)])
                          (advance! lex)
                          (ansi-c-emit-char!
                            (integer->char
                              (bitwise-and (char->integer ctrl-ch) 31))
                            buf)))]
                   [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
                    (let* ([first-digit (- (char->integer esc)
                                           (char->integer #\0))])
                      (let* ([more (read-octal-digits-with-init
                                     lex
                                     2
                                     first-digit)])
                        (ansi-c-emit-char!
                          (byte->raw-char (bitwise-and more 255))
                          buf)))]
                   [else (display "\\" buf) (ansi-c-emit-char! esc buf)])
                 (loop)))]
          [else
           (ansi-c-emit-char! (current-char lex) buf)
           (advance! lex)
           (loop)]))))
  (define (read-ansi-c-quoted! lex)
    (let ([pos (cons (lexer-line lex) (lexer-col lex))]
          [content (read-ansi-c-quoted-content! lex)])
      (make-token 'WORD content pos)))
  (define (read-dollar! lex)
    (let ([buf (open-output-string)])
      (display "$" buf)
      (advance! lex)
      (cond
        [(at-end? lex) (get-output-string buf)]
        [(and (char=? (current-char lex) #\()
              (< (+ (lexer-pos lex) 1) (lexer-len lex))
              (char=? (char-at lex (+ (lexer-pos lex) 1)) #\())
         (display "((" buf)
         (advance! lex)
         (advance! lex)
         (let loop ([paren-depth 0])
           (cond
             [(at-end? lex) (lexer-want-more?-set! lex #t)]
             [(and (= paren-depth 0)
                   (char=? (current-char lex) #\))
                   (< (+ (lexer-pos lex) 1) (lexer-len lex))
                   (char=? (char-at lex (+ (lexer-pos lex) 1)) #\)))
              (display "))" buf)
              (advance! lex)
              (advance! lex)]
             [(and (char=? (current-char lex) #\$)
                   (< (+ (lexer-pos lex) 2) (lexer-len lex))
                   (char=? (char-at lex (+ (lexer-pos lex) 1)) #\()
                   (char=? (char-at lex (+ (lexer-pos lex) 2)) #\())
              (display "$((" buf)
              (advance! lex)
              (advance! lex)
              (advance! lex)
              (let inner ([pd 0])
                (cond
                  [(at-end? lex) (lexer-want-more?-set! lex #t)]
                  [(and (= pd 0)
                        (char=? (current-char lex) #\))
                        (< (+ (lexer-pos lex) 1) (lexer-len lex))
                        (char=? (char-at lex (+ (lexer-pos lex) 1)) #\)))
                   (display "))" buf)
                   (advance! lex)
                   (advance! lex)]
                  [(char=? (current-char lex) #\()
                   (display (current-char lex) buf)
                   (advance! lex)
                   (inner (+ pd 1))]
                  [(and (char=? (current-char lex) #\)) (> pd 0))
                   (display (current-char lex) buf)
                   (advance! lex)
                   (inner (- pd 1))]
                  [else
                   (display (current-char lex) buf)
                   (advance! lex)
                   (inner pd)]))
              (loop paren-depth)]
             [(char=? (current-char lex) #\()
              (display (current-char lex) buf)
              (advance! lex)
              (loop (+ paren-depth 1))]
             [(and (char=? (current-char lex) #\)) (> paren-depth 0))
              (display (current-char lex) buf)
              (advance! lex)
              (loop (- paren-depth 1))]
             [(char=? (current-char lex) #\\)
              (advance! lex)
              (cond
                [(at-end? lex) (display "\\" buf)]
                [(char=? (current-char lex) #\newline)
                 (advance! lex)
                 (loop paren-depth)]
                [else
                 (display "\\" buf)
                 (display (current-char lex) buf)
                 (advance! lex)
                 (loop paren-depth)])]
             [else
              (display (current-char lex) buf)
              (advance! lex)
              (loop paren-depth)]))
         (get-output-string buf)]
        [(char=? (current-char lex) #\()
         (display "(" buf)
         (advance! lex)
         (let loop ([depth 1] [case-depth 0] [word ""])
           (define (update-case-depth w cd)
             (cond
               [(string=? w "case") (+ cd 1)]
               [(string=? w "esac") (max 0 (- cd 1))]
               [else cd]))
           (cond
             [(at-end? lex) (lexer-want-more?-set! lex #t)]
             [(char=? (current-char lex) #\()
              (let ([cd (update-case-depth word case-depth)])
                (display "(" buf)
                (advance! lex)
                (loop (+ depth 1) cd ""))]
             [(char=? (current-char lex) #\))
              (let ([cd (update-case-depth word case-depth)])
                (if (and (= depth 1) (> cd 0))
                    (begin
                      (display ")" buf)
                      (advance! lex)
                      (loop depth cd ""))
                    (begin
                      (display ")" buf)
                      (advance! lex)
                      (when (> depth 1) (loop (- depth 1) cd "")))))]
             [(char=? (current-char lex) #\')
              (let ([cd (update-case-depth word case-depth)])
                (display "'" buf)
                (advance! lex)
                (let inner ()
                  (cond
                    [(at-end? lex) (%%void)]
                    [(char=? (current-char lex) #\')
                     (display "'" buf)
                     (advance! lex)]
                    [else
                     (display (current-char lex) buf)
                     (advance! lex)
                     (inner)]))
                (loop depth cd ""))]
             [(char=? (current-char lex) #\")
              (let ([cd (update-case-depth word case-depth)])
                (display "\"" buf)
                (advance! lex)
                (let inner ()
                  (cond
                    [(at-end? lex) (%%void)]
                    [(char=? (current-char lex) #\\)
                     (display "\\" buf)
                     (advance! lex)
                     (unless (at-end? lex)
                       (display (current-char lex) buf)
                       (advance! lex))
                     (inner)]
                    [(char=? (current-char lex) #\")
                     (display "\"" buf)
                     (advance! lex)]
                    [else
                     (display (current-char lex) buf)
                     (advance! lex)
                     (inner)]))
                (loop depth cd ""))]
             [(char=? (current-char lex) #\\)
              (let ([cd (update-case-depth word case-depth)])
                (display "\\" buf)
                (advance! lex)
                (unless (at-end? lex)
                  (display (current-char lex) buf)
                  (advance! lex))
                (loop depth cd ""))]
             [(or (char-alphabetic? (current-char lex))
                  (char=? (current-char lex) #\_))
              (let ([ch (current-char lex)])
                (display ch buf)
                (advance! lex)
                (loop depth case-depth (string-append word (string ch))))]
             [else
              (let ([cd (update-case-depth word case-depth)])
                (display (current-char lex) buf)
                (advance! lex)
                (loop depth cd ""))]))
         (get-output-string buf)]
        [(char=? (current-char lex) #\{)
         (display "{" buf)
         (advance! lex)
         (let loop ([depth 1])
           (cond
             [(at-end? lex) (lexer-want-more?-set! lex #t)]
             [(char=? (current-char lex) #\{)
              (display "{" buf)
              (advance! lex)
              (loop (+ depth 1))]
             [(char=? (current-char lex) #\})
              (display "}" buf)
              (advance! lex)
              (when (> depth 1) (loop (- depth 1)))]
             [(char=? (current-char lex) #\\)
              (advance! lex)
              (cond
                [(at-end? lex) (display "\\" buf)]
                [(char=? (current-char lex) #\newline)
                 (advance! lex)
                 (loop depth)]
                [else
                 (display "\\" buf)
                 (display (current-char lex) buf)
                 (advance! lex)
                 (loop depth)])]
             [(char=? (current-char lex) #\')
              (display "'" buf)
              (advance! lex)
              (let inner ()
                (cond
                  [(at-end? lex) (lexer-want-more?-set! lex #t)]
                  [(char=? (current-char lex) #\')
                   (display "'" buf)
                   (advance! lex)]
                  [else
                   (display (current-char lex) buf)
                   (advance! lex)
                   (inner)]))
              (loop depth)]
             [(char=? (current-char lex) #\")
              (display "\"" buf)
              (advance! lex)
              (let inner ()
                (cond
                  [(at-end? lex) (lexer-want-more?-set! lex #t)]
                  [(char=? (current-char lex) #\")
                   (display "\"" buf)
                   (advance! lex)]
                  [(char=? (current-char lex) #\\)
                   (display "\\" buf)
                   (advance! lex)
                   (unless (at-end? lex)
                     (display (current-char lex) buf)
                     (advance! lex))
                   (inner)]
                  [else
                   (display (current-char lex) buf)
                   (advance! lex)
                   (inner)]))
              (loop depth)]
             [(char=? (current-char lex) #\$)
              (display (read-dollar! lex) buf)
              (loop depth)]
             [(char=? (current-char lex) #\`)
              (display (read-backtick! lex) buf)
              (loop depth)]
             [else
              (display (current-char lex) buf)
              (advance! lex)
              (loop depth)]))
         (get-output-string buf)]
        [(or (char-alphabetic? (current-char lex))
             (char=? (current-char lex) #\_))
         (let loop ()
           (if (and (not (at-end? lex))
                    (let ([ch (current-char lex)])
                      (or (char-alphabetic? ch)
                          (char-numeric? ch)
                          (char=? ch #\_))))
               (begin
                 (display (current-char lex) buf)
                 (advance! lex)
                 (loop))
               (get-output-string buf)))]
        [(memq
           (current-char lex)
           '(#\? #\$ #\! #\# #\* #\@ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6
                 #\7 #\8 #\9))
         (display (current-char lex) buf)
         (advance! lex)
         (get-output-string buf)]
        [else (get-output-string buf)])))
  (define (read-backtick! lex)
    (let ([buf (open-output-string)])
      (display "`" buf)
      (advance! lex)
      (let loop ()
        (cond
          [(at-end? lex)
           (lexer-want-more?-set! lex #t)
           (get-output-string buf)]
          [(char=? (current-char lex) #\`)
           (display "`" buf)
           (advance! lex)
           (get-output-string buf)]
          [(char=? (current-char lex) #\\)
           (display "\\" buf)
           (advance! lex)
           (unless (at-end? lex)
             (display (current-char lex) buf)
             (advance! lex))
           (loop)]
          [else
           (display (current-char lex) buf)
           (advance! lex)
           (loop)]))))
  (define (collect-heredocs! lex)
    (let loop ([specs (reverse (lexer-heredocs lex))]
               [acc (list)])
      (if (pair? specs)
          (let* ([spec (car specs)])
            (let* ([delimiter (car spec)])
              (let* ([strip-tabs? (cadr spec)])
                (let* ([body (collect-heredoc-body!
                               lex
                               delimiter
                               strip-tabs?)])
                  (loop
                    (cdr specs)
                    (cons
                      (make-token
                        'HEREDOC_BODY
                        body
                        (cons (lexer-line lex) (lexer-col lex)))
                      acc))))))
          (lexer-tokens-set!
            lex
            (append (reverse acc) (lexer-tokens lex)))))
    (lexer-heredocs-set! lex (list)))
  (define (collect-heredoc-body! lex delimiter strip-tabs?)
    (let ([buf (open-output-string)])
      (let loop ()
        (if (at-end? lex)
            (begin
              (lexer-want-more?-set! lex #t)
              (get-output-string buf))
            (let ([line (read-line-from-lexer! lex)])
              (let ([trimmed (if strip-tabs?
                                 (string-trim-tabs line)
                                 line)])
                (if (string=? trimmed delimiter)
                    (get-output-string buf)
                    (begin
                      (display trimmed buf)
                      (display "\n" buf)
                      (loop)))))))))
  (define (read-line-from-lexer! lex)
    (let ([buf (open-output-string)])
      (let loop ()
        (cond
          [(at-end? lex) (get-output-string buf)]
          [(char=? (current-char lex) #\newline)
           (advance! lex)
           (get-output-string buf)]
          [else
           (display (current-char lex) buf)
           (advance! lex)
           (loop)]))))
  (define (string-trim-tabs str)
    (let loop ([i 0])
      (if (and (< i (string-length str))
               (char=? (string-ref str i) #\tab))
          (loop (+ i 1))
          (substring str i (string-length str)))))
  (define (all-digits? str)
    (and (> (string-length str) 0)
         (let loop ([i 0])
           (or (>= i (string-length str))
               (and (char-numeric? (string-ref str i)) (loop (+ i 1)))))))
  (define (assignment-word? word)
    (let ([eq-pos (string-find-char word #\=)])
      (and eq-pos
           (> eq-pos 0)
           (let ([name-part (if (and (> eq-pos 1)
                                     (char=?
                                       (string-ref word (- eq-pos 1))
                                       #\+))
                                (substring word 0 (- eq-pos 1))
                                (substring word 0 eq-pos))])
             (let ([bracket-pos (string-find-char-in-name
                                  name-part
                                  #\[)])
               (if bracket-pos
                   (and (> bracket-pos 0)
                        (valid-shell-name?
                          (substring name-part 0 bracket-pos))
                        (let ([close (string-find-char-in-name
                                       name-part
                                       #\])])
                          (and close
                               (= close (- (string-length name-part) 1)))))
                   (valid-shell-name? name-part)))))))
  (define (string-find-char-in-name str ch)
    (let loop ([i 0])
      (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))])))
  (define (valid-shell-name? str)
    (and (> (string-length str) 0)
         (let ([ch (string-ref str 0)])
           (or (char-alphabetic? ch) (char=? ch #\_)))
         (let loop ([i 1])
           (or (>= i (string-length str))
               (let ([ch (string-ref str i)])
                 (and (or (char-alphabetic? ch)
                          (char-numeric? ch)
                          (char=? ch #\_))
                      (loop (+ i 1))))))))
  (define (string-find-char str ch)
    (let loop ([i 0])
      (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (+ i 1))])))
  (define (read-hex-digits lex max-count)
    (let loop ([count 0] [val 0])
      (if (or (>= count max-count)
              (at-end? lex)
              (not (hex-char? (current-char lex))))
          (values val count)
          (let ([digit (hex-val (current-char lex))])
            (advance! lex)
            (loop (+ count 1) (+ (* val 16) digit))))))
  (define (read-octal-digits lex max-count)
    (read-octal-digits-with-init lex max-count 0))
  (define (read-octal-digits-with-init lex max-count init)
    (let loop ([count 0] [val init])
      (if (or (>= count max-count)
              (at-end? lex)
              (not (octal-char? (current-char lex))))
          val
          (let ([digit (- (char->integer (current-char lex))
                          (char->integer #\0))])
            (advance! lex)
            (loop (+ count 1) (+ (* val 8) digit))))))
  (define (hex-val ch)
    (cond
      [(and (char>=? ch #\0) (char<=? ch #\9))
       (- (char->integer ch) (char->integer #\0))]
      [(and (char>=? ch #\a) (char<=? ch #\f))
       (+ 10 (- (char->integer ch) (char->integer #\a)))]
      [(and (char>=? ch #\A) (char<=? ch #\F))
       (+ 10 (- (char->integer ch) (char->integer #\A)))]
      [else 0]))
  (define (octal-char? ch)
    (and (char>=? ch #\0) (char<=? ch #\7)))
  (define *reserved-words*
    '("if" "then" "elif" "else" "fi" "do" "done" "case" "esac"
      "while" "until" "for" "in" "select" "function" "time"
      "coproc" "{" "}" "!" "[[" "]]"))
  (define (reserved-word? word)
    (member word *reserved-words*))
  (set! lexer-read-token! lexer-read-token-impl!))
