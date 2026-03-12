#!chezscheme
(library (jsh parser)
  (export parser-state::t make-parser-state parser-state?
   parser-state-lexer parser-state-peeked
   parser-state-needs-more? parser-state-heredoc-queue
   parser-state-alias-fn parser-state-lexer-set!
   parser-state-peeked-set! parser-state-needs-more?-set!
   parser-state-heredoc-queue-set! parser-state-alias-fn-set!
   &parser-state-lexer &parser-state-peeked
   &parser-state-needs-more? &parser-state-heredoc-queue
   &parser-state-alias-fn &parser-state-lexer-set!
   &parser-state-peeked-set! &parser-state-needs-more?-set!
   &parser-state-heredoc-queue-set! &parser-state-alias-fn-set!
   parse-complete-command parse-one-line parser-needs-more?
   parser-peek parser-next! parser-consume! parser-check?
   parser-check-word? parser-expect-word! skip-newlines!
   consume-heredoc-bodies! at-end-or-sep? parse-list
   parse-and-or parse-pipeline parse-command
   alias-value-continues? parse-simple-command
   parse-assignment-token string-find-bracket
   string-find-close-bracket string-find-eq
   parse-compound-array-values heredoc-delimiter-quoted?
   strip-heredoc-quotes redirect-token? parse-redirect!
   parse-redirect-list parse-coproc coproc-compound-start?
   parse-brace-group parse-subshell parse-if parse-while
   parse-until parse-for parse-for-in parse-arith-command
   parse-arith-for lexer-raw-char lexer-raw-advance!
   parser-clear-peeked! read-arith-body read-arith-until-semi
   read-arith-until-close string-trim-ws parse-case
   parse-select parse-function-def-keyword parse-cond-command
   parse-cond-or parse-cond-and parse-cond-not cond-unary-op?
   cond-binary-op? cond-peek-binary-op parse-cond-primary
   cond-collect-rhs)
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
    (std sugar) (gsh ast) (gsh lexer))
  (begin
    (define parser-state::t
      (make-class-type 'gerbil\x23;parser-state::t 'parser-state (list object::t)
        '(lexer peeked needs-more? heredoc-queue alias-fn)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-parser-state . args)
      (let* ([type parser-state::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (parser-state? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;parser-state::t))
    (define (parser-state-lexer obj)
      (unchecked-slot-ref obj 'lexer))
    (define (parser-state-peeked obj)
      (unchecked-slot-ref obj 'peeked))
    (define (parser-state-needs-more? obj)
      (unchecked-slot-ref obj 'needs-more?))
    (define (parser-state-heredoc-queue obj)
      (unchecked-slot-ref obj 'heredoc-queue))
    (define (parser-state-alias-fn obj)
      (unchecked-slot-ref obj 'alias-fn))
    (define (parser-state-lexer-set! obj val)
      (unchecked-slot-set! obj 'lexer val))
    (define (parser-state-peeked-set! obj val)
      (unchecked-slot-set! obj 'peeked val))
    (define (parser-state-needs-more?-set! obj val)
      (unchecked-slot-set! obj 'needs-more? val))
    (define (parser-state-heredoc-queue-set! obj val)
      (unchecked-slot-set! obj 'heredoc-queue val))
    (define (parser-state-alias-fn-set! obj val)
      (unchecked-slot-set! obj 'alias-fn val))
    (define (&parser-state-lexer obj)
      (unchecked-slot-ref obj 'lexer))
    (define (&parser-state-peeked obj)
      (unchecked-slot-ref obj 'peeked))
    (define (&parser-state-needs-more? obj)
      (unchecked-slot-ref obj 'needs-more?))
    (define (&parser-state-heredoc-queue obj)
      (unchecked-slot-ref obj 'heredoc-queue))
    (define (&parser-state-alias-fn obj)
      (unchecked-slot-ref obj 'alias-fn))
    (define (&parser-state-lexer-set! obj val)
      (unchecked-slot-set! obj 'lexer val))
    (define (&parser-state-peeked-set! obj val)
      (unchecked-slot-set! obj 'peeked val))
    (define (&parser-state-needs-more?-set! obj val)
      (unchecked-slot-set! obj 'needs-more? val))
    (define (&parser-state-heredoc-queue-set! obj val)
      (unchecked-slot-set! obj 'heredoc-queue val))
    (define (&parser-state-alias-fn-set! obj val)
      (unchecked-slot-set! obj 'alias-fn val)))
  (define parse-complete-command
    (case-lambda
      [(input)
       (let* ([extglob? #f] [alias-fn #f])
         (let* ([lex (if (string? input)
                         (make-shell-lexer input extglob?)
                         input)])
           (let* ([ps (make-parser-state lex #f #f (list) alias-fn)])
             (let ([result (parse-list ps)])
               (when (lexer-needs-more? lex)
                 (parser-state-needs-more?-set! ps #t))
               (let ([tok (parser-peek ps)])
                 (when (and (token? tok)
                            (not (eq? (token-type tok) 'NEWLINE)))
                   (error 'gerbil
                     (string-append
                       "parse error near `"
                       (if (token-value tok)
                           (token-value tok)
                           (symbol->string (token-type tok)))
                       "'"))))
               result))))]
      [(input extglob?)
       (let* ([alias-fn #f])
         (let* ([lex (if (string? input)
                         (make-shell-lexer input extglob?)
                         input)])
           (let* ([ps (make-parser-state lex #f #f (list) alias-fn)])
             (let ([result (parse-list ps)])
               (when (lexer-needs-more? lex)
                 (parser-state-needs-more?-set! ps #t))
               (let ([tok (parser-peek ps)])
                 (when (and (token? tok)
                            (not (eq? (token-type tok) 'NEWLINE)))
                   (error 'gerbil
                     (string-append
                       "parse error near `"
                       (if (token-value tok)
                           (token-value tok)
                           (symbol->string (token-type tok)))
                       "'"))))
               result))))]
      [(input extglob? alias-fn)
       (let* ([lex (if (string? input)
                       (make-shell-lexer input extglob?)
                       input)])
         (let* ([ps (make-parser-state lex #f #f (list) alias-fn)])
           (let ([result (parse-list ps)])
             (when (lexer-needs-more? lex)
               (parser-state-needs-more?-set! ps #t))
             (let ([tok (parser-peek ps)])
               (when (and (token? tok)
                          (not (eq? (token-type tok) 'NEWLINE)))
                 (error 'gerbil
                   (string-append
                     "parse error near `"
                     (if (token-value tok)
                         (token-value tok)
                         (symbol->string (token-type tok)))
                     "'"))))
             result)))]))
  (define parse-one-line
    (case-lambda
      [(input)
       (let* ([extglob? #f] [alias-fn #f])
         (let* ([lex (if (string? input)
                         (make-shell-lexer input extglob?)
                         input)])
           (let* ([ps (make-parser-state lex #f #f (list) alias-fn)])
             (skip-newlines! ps)
             (let ([first (parse-and-or ps)])
               (if (not first)
                   (begin
                     (when (lexer-needs-more? lex)
                       (parser-state-needs-more?-set! ps #t))
                     (let ([tok (parser-peek ps)])
                       (when (and (token? tok)
                                  (not (eq? (token-type tok) 'NEWLINE)))
                         (error 'gerbil
                           (string-append
                             "parse error near `"
                             (if (token-value tok)
                                 (token-value tok)
                                 (symbol->string (token-type tok)))
                             "'"))))
                     #f)
                   (let loop ([items (list (cons 'sequential first))])
                     (cond
                       [(parser-check? ps 'SEMI)
                        (parser-consume! ps)
                        (let ([next (parse-and-or ps)])
                          (if next
                              (loop (cons (cons 'sequential next) items))
                              (begin
                                (let ([tok (parser-peek ps)])
                                  (when (and (token? tok)
                                             (not (eq? (token-type tok)
                                                       'NEWLINE)))
                                    (error 'gerbil
                                      (string-append
                                        "syntax error near unexpected token `"
                                        (if (token-value tok)
                                            (token-value tok)
                                            (symbol->string
                                              (token-type tok)))
                                        "'"))))
                                (if (= (length items) 1)
                                    (cdar items)
                                    (make-command-list
                                      (reverse items))))))]
                       [(parser-check? ps 'AMP)
                        (parser-consume! ps)
                        (when (pair? (parser-state-heredoc-queue ps))
                          (consume-heredoc-bodies! ps))
                        (let ([updated (cons
                                         (cons 'background (cdar items))
                                         (cdr items))])
                          (make-command-list (reverse updated)))]
                       [(parser-check? ps 'NEWLINE)
                        (parser-consume! ps)
                        (when (pair? (parser-state-heredoc-queue ps))
                          (consume-heredoc-bodies! ps))
                        (if (= (length items) 1)
                            (cdar items)
                            (make-command-list (reverse items)))]
                       [else
                        (when (pair? (parser-state-heredoc-queue ps))
                          (consume-heredoc-bodies! ps))
                        (when (lexer-needs-more? lex)
                          (parser-state-needs-more?-set! ps #t))
                        (if (= (length items) 1)
                            (cdar items)
                            (make-command-list (reverse items)))])))))))]
      [(input extglob?)
       (let* ([alias-fn #f])
         (let* ([lex (if (string? input)
                         (make-shell-lexer input extglob?)
                         input)])
           (let* ([ps (make-parser-state lex #f #f (list) alias-fn)])
             (skip-newlines! ps)
             (let ([first (parse-and-or ps)])
               (if (not first)
                   (begin
                     (when (lexer-needs-more? lex)
                       (parser-state-needs-more?-set! ps #t))
                     (let ([tok (parser-peek ps)])
                       (when (and (token? tok)
                                  (not (eq? (token-type tok) 'NEWLINE)))
                         (error 'gerbil
                           (string-append
                             "parse error near `"
                             (if (token-value tok)
                                 (token-value tok)
                                 (symbol->string (token-type tok)))
                             "'"))))
                     #f)
                   (let loop ([items (list (cons 'sequential first))])
                     (cond
                       [(parser-check? ps 'SEMI)
                        (parser-consume! ps)
                        (let ([next (parse-and-or ps)])
                          (if next
                              (loop (cons (cons 'sequential next) items))
                              (begin
                                (let ([tok (parser-peek ps)])
                                  (when (and (token? tok)
                                             (not (eq? (token-type tok)
                                                       'NEWLINE)))
                                    (error 'gerbil
                                      (string-append
                                        "syntax error near unexpected token `"
                                        (if (token-value tok)
                                            (token-value tok)
                                            (symbol->string
                                              (token-type tok)))
                                        "'"))))
                                (if (= (length items) 1)
                                    (cdar items)
                                    (make-command-list
                                      (reverse items))))))]
                       [(parser-check? ps 'AMP)
                        (parser-consume! ps)
                        (when (pair? (parser-state-heredoc-queue ps))
                          (consume-heredoc-bodies! ps))
                        (let ([updated (cons
                                         (cons 'background (cdar items))
                                         (cdr items))])
                          (make-command-list (reverse updated)))]
                       [(parser-check? ps 'NEWLINE)
                        (parser-consume! ps)
                        (when (pair? (parser-state-heredoc-queue ps))
                          (consume-heredoc-bodies! ps))
                        (if (= (length items) 1)
                            (cdar items)
                            (make-command-list (reverse items)))]
                       [else
                        (when (pair? (parser-state-heredoc-queue ps))
                          (consume-heredoc-bodies! ps))
                        (when (lexer-needs-more? lex)
                          (parser-state-needs-more?-set! ps #t))
                        (if (= (length items) 1)
                            (cdar items)
                            (make-command-list (reverse items)))])))))))]
      [(input extglob? alias-fn)
       (let* ([lex (if (string? input)
                       (make-shell-lexer input extglob?)
                       input)])
         (let* ([ps (make-parser-state lex #f #f (list) alias-fn)])
           (skip-newlines! ps)
           (let ([first (parse-and-or ps)])
             (if (not first)
                 (begin
                   (when (lexer-needs-more? lex)
                     (parser-state-needs-more?-set! ps #t))
                   (let ([tok (parser-peek ps)])
                     (when (and (token? tok)
                                (not (eq? (token-type tok) 'NEWLINE)))
                       (error 'gerbil
                         (string-append
                           "parse error near `"
                           (if (token-value tok)
                               (token-value tok)
                               (symbol->string (token-type tok)))
                           "'"))))
                   #f)
                 (let loop ([items (list (cons 'sequential first))])
                   (cond
                     [(parser-check? ps 'SEMI)
                      (parser-consume! ps)
                      (let ([next (parse-and-or ps)])
                        (if next
                            (loop (cons (cons 'sequential next) items))
                            (begin
                              (let ([tok (parser-peek ps)])
                                (when (and (token? tok)
                                           (not (eq? (token-type tok)
                                                     'NEWLINE)))
                                  (error 'gerbil
                                    (string-append
                                      "syntax error near unexpected token `"
                                      (if (token-value tok)
                                          (token-value tok)
                                          (symbol->string
                                            (token-type tok)))
                                      "'"))))
                              (if (= (length items) 1)
                                  (cdar items)
                                  (make-command-list (reverse items))))))]
                     [(parser-check? ps 'AMP)
                      (parser-consume! ps)
                      (when (pair? (parser-state-heredoc-queue ps))
                        (consume-heredoc-bodies! ps))
                      (let ([updated (cons
                                       (cons 'background (cdar items))
                                       (cdr items))])
                        (make-command-list (reverse updated)))]
                     [(parser-check? ps 'NEWLINE)
                      (parser-consume! ps)
                      (when (pair? (parser-state-heredoc-queue ps))
                        (consume-heredoc-bodies! ps))
                      (if (= (length items) 1)
                          (cdar items)
                          (make-command-list (reverse items)))]
                     [else
                      (when (pair? (parser-state-heredoc-queue ps))
                        (consume-heredoc-bodies! ps))
                      (when (lexer-needs-more? lex)
                        (parser-state-needs-more?-set! ps #t))
                      (if (= (length items) 1)
                          (cdar items)
                          (make-command-list (reverse items)))]))))))]))
  (define (parser-needs-more? ps)
    (parser-state-needs-more? ps))
  (define (parser-peek ps)
    (unless (parser-state-peeked ps)
      (parser-state-peeked-set!
        ps
        (lexer-next! (parser-state-lexer ps))))
    (parser-state-peeked ps))
  (define (parser-next! ps)
    (if (parser-state-peeked ps)
        (let ([tok (parser-state-peeked ps)])
          (parser-state-peeked-set! ps #f)
          tok)
        (lexer-next! (parser-state-lexer ps))))
  (define (parser-consume! ps) (parser-next! ps))
  (define (parser-check? ps type)
    (let ([tok (parser-peek ps)])
      (and (token? tok) (eq? (token-type tok) type))))
  (define (parser-check-word? ps word)
    (let ([tok (parser-peek ps)])
      (and (token? tok)
           (eq? (token-type tok) 'WORD)
           (string=? (token-value tok) word))))
  (define (parser-expect-word! ps word)
    (let ([tok (parser-next! ps)])
      (unless (and (token? tok)
                   (eq? (token-type tok) 'WORD)
                   (string=? (token-value tok) word))
        (error 'gerbil
          (format
            "parse error: expected '~a', got ~a"
            word
            (if (token? tok) (token-value tok) tok))))))
  (define (skip-newlines! ps)
    (let loop ()
      (cond
        [(parser-check? ps 'NEWLINE) (parser-consume! ps) (loop)]
        [(and (pair? (parser-state-heredoc-queue ps))
              (parser-check? ps 'HEREDOC_BODY))
         (consume-heredoc-bodies! ps)
         (loop)]
        [else (void)])))
  (define (consume-heredoc-bodies! ps)
    (let loop ([queue (parser-state-heredoc-queue ps)])
      (when (pair? queue)
        (let ([redir (car queue)])
          (when (parser-check? ps 'HEREDOC_BODY)
            (let ([tok (parser-next! ps)])
              (redir-target-set! redir (token-value tok)))))
        (loop (cdr queue))))
    (parser-state-heredoc-queue-set! ps (list)))
  (define (at-end-or-sep? ps)
    (let ([tok (parser-peek ps)])
      (or (eq? tok 'eof) (not (token? tok)))))
  (define (parse-list ps)
    (skip-newlines! ps)
    (let ([first (parse-and-or ps)])
      (if (not first)
          #f
          (let loop ([items (list (cons 'sequential first))])
            (let ([tok (parser-peek ps)])
              (cond
                [(parser-check? ps 'SEMI)
                 (parser-consume! ps)
                 (skip-newlines! ps)
                 (let ([next (parse-and-or ps)])
                   (if next
                       (loop (cons (cons 'sequential next) items))
                       (make-command-list (reverse items))))]
                [(parser-check? ps 'AMP)
                 (parser-consume! ps)
                 (let ([updated (cons
                                  (cons 'background (cdar items))
                                  (cdr items))])
                   (skip-newlines! ps)
                   (let ([next (parse-and-or ps)])
                     (if next
                         (loop (cons (cons 'sequential next) updated))
                         (make-command-list (reverse updated)))))]
                [(parser-check? ps 'NEWLINE)
                 (parser-consume! ps)
                 (skip-newlines! ps)
                 (let ([next (parse-and-or ps)])
                   (if next
                       (loop (cons (cons 'sequential next) items))
                       (make-command-list (reverse items))))]
                [else
                 (if (= (length items) 1)
                     (cdar items)
                     (make-command-list (reverse items)))]))))))
  (define (parse-and-or ps)
    (let ([first (parse-pipeline ps)])
      (if (not first)
          #f
          (let loop ([rest (list)])
            (cond
              [(parser-check? ps 'AND_IF)
               (parser-consume! ps)
               (skip-newlines! ps)
               (let ([next (parse-pipeline ps)])
                 (if next
                     (loop (cons (cons 'and next) rest))
                     (error 'gerbil
                       "parse error: expected command after &&")))]
              [(parser-check? ps 'OR_IF)
               (parser-consume! ps)
               (skip-newlines! ps)
               (let ([next (parse-pipeline ps)])
                 (if next
                     (loop (cons (cons 'or next) rest))
                     (error 'gerbil
                       "parse error: expected command after ||")))]
              [else
               (if (null? rest)
                   first
                   (make-and-or-list first (reverse rest)))])))))
  (define (parse-pipeline ps)
    (let* ([time? (and (parser-check-word? ps "time")
                       (begin (parser-consume! ps) #t))])
      (let* ([posix-time? (and time?
                               (parser-check-word? ps "-p")
                               (begin (parser-consume! ps) #t))])
        (let* ([bang? (and (or (parser-check-word? ps "!")
                               (parser-check? ps 'BANG))
                           (begin (parser-consume! ps) #t))])
          (let* ([first (parse-command ps)])
            (if (not first)
                (cond
                  [bang?
                   (error 'gerbil "parse error: expected command after !")]
                  [time? (make-time-command posix-time? #f)]
                  [else #f])
                (let loop ([cmds (list first)] [ptypes (list)])
                  (cond
                    [(or (parser-check? ps 'PIPE)
                         (parser-check? ps 'PIPEAMP))
                     (let ([pipe-type (token-type (parser-next! ps))])
                       (skip-newlines! ps)
                       (let ([next (parse-command ps)])
                         (if next
                             (loop
                               (cons next cmds)
                               (cons pipe-type ptypes))
                             (error 'gerbil
                               "parse error: expected command after |"))))]
                    [else
                     (let* ([commands (reverse cmds)])
                       (let* ([pipe-types (reverse ptypes)])
                         (let* ([pipeline (if (and (= (length commands) 1)
                                                   (not bang?))
                                              (car commands)
                                              (make-ast-pipeline
                                                commands
                                                bang?
                                                pipe-types))])
                           (if time?
                               (make-time-command posix-time? pipeline)
                               pipeline))))]))))))))
  (define (parse-command ps)
    (let command-loop ()
      (let ([tok (parser-peek ps)])
        (cond
          [(not (token? tok)) #f]
          [(eq? tok 'eof) #f]
          [(parser-check-word? ps "{") (parse-brace-group ps)]
          [(parser-check? ps 'LPAREN)
           (parser-consume! ps)
           (if (parser-check? ps 'LPAREN)
               (let ([cmd (parse-arith-command ps)]
                     [redirs (parse-redirect-list ps)])
                 (if (pair? redirs)
                     (make-redirected-command cmd redirs)
                     cmd))
               (let ([body (parse-list ps)])
                 (skip-newlines! ps)
                 (unless (parser-check? ps 'RPAREN)
                   (error 'gerbil "parse error: expected ')'"))
                 (parser-consume! ps)
                 (let ([redirs (parse-redirect-list ps)])
                   (if (pair? redirs)
                       (make-redirected-command
                         (make-subshell body)
                         redirs)
                       (make-subshell body)))))]
          [(parser-check-word? ps "if") (parse-if ps)]
          [(parser-check-word? ps "while") (parse-while ps)]
          [(parser-check-word? ps "until") (parse-until ps)]
          [(parser-check-word? ps "for") (parse-for ps)]
          [(parser-check-word? ps "case") (parse-case ps)]
          [(parser-check-word? ps "select") (parse-select ps)]
          [(parser-check-word? ps "[[")
           (let ([cmd (parse-cond-command ps)]
                 [redirs (parse-redirect-list ps)])
             (if (pair? redirs)
                 (make-redirected-command cmd redirs)
                 cmd))]
          [(parser-check-word? ps "function")
           (parse-function-def-keyword ps)]
          [(parser-check-word? ps "coproc") (parse-coproc ps)]
          [else
           (let ([result (parse-simple-command ps)])
             (if (eq? result 'alias-restart) (command-loop) result))]))))
  (define (alias-value-continues? value)
    (and (string? value)
         (> (string-length value) 0)
         (char=?
           (string-ref value (- (string-length value) 1))
           #\space)))
  (define (parse-simple-command ps)
    (call/cc
      (lambda (return)
        (let ([assignments (list)]
              [words (list)]
              [redirections (list)]
              [alias-fn (parser-state-alias-fn ps)]
              [expanded-aliases (make-hash-table)]
              [check-next-for-alias #f]
              [alias-text-end #f])
          (let prefix-loop ()
            (let ([tok (parser-peek ps)])
              (cond
                [(parser-check? ps 'ASSIGNMENT_WORD)
                 (let* ([tok (parser-next! ps)])
                   (let* ([asgn (parse-assignment-token tok)])
                     (if (and (string=? (assignment-value asgn) "")
                              (not (assignment-index asgn))
                              (parser-check? ps 'LPAREN))
                         (begin
                           (parser-consume! ps)
                           (let ([values (parse-compound-array-values ps)])
                             (set! assignments
                               (cons
                                 (make-assignment
                                   (assignment-name asgn)
                                   #f
                                   values
                                   (assignment-op asgn))
                                 assignments))
                             (prefix-loop)))
                         (begin
                           (set! assignments (cons asgn assignments))
                           (prefix-loop)))))]
                [(redirect-token? tok)
                 (set! redirections
                   (cons (parse-redirect! ps) redirections))
                 (prefix-loop)]
                [else (%%void)])))
          (let ([tok (parser-peek ps)])
            (when (and (token? tok)
                       (or (eq? (token-type tok) 'WORD)
                           (eq? (token-type tok) 'IO_NUMBER)))
              (when (and (eq? (token-type tok) 'WORD)
                         (or (pair? assignments)
                             (not (reserved-word? (token-value tok)))))
                (let ([word-tok (parser-next! ps)])
                  (let alias-check ([wtok word-tok])
                    (let* ([word-val (token-value wtok)])
                      (let* ([alias-val (and alias-fn
                                             (not (hash-get
                                                    expanded-aliases
                                                    word-val))
                                             (alias-fn word-val))])
                        (if alias-val
                            (begin
                              (hash-put! expanded-aliases word-val #t)
                              (let ([lex (parser-state-lexer ps)])
                                (lexer-prepend-text! lex alias-val)
                                (set! alias-text-end
                                  (string-length alias-val))
                                (set! check-next-for-alias
                                  (alias-value-continues? alias-val))
                                (parser-state-peeked-set! ps #f)
                                (let ([new-tok (parser-peek ps)])
                                  (cond
                                    [(and (token? new-tok)
                                          (eq? (token-type new-tok) 'WORD)
                                          (null? assignments)
                                          (null? redirections)
                                          (reserved-word?
                                            (token-value new-tok)))
                                     (return 'alias-restart)]
                                    [(and (token? new-tok)
                                          (eq? (token-type new-tok)
                                               'LPAREN)
                                          (null? assignments)
                                          (null? redirections))
                                     (return 'alias-restart)]
                                    [(and (token? new-tok)
                                          (eq? (token-type new-tok) 'WORD))
                                     (alias-check (parser-next! ps))]
                                    [(and (token? new-tok)
                                          (not (eq? (token-type new-tok)
                                                    'WORD))
                                          (null? assignments)
                                          (null? redirections))
                                     (return 'alias-restart)]
                                    [else
                                     (when (and (token? new-tok)
                                                (eq? (token-type new-tok)
                                                     'WORD))
                                       (set! words
                                         (cons
                                           (token-value (parser-next! ps))
                                           words)))]))))
                            (set! words
                              (cons (token-value wtok) words))))))
                  (when (and (pair? words) (parser-check? ps 'LPAREN))
                    (parser-consume! ps)
                    (if (parser-check? ps 'RPAREN)
                        (begin
                          (parser-consume! ps)
                          (let ([func-line (lexer-line
                                             (parser-state-lexer ps))])
                            (skip-newlines! ps)
                            (let ([body (parse-command ps)]
                                  [redirs (parse-redirect-list ps)])
                              (return
                                (make-function-def
                                  (car words)
                                  body
                                  redirs
                                  func-line)))))
                        (error 'gerbil
                          (string-append
                            "parse error near unexpected token `('"))))
                  (let suffix-loop ()
                    (let ([tok (parser-peek ps)])
                      (cond
                        [(and check-next-for-alias
                              alias-fn
                              (token? tok)
                              (eq? (token-type tok) 'WORD)
                              (number? alias-text-end)
                              (>= (lexer-pos (parser-state-lexer ps))
                                  alias-text-end))
                         (set! check-next-for-alias #f)
                         (let expand-next-alias ([next-seen (make-hash-table)])
                           (let* ([tok2 (parser-peek ps)])
                             (if (not (and (token? tok2)
                                           (eq? (token-type tok2) 'WORD)))
                                 (suffix-loop)
                                 (let* ([next-tok (parser-next! ps)])
                                   (let* ([next-val (token-value
                                                      next-tok)])
                                     (let* ([next-alias (and (not (hash-get
                                                                    next-seen
                                                                    next-val))
                                                             (alias-fn
                                                               next-val))])
                                       (if next-alias
                                           (begin
                                             (hash-put!
                                               next-seen
                                               next-val
                                               #t)
                                             (let ([lex (parser-state-lexer
                                                          ps)])
                                               (lexer-prepend-text!
                                                 lex
                                                 next-alias)
                                               (when (alias-value-continues?
                                                       next-alias)
                                                 (set! check-next-for-alias
                                                   #t)
                                                 (set! alias-text-end
                                                   (string-length
                                                     next-alias)))
                                               (parser-state-peeked-set!
                                                 ps
                                                 #f)
                                               (expand-next-alias
                                                 next-seen)))
                                           (begin
                                             (set! words
                                               (cons next-val words))
                                             (suffix-loop)))))))))]
                        [(and (token? tok)
                              (eq? (token-type tok) 'ASSIGNMENT_WORD))
                         (let* ([t (parser-next! ps)])
                           (let* ([asgn (parse-assignment-token t)])
                             (if (and (string=? (assignment-value asgn) "")
                                      (not (assignment-index asgn))
                                      (parser-check? ps 'LPAREN))
                                 (begin
                                   (parser-consume! ps)
                                   (let ([values (parse-compound-array-values
                                                   ps)])
                                     (let* ([val-strs (map (lambda (v)
                                                             (if (string?
                                                                   v)
                                                                 v
                                                                 ""))
                                                           values)])
                                       (let* ([compound-str (string-append
                                                              (assignment-name
                                                                asgn)
                                                              "=("
                                                              (let loop ([vs val-strs]
                                                                         [acc ""])
                                                                (if (null?
                                                                      vs)
                                                                    acc
                                                                    (loop
                                                                      (cdr vs)
                                                                      (if (string=?
                                                                            acc
                                                                            "")
                                                                          (car vs)
                                                                          (string-append
                                                                            acc
                                                                            " "
                                                                            (car vs))))))
                                                              ")")])
                                         (set! words
                                           (cons compound-str words))
                                         (suffix-loop)))))
                                 (begin
                                   (set! words
                                     (cons (token-value t) words))
                                   (suffix-loop)))))]
                        [(and (token? tok)
                              (or (eq? (token-type tok) 'WORD)
                                  (eq? (token-type tok) 'BANG)))
                         (set! words
                           (cons (token-value (parser-next! ps)) words))
                         (suffix-loop)]
                        [(and (token? tok)
                              (eq? (token-type tok) 'PROCSUB_IN))
                         (let ([t (parser-next! ps)])
                           (set! words
                             (cons
                               (make-word-process-sub 'in (token-value t))
                               words)))
                         (suffix-loop)]
                        [(and (token? tok)
                              (eq? (token-type tok) 'PROCSUB_OUT))
                         (let ([t (parser-next! ps)])
                           (set! words
                             (cons
                               (make-word-process-sub 'out (token-value t))
                               words)))
                         (suffix-loop)]
                        [(redirect-token? tok)
                         (set! redirections
                           (cons (parse-redirect! ps) redirections))
                         (suffix-loop)]
                        [else (%%void)])))))))
          (if (and (null? words)
                   (null? assignments)
                   (null? redirections))
              #f
              (make-simple-command
                (reverse assignments)
                (reverse words)
                (reverse redirections)))))))
  (define (parse-assignment-token tok)
    (let* ([word (token-value tok)])
      (let* ([eq-pos (string-find-eq word)])
        (let* ([append? (and eq-pos
                             (> eq-pos 0)
                             (char=? (string-ref word (- eq-pos 1)) #\+))])
          (let* ([name-end (if append? (- eq-pos 1) eq-pos)])
            (let* ([name-part (substring word 0 name-end)])
              (let* ([value (substring
                              word
                              (+ eq-pos 1)
                              (string-length word))])
                (let* ([op (if append? '\x2B;= '=)])
                  (let ([bracket-pos (string-find-bracket name-part)])
                    (if bracket-pos
                        (let* ([name (substring name-part 0 bracket-pos)])
                          (let* ([close (string-find-close-bracket
                                          name-part
                                          (+ bracket-pos 1))])
                            (let* ([index (if close
                                              (substring
                                                name-part
                                                (+ bracket-pos 1)
                                                close)
                                              "")])
                              (make-assignment name index value op))))
                        (make-assignment name-part #f value op)))))))))))
  (define (string-find-bracket str)
    (let loop ([i 0])
      (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) #\[) i]
        [else (loop (+ i 1))])))
  (define (string-find-close-bracket str start)
    (let loop ([i start])
      (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) #\]) i]
        [else (loop (+ i 1))])))
  (define (string-find-eq str)
    (let loop ([i 0])
      (cond
        [(>= i (string-length str)) #f]
        [(char=? (string-ref str i) #\=) i]
        [else (loop (+ i 1))])))
  (define (parse-compound-array-values ps)
    (skip-newlines! ps)
    (let loop ([values (list)])
      (cond
        [(parser-check? ps 'RPAREN)
         (parser-consume! ps)
         (reverse values)]
        [(parser-check? ps 'NEWLINE)
         (parser-consume! ps)
         (skip-newlines! ps)
         (loop values)]
        [(or (parser-check? ps 'WORD)
             (parser-check? ps 'ASSIGNMENT_WORD))
         (let ([tok (parser-next! ps)])
           (loop (cons (token-value tok) values)))]
        [else (reverse values)])))
  (define (heredoc-delimiter-quoted? delim)
    (let ([len (string-length delim)])
      (let loop ([i 0])
        (if (>= i len)
            #f
            (let ([ch (string-ref delim i)])
              (if (or (char=? ch #\') (char=? ch #\"))
                  #t
                  (loop (+ i 1))))))))
  (define (strip-heredoc-quotes delim)
    (let ([out (open-output-string)]
          [len (string-length delim)])
      (let loop ([i 0])
        (if (>= i len)
            (get-output-string out)
            (let ([ch (string-ref delim i)])
              (if (or (char=? ch #\') (char=? ch #\"))
                  (loop (+ i 1))
                  (begin (display ch out) (loop (+ i 1)))))))))
  (define (redirect-token? tok)
    (and (token? tok)
         (memq
           (token-type tok)
           '(LESS GREAT DGREAT DLESS DLESSDASH TLESS LESSAND GREATAND
              LESSGREAT CLOBBER AMPGREAT AMPGREAT_GREAT IO_NUMBER
              IO_VARNAME))))
  (define (parse-redirect! ps)
    (let ([fd #f] [fd-var #f])
      (cond
        [(parser-check? ps 'IO_NUMBER)
         (set! fd (string->number (token-value (parser-next! ps))))]
        [(parser-check? ps 'IO_VARNAME)
         (set! fd-var (token-value (parser-next! ps)))])
      (let ([op-tok (parser-next! ps)])
        (let ([op-type (token-type op-tok)])
          (let* ([target-tok (parser-peek ps)])
            (let* ([_ (when (or (not target-tok)
                                (not (token? target-tok))
                                (memq
                                  (token-type target-tok)
                                  '(NEWLINE SEMI EOF)))
                        (error 'gerbil
                          "parse error: redirect target missing"))])
              (let* ([target-tok (parser-next! ps)])
                (let* ([target (token-value target-tok)])
                  (let* ([r (make-redir
                              (case op-type
                                [(LESS) '<]
                                [(GREAT) '>]
                                [(DGREAT) '>>]
                                [(CLOBBER) 'clobber]
                                [(LESSAND) '<&]
                                [(GREATAND) '>&]
                                [(LESSGREAT) '<>]
                                [(DLESS) '<<]
                                [(DLESSDASH) '<<-]
                                [(TLESS) '<<<]
                                [(AMPGREAT) '&>]
                                [(AMPGREAT_GREAT) '&>>]
                                [else '>])
                              fd
                              target
                              fd-var)])
                    (when (memq op-type '(DLESS DLESSDASH))
                      (let* ([lex (parser-state-lexer ps)])
                        (let* ([strip-tabs? (eq? op-type 'DLESSDASH)])
                          (let* ([quoted? (heredoc-delimiter-quoted?
                                            target)])
                            (let* ([bare-delim (if quoted?
                                                   (strip-heredoc-quotes
                                                     target)
                                                   target)])
                              (lexer-heredocs-set!
                                lex
                                (cons
                                  (list bare-delim strip-tabs? quoted?)
                                  (lexer-heredocs lex)))
                              (when quoted?
                                (redir-op-set!
                                  r
                                  (if strip-tabs? '<<-q '<<q)))
                              (parser-state-heredoc-queue-set!
                                ps
                                (append
                                  (parser-state-heredoc-queue ps)
                                  (list r))))))))
                    r)))))))))
  (define (parse-redirect-list ps)
    (let loop ([redirs (list)])
      (if (redirect-token? (parser-peek ps))
          (loop (cons (parse-redirect! ps) redirs))
          (reverse redirs))))
  (define (parse-coproc ps)
    (parser-expect-word! ps "coproc")
    (skip-newlines! ps)
    (let ([first (parse-command ps)])
      (if (and (simple-command? first)
               (let ([words (simple-command-words first)])
                 (and (pair? words)
                      (null? (cdr words))
                      (string? (car words))))
               (null? (simple-command-assignments first))
               (null? (simple-command-redirections first))
               (coproc-compound-start? ps))
          (let ([name (car (simple-command-words first))]
                [body (parse-command ps)])
            (make-coproc-command name body))
          (make-coproc-command "COPROC" first))))
  (define (coproc-compound-start? ps)
    (let ([tok (parser-peek ps)])
      (and (token? tok)
           (or (parser-check? ps 'LPAREN)
               (and (eq? (token-type tok) 'WORD)
                    (member
                      (token-value tok)
                      '("{" "while" "until" "if" "for")))))))
  (define (parse-brace-group ps)
    (parser-expect-word! ps "{")
    (let ([body (parse-list ps)])
      (skip-newlines! ps)
      (parser-expect-word! ps "}")
      (let ([redirs (parse-redirect-list ps)])
        (if (pair? redirs)
            (make-redirected-command (make-brace-group body) redirs)
            (make-brace-group body)))))
  (define (parse-subshell ps)
    (parser-consume! ps)
    (let ([body (parse-list ps)])
      (skip-newlines! ps)
      (unless (parser-check? ps 'RPAREN)
        (error 'gerbil "parse error: expected ')'"))
      (parser-consume! ps)
      (let ([redirs (parse-redirect-list ps)])
        (if (pair? redirs)
            (make-redirected-command (make-subshell body) redirs)
            (make-subshell body)))))
  (define (parse-if ps)
    (parser-expect-word! ps "if")
    (let loop ([clauses (list)])
      (let ([condition (parse-list ps)])
        (skip-newlines! ps)
        (parser-expect-word! ps "then")
        (let ([body (parse-list ps)])
          (skip-newlines! ps)
          (let ([new-clauses (cons (cons condition body) clauses)])
            (cond
              [(parser-check-word? ps "elif")
               (parser-consume! ps)
               (loop new-clauses)]
              [(parser-check-word? ps "else")
               (parser-consume! ps)
               (let ([else-body (parse-list ps)])
                 (skip-newlines! ps)
                 (parser-expect-word! ps "fi")
                 (let* ([cmd (make-if-command
                               (reverse new-clauses)
                               else-body)])
                   (let* ([redirs (parse-redirect-list ps)])
                     (if (pair? redirs)
                         (make-redirected-command cmd redirs)
                         cmd))))]
              [else
               (parser-expect-word! ps "fi")
               (let* ([cmd (make-if-command (reverse new-clauses) #f)])
                 (let* ([redirs (parse-redirect-list ps)])
                   (if (pair? redirs)
                       (make-redirected-command cmd redirs)
                       cmd)))]))))))
  (define (parse-while ps)
    (parser-expect-word! ps "while")
    (let ([test (parse-list ps)])
      (skip-newlines! ps)
      (parser-expect-word! ps "do")
      (let ([body (parse-list ps)])
        (skip-newlines! ps)
        (parser-expect-word! ps "done")
        (let* ([cmd (make-while-command test body)])
          (let* ([redirs (parse-redirect-list ps)])
            (if (pair? redirs)
                (make-redirected-command cmd redirs)
                cmd))))))
  (define (parse-until ps)
    (parser-expect-word! ps "until")
    (let ([test (parse-list ps)])
      (skip-newlines! ps)
      (parser-expect-word! ps "do")
      (let ([body (parse-list ps)])
        (skip-newlines! ps)
        (parser-expect-word! ps "done")
        (let* ([cmd (make-until-command test body)])
          (let* ([redirs (parse-redirect-list ps)])
            (if (pair? redirs)
                (make-redirected-command cmd redirs)
                cmd))))))
  (define (parse-for ps)
    (parser-expect-word! ps "for")
    (if (parser-check? ps 'LPAREN)
        (let ([saved (parser-next! ps)])
          (if (parser-check? ps 'LPAREN)
              (parse-arith-for ps)
              (begin
                (parser-state-peeked-set! ps saved)
                (parse-for-in ps))))
        (parse-for-in ps)))
  (define (parse-for-in ps)
    (let ([name-tok (parser-next! ps)])
      (unless (and (token? name-tok)
                   (eq? (token-type name-tok) 'WORD))
        (error 'gerbil
          "parse error: expected variable name after 'for'"))
      (let ([var-name (token-value name-tok)])
        (skip-newlines! ps)
        (let ([words (if (parser-check-word? ps "in")
                         (begin
                           (parser-consume! ps)
                           (let loop ([words (list)])
                             (let ([tok (parser-peek ps)])
                               (cond
                                 [(or (parser-check? ps 'SEMI)
                                      (parser-check? ps 'NEWLINE))
                                  (parser-consume! ps)
                                  (reverse words)]
                                 [(and (token? tok)
                                       (eq? (token-type tok) 'WORD)
                                       (not (string=?
                                              (token-value tok)
                                              "do")))
                                  (loop
                                    (cons
                                      (token-value (parser-next! ps))
                                      words))]
                                 [else (reverse words)]))))
                         (begin
                           (when (parser-check? ps 'SEMI)
                             (parser-consume! ps))
                           #f))])
          (skip-newlines! ps)
          (parser-expect-word! ps "do")
          (let ([body (parse-list ps)])
            (skip-newlines! ps)
            (parser-expect-word! ps "done")
            (let* ([cmd (make-for-command var-name words body)])
              (let* ([redirs (parse-redirect-list ps)])
                (if (pair? redirs)
                    (make-redirected-command cmd redirs)
                    cmd))))))))
  (define (parse-arith-command ps)
    (parser-consume! ps)
    (let ([expr (read-arith-body ps)])
      (make-arith-command expr)))
  (define (parse-arith-for ps)
    (parser-consume! ps)
    (let* ([init-expr (read-arith-until-semi ps)])
      (let* ([test-expr (read-arith-until-semi ps)])
        (let* ([update-expr (read-arith-until-close ps)])
          (when (parser-check? ps 'SEMI) (parser-consume! ps))
          (skip-newlines! ps)
          (if (parser-check-word? ps "{")
              (begin
                (parser-consume! ps)
                (let ([body (parse-list ps)])
                  (skip-newlines! ps)
                  (parser-expect-word! ps "}")
                  (make-arith-for-command
                    init-expr
                    test-expr
                    update-expr
                    body)))
              (begin
                (parser-expect-word! ps "do")
                (let ([body (parse-list ps)])
                  (skip-newlines! ps)
                  (parser-expect-word! ps "done")
                  (make-arith-for-command
                    init-expr
                    test-expr
                    update-expr
                    body))))))))
  (define (lexer-raw-char lex)
    (if (>= (lexer-pos lex) (lexer-len lex))
        #f
        (string-ref (lexer-input lex) (lexer-pos lex))))
  (define (lexer-raw-advance! lex)
    (lexer-pos-set! lex (+ 1 (lexer-pos lex))))
  (define (parser-clear-peeked! ps)
    (parser-state-peeked-set! ps #f)
    (lexer-peeked-set! (parser-state-lexer ps) #f))
  (define (read-arith-body ps)
    (parser-clear-peeked! ps)
    (let ([lex (parser-state-lexer ps)]
          [buf (open-output-string)])
      (let loop ([depth 0])
        (let ([ch (lexer-raw-char lex)])
          (cond
            [(not ch) (error 'gerbil "parse error: expected '))'")]
            [(char=? ch #\()
             (lexer-raw-advance! lex)
             (display ch buf)
             (loop (+ depth 1))]
            [(char=? ch #\))
             (if (> depth 0)
                 (begin
                   (lexer-raw-advance! lex)
                   (display ch buf)
                   (loop (- depth 1)))
                 (begin
                   (lexer-raw-advance! lex)
                   (let ([ch2 (lexer-raw-char lex)])
                     (if (and ch2 (char=? ch2 #\)))
                         (begin
                           (lexer-raw-advance! lex)
                           (string-trim-ws (get-output-string buf)))
                         (begin (display ")" buf) (loop depth))))))]
            [else
             (lexer-raw-advance! lex)
             (display ch buf)
             (loop depth)])))))
  (define (read-arith-until-semi ps)
    (parser-clear-peeked! ps)
    (let ([lex (parser-state-lexer ps)]
          [buf (open-output-string)])
      (let loop ([depth 0])
        (let ([ch (lexer-raw-char lex)])
          (cond
            [(not ch)
             (error 'gerbil "parse error: expected ';' in for (( ))")]
            [(char=? ch #\()
             (lexer-raw-advance! lex)
             (display ch buf)
             (loop (+ depth 1))]
            [(char=? ch #\))
             (lexer-raw-advance! lex)
             (display ch buf)
             (loop (if (> depth 0) (- depth 1) depth))]
            [(and (char=? ch #\;) (= depth 0))
             (lexer-raw-advance! lex)
             (string-trim-ws (get-output-string buf))]
            [else
             (lexer-raw-advance! lex)
             (display ch buf)
             (loop depth)])))))
  (define (read-arith-until-close ps)
    (parser-clear-peeked! ps)
    (let ([lex (parser-state-lexer ps)]
          [buf (open-output-string)])
      (let loop ([depth 0])
        (let ([ch (lexer-raw-char lex)])
          (cond
            [(not ch)
             (error 'gerbil "parse error: expected '))' in for (( ))")]
            [(char=? ch #\()
             (lexer-raw-advance! lex)
             (display ch buf)
             (loop (+ depth 1))]
            [(char=? ch #\))
             (if (> depth 0)
                 (begin
                   (lexer-raw-advance! lex)
                   (display ch buf)
                   (loop (- depth 1)))
                 (begin
                   (lexer-raw-advance! lex)
                   (let ([ch2 (lexer-raw-char lex)])
                     (if (and ch2 (char=? ch2 #\)))
                         (begin
                           (lexer-raw-advance! lex)
                           (string-trim-ws (get-output-string buf)))
                         (begin (display ")" buf) (loop depth))))))]
            [else
             (lexer-raw-advance! lex)
             (display ch buf)
             (loop depth)])))))
  (define (string-trim-ws str)
    (let* ([len (string-length str)])
      (let* ([end (let loop ([i (- len 1)])
                    (if (and (>= i 0)
                             (char-whitespace? (string-ref str i)))
                        (loop (- i 1))
                        (+ i 1)))])
        (substring str 0 end))))
  (define (parse-case ps)
    (parser-expect-word! ps "case")
    (let ([word-tok (parser-next! ps)])
      (unless (and (token? word-tok)
                   (eq? (token-type word-tok) 'WORD))
        (error 'gerbil "parse error: expected word after 'case'"))
      (let ([word (token-value word-tok)])
        (skip-newlines! ps)
        (parser-expect-word! ps "in")
        (skip-newlines! ps)
        (let loop ([clauses (list)])
          (cond
            [(parser-check-word? ps "esac")
             (parser-consume! ps)
             (let* ([cmd (make-case-command word (reverse clauses))])
               (let* ([redirs (parse-redirect-list ps)])
                 (if (pair? redirs)
                     (make-redirected-command cmd redirs)
                     cmd)))]
            [else
             (when (parser-check? ps 'LPAREN) (parser-consume! ps))
             (let pattern-loop ([patterns (list)])
               (let ([pat-tok (parser-next! ps)])
                 (unless (and (token? pat-tok)
                              (eq? (token-type pat-tok) 'WORD))
                   (error 'gerbil
                     "parse error: expected pattern in case clause"))
                 (let ([new-patterns (cons
                                       (token-value pat-tok)
                                       patterns)])
                   (if (parser-check? ps 'PIPE)
                       (begin
                         (parser-consume! ps)
                         (pattern-loop new-patterns))
                       (begin
                         (unless (parser-check? ps 'RPAREN)
                           (error 'gerbil
                             "parse error: expected ')' in case clause"))
                         (parser-consume! ps)
                         (skip-newlines! ps)
                         (let ([body (parse-list ps)])
                           (skip-newlines! ps)
                           (let ([terminator (cond
                                               [(parser-check?
                                                  ps
                                                  'DSEMI_AMP)
                                                (parser-consume! ps)
                                                'test-next]
                                               [(parser-check?
                                                  ps
                                                  'SEMI_AMP)
                                                (parser-consume! ps)
                                                'fallthrough]
                                               [(parser-check? ps 'DSEMI)
                                                (parser-consume! ps)
                                                'break]
                                               [else 'break])])
                             (skip-newlines! ps)
                             (loop
                               (cons
                                 (make-case-clause
                                   (reverse new-patterns)
                                   body
                                   terminator)
                                 clauses)))))))))])))))
  (define (parse-select ps)
    (parser-expect-word! ps "select")
    (let ([name-tok (parser-next! ps)])
      (unless (and (token? name-tok)
                   (eq? (token-type name-tok) 'WORD))
        (error 'gerbil
          "parse error: expected variable name after 'select'"))
      (let ([var-name (token-value name-tok)])
        (skip-newlines! ps)
        (parser-expect-word! ps "in")
        (let loop ([words (list)])
          (let ([tok (parser-peek ps)])
            (cond
              [(or (parser-check? ps 'SEMI) (parser-check? ps 'NEWLINE))
               (parser-consume! ps)
               (skip-newlines! ps)
               (parser-expect-word! ps "do")
               (let ([body (parse-list ps)])
                 (skip-newlines! ps)
                 (parser-expect-word! ps "done")
                 (make-select-command var-name (reverse words) body))]
              [(and (token? tok)
                    (eq? (token-type tok) 'WORD)
                    (not (string=? (token-value tok) "do")))
               (loop (cons (token-value (parser-next! ps)) words))]
              [else
               (parser-expect-word! ps "do")
               (let ([body (parse-list ps)])
                 (skip-newlines! ps)
                 (parser-expect-word! ps "done")
                 (make-select-command
                   var-name
                   (reverse words)
                   body))]))))))
  (define (parse-function-def-keyword ps)
    (parser-consume! ps)
    (let ([name-tok (parser-next! ps)])
      (unless (and (token? name-tok)
                   (eq? (token-type name-tok) 'WORD))
        (error 'gerbil "parse error: expected function name"))
      (when (parser-check? ps 'LPAREN)
        (parser-consume! ps)
        (unless (parser-check? ps 'RPAREN)
          (error 'gerbil
            "parse error: expected ')' after '(' in function definition"))
        (parser-consume! ps))
      (let ([func-line (lexer-line (parser-state-lexer ps))])
        (skip-newlines! ps)
        (let ([body (parse-command ps)]
              [redirs (parse-redirect-list ps)])
          (make-function-def
            (token-value name-tok)
            body
            redirs
            func-line)))))
  (define (parse-cond-command ps)
    (parser-consume! ps)
    (let ([expr (parse-cond-or ps)])
      (unless (parser-check-word? ps "]]")
        (error 'gerbil "parse error: expected ']]'"))
      (parser-consume! ps)
      (make-cond-command expr)))
  (define (parse-cond-or ps)
    (let loop ([left (parse-cond-and ps)])
      (cond
        [(parser-check? ps 'OR_IF)
         (parser-consume! ps)
         (loop (make-cond-binary "||" left (parse-cond-and ps)))]
        [else left])))
  (define (parse-cond-and ps)
    (let loop ([left (parse-cond-not ps)])
      (cond
        [(parser-check? ps 'AND_IF)
         (parser-consume! ps)
         (loop (make-cond-binary "&&" left (parse-cond-not ps)))]
        [else left])))
  (define (parse-cond-not ps)
    (if (parser-check? ps 'BANG)
        (begin
          (parser-consume! ps)
          (make-cond-not (parse-cond-not ps)))
        (parse-cond-primary ps)))
  (define (cond-unary-op? s)
    (member
      s
      '("-a" "-b" "-c" "-d" "-e" "-f" "-g" "-h" "-k" "-p" "-r"
        "-s" "-t" "-u" "-w" "-x" "-G" "-L" "-N" "-O" "-S" "-z" "-n"
        "-o" "-v" "-R")))
  (define (cond-binary-op? s)
    (member
      s
      '("=" "==" "!=" "<" ">" "=~" "-eq" "-ne" "-lt" "-gt" "-le"
            "-ge" "-ef" "-nt" "-ot")))
  (define (cond-peek-binary-op ps)
    (let ([peek (parser-peek ps)])
      (and peek
           (cond
             [(and (eq? (token-type peek) 'WORD)
                   (cond-binary-op? (token-value peek)))
              (token-value peek)]
             [(eq? (token-type peek) 'LESS) "<"]
             [(eq? (token-type peek) 'GREAT) ">"]
             [(eq? (token-type peek) 'BANG) "!="]
             [else #f]))))
  (define (parse-cond-primary ps)
    (cond
      [(parser-check? ps 'LPAREN)
       (parser-consume! ps)
       (let ([expr (parse-cond-or ps)])
         (unless (parser-check? ps 'RPAREN)
           (error 'gerbil
             "parse error: expected ')' in [[ ]] expression"))
         (parser-consume! ps)
         expr)]
      [(or (parser-check? ps 'WORD)
           (parser-check? ps 'ASSIGNMENT_WORD))
       (let* ([tok (parser-next! ps)])
         (let* ([val (token-value tok)])
           (if (cond-unary-op? val)
               (let ([arg-tok (parser-next! ps)])
                 (make-cond-unary-test val (token-value arg-tok)))
               (let ([op (cond-peek-binary-op ps)])
                 (if op
                     (begin
                       (parser-consume! ps)
                       (when (string=? op "!=") (parser-consume! ps))
                       (let ([right (cond-collect-rhs ps op)])
                         (make-cond-binary-test op val right)))
                     (make-cond-word val))))))]
      [else
       (error 'gerbil
         "parse error: unexpected token in [[ ]] expression"
         (let ([t (parser-peek ps)]) (and t (token-value t))))]))
  (define (cond-collect-rhs ps op)
    (if (member op '("=~" "==" "=" "!="))
        (let loop ([parts (list)])
          (let ([peek (parser-peek ps)])
            (cond
              [(not peek) (apply string-append (reverse parts))]
              [(and (eq? (token-type peek) 'WORD)
                    (member (token-value peek) '("]]")))
               (apply string-append (reverse parts))]
              [(memq (token-type peek) '(AND_AND OR_OR))
               (apply string-append (reverse parts))]
              [(eq? (token-type peek) 'RPAREN)
               (if (null? parts)
                   (apply string-append (reverse parts))
                   (begin (parser-consume! ps) (loop (cons ")" parts))))]
              [(eq? (token-type peek) 'LPAREN)
               (parser-consume! ps)
               (loop (cons "(" parts))]
              [(eq? (token-type peek) 'LESS)
               (parser-consume! ps)
               (loop (cons "<" parts))]
              [(eq? (token-type peek) 'GREAT)
               (parser-consume! ps)
               (loop (cons ">" parts))]
              [(eq? (token-type peek) 'BANG)
               (parser-consume! ps)
               (loop (cons "!" parts))]
              [(eq? (token-type peek) 'PIPE)
               (parser-consume! ps)
               (loop (cons "|" parts))]
              [(memq (token-type peek) '(WORD ASSIGNMENT_WORD))
               (parser-consume! ps)
               (loop (cons (token-value peek) parts))]
              [else (apply string-append (reverse parts))])))
        (let ([tok (parser-next! ps)]) (token-value tok)))))
