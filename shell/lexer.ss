;;; lexer.ss — Shell tokenizer for gsh
;;; Handles quoting, operators, heredocs, line continuations,
;;; assignment detection, ANSI-C quoting, and comments.

(export #t)
(import :std/sugar
        :std/format
        :gsh/ast
        :gsh/util)

;;; --- Lexer state ---

(defstruct lexer
  (input        ;; input string
   pos          ;; current position
   len          ;; input length
   tokens       ;; accumulated tokens (reverse order)
   peeked       ;; peeked token or #f
   heredocs     ;; list of pending heredoc specs: ((delimiter strip-tabs? quoted?) ...)
   want-more?   ;; #t if input is incomplete (open quote, heredoc, etc.)
   line         ;; current line number
   col          ;; current column number
   extglob?     ;; #t if extglob shopt is enabled (for ?()/*()/+()/@()/!() patterns)
   )
  transparent: #t)

;;; --- Public interface ---

;; Create a new lexer for the given input string
(def (make-shell-lexer input (extglob? #f))
  (make-lexer input 0 (string-length input) [] #f [] #f 1 0 extglob?))

;; Get the next token, or 'eof
(def (lexer-next! lex)
  (if (lexer-peeked lex)
    (let ((tok (lexer-peeked lex)))
      (set! (lexer-peeked lex) #f)
      tok)
    (lexer-read-token! lex)))

;; Peek at the next token without consuming it
(def (lexer-peek lex)
  (unless (lexer-peeked lex)
    (set! (lexer-peeked lex) (lexer-read-token! lex)))
  (lexer-peeked lex))

;; Check if the lexer needs more input
(def (lexer-needs-more? lex)
  (lexer-want-more? lex))

;; Prepend text to the lexer's input stream (used for alias expansion).
;; Inserts text at the current read position, so it will be tokenized next.
(def (lexer-prepend-text! lex text)
  (let* ((remaining (substring (lexer-input lex) (lexer-pos lex) (lexer-len lex)))
         (new-input (string-append text remaining)))
    (set! (lexer-input lex) new-input)
    (set! (lexer-pos lex) 0)
    (set! (lexer-len lex) (string-length new-input))
    ;; Clear any peeked token since the input has changed
    (set! (lexer-peeked lex) #f)))

;; Tokenize an entire input string, return list of tokens
(def (tokenize input)
  (let ((lex (make-shell-lexer input)))
    (let loop ((tokens []))
      (let ((tok (lexer-next! lex)))
        (if (eq? tok 'eof)
          (reverse tokens)
          (loop (cons tok tokens)))))))

;;; --- Main tokenizer ---

(def (lexer-read-token! lex)
  (skip-whitespace! lex)
  (if (>= (lexer-pos lex) (lexer-len lex))
    ;; Check for pending heredocs
    (if (pair? (lexer-heredocs lex))
      (begin
        (set! (lexer-want-more? lex) #t)
        'eof)
      'eof)
    (let ((ch (current-char lex)))
      (cond
        ;; Comment
        ((char=? ch #\#)
         (skip-comment! lex)
         (lexer-read-token! lex))
        ;; Newline
        ((char=? ch #\newline)
         (advance! lex)
         ;; Process pending heredocs
         (when (pair? (lexer-heredocs lex))
           (collect-heredocs! lex))
         (make-token 'NEWLINE "\n" (cons (lexer-line lex) (lexer-col lex))))
        ;; Operators (multi-char first)
        ((operator-start? ch)
         (read-operator! lex))
        ;; Single quote
        ((char=? ch #\')
         (read-single-quoted! lex))
        ;; Double quote
        ((char=? ch #\")
         (read-double-quoted! lex))
        ;; ANSI-C quote: $'...'
        ((and (char=? ch #\$)
              (< (+ (lexer-pos lex) 1) (lexer-len lex))
              (char=? (char-at lex (+ (lexer-pos lex) 1)) #\'))
         (read-ansi-c-quoted! lex))
        ;; Locale quote: $"..."
        ((and (char=? ch #\$)
              (< (+ (lexer-pos lex) 1) (lexer-len lex))
              (char=? (char-at lex (+ (lexer-pos lex) 1)) #\"))
         (advance! lex)  ;; skip $
         (read-double-quoted! lex))
        ;; Backslash
        ((char=? ch #\\)
         (if (and (< (+ (lexer-pos lex) 1) (lexer-len lex))
                  (char=? (char-at lex (+ (lexer-pos lex) 1)) #\newline))
           ;; Line continuation
           (begin
             (advance! lex) (advance! lex)
             (lexer-read-token! lex))
           ;; Escaped character — start of word
           (read-word! lex)))
        ;; Regular word
        (else
         (read-word! lex))))))

;;; --- Character access ---

(def (current-char lex)
  (string-ref (lexer-input lex) (lexer-pos lex)))

(def (char-at lex pos)
  (string-ref (lexer-input lex) pos))

(def (advance! lex)
  (when (< (lexer-pos lex) (lexer-len lex))
    (when (char=? (string-ref (lexer-input lex) (lexer-pos lex)) #\newline)
      (set! (lexer-line lex) (+ 1 (lexer-line lex)))
      (set! (lexer-col lex) 0))
    (set! (lexer-pos lex) (+ 1 (lexer-pos lex)))
    (set! (lexer-col lex) (+ 1 (lexer-col lex)))))

(def (at-end? lex)
  (>= (lexer-pos lex) (lexer-len lex)))

;;; --- Whitespace and comments ---

(def (skip-whitespace! lex)
  (let loop ()
    (when (and (not (at-end? lex))
               (let ((ch (current-char lex)))
                 (and (char-whitespace? ch)
                      (not (char=? ch #\newline)))))
      (advance! lex)
      (loop))))

(def (skip-comment! lex)
  ;; Skip from # to end of line (but not the newline itself)
  (let loop ()
    (when (and (not (at-end? lex))
               (not (char=? (current-char lex) #\newline)))
      (advance! lex)
      (loop))))

;;; --- Operators ---

(def (operator-start? ch)
  (memq ch '(#\| #\& #\; #\( #\) #\< #\> #\!)))

;;; Read the body of a process substitution <(...) or >(...)
;;; The opening ( has already been consumed. Reads until matching ).
(def (read-procsub-body! lex)
  (let ((buf (open-output-string)))
    (let loop ((depth 1))
      (cond
        ((at-end? lex) (set! (lexer-want-more? lex) #t))
        ((char=? (current-char lex) #\()
         (display #\( buf) (advance! lex) (loop (+ depth 1)))
        ((char=? (current-char lex) #\))
         (if (= depth 1)
           (advance! lex)  ;; consume final ), done
           (begin (display #\) buf) (advance! lex) (loop (- depth 1)))))
        ((char=? (current-char lex) #\')
         (display #\' buf) (advance! lex)
         (let inner ()
           (cond
             ((at-end? lex) #!void)
             ((char=? (current-char lex) #\')
              (display #\' buf) (advance! lex))
             (else (display (current-char lex) buf) (advance! lex) (inner))))
         (loop depth))
        ((char=? (current-char lex) #\\)
         (display #\\ buf) (advance! lex)
         (unless (at-end? lex)
           (display (current-char lex) buf) (advance! lex))
         (loop depth))
        ((char=? (current-char lex) #\")
         (display #\" buf) (advance! lex)
         (let inner ()
           (cond
             ((at-end? lex) #!void)
             ((char=? (current-char lex) #\")
              (display #\" buf) (advance! lex))
             ((char=? (current-char lex) #\\)
              (display #\\ buf) (advance! lex)
              (unless (at-end? lex)
                (display (current-char lex) buf) (advance! lex))
              (inner))
             (else (display (current-char lex) buf) (advance! lex) (inner))))
         (loop depth))
        (else
         (display (current-char lex) buf)
         (advance! lex)
         (loop depth))))
    (get-output-string buf)))

(def (read-operator! lex)
  (let* ((pos (cons (lexer-line lex) (lexer-col lex)))
         (ch (current-char lex)))
    (advance! lex)
    (case ch
      ((#\|)
       (cond
         ((and (not (at-end? lex)) (char=? (current-char lex) #\|))
          (advance! lex)
          (make-token 'OR_IF "||" pos))
         ((and (not (at-end? lex)) (char=? (current-char lex) #\&))
          (advance! lex)
          (make-token 'PIPEAMP "|&" pos))
         (else
          (make-token 'PIPE "|" pos))))
      ((#\&)
       (cond
         ((and (not (at-end? lex)) (char=? (current-char lex) #\&))
          (advance! lex)
          (make-token 'AND_IF "&&" pos))
         ((and (not (at-end? lex)) (char=? (current-char lex) #\>))
          (advance! lex)
          (if (and (not (at-end? lex)) (char=? (current-char lex) #\>))
            (begin (advance! lex)
                   (make-token 'AMPGREAT_GREAT "&>>" pos))
            (make-token 'AMPGREAT "&>" pos)))
         (else
          (make-token 'AMP "&" pos))))
      ((#\;)
       (cond
         ((and (not (at-end? lex)) (char=? (current-char lex) #\;))
          (advance! lex)
          ;; Check for ;;& (test-next in case)
          (if (and (not (at-end? lex)) (char=? (current-char lex) #\&))
            (begin (advance! lex)
                   (make-token 'DSEMI_AMP ";;&" pos))
            (make-token 'DSEMI ";;" pos)))
         ((and (not (at-end? lex)) (char=? (current-char lex) #\&))
          (advance! lex)
          (make-token 'SEMI_AMP ";&" pos))
         (else
          (make-token 'SEMI ";" pos))))
      ((#\()
       (make-token 'LPAREN "(" pos))
      ((#\))
       (make-token 'RPAREN ")" pos))
      ((#\!)
       (make-token 'BANG "!" pos))
      ((#\<)
       (cond
         ((and (not (at-end? lex)) (char=? (current-char lex) #\<))
          (advance! lex)
          (if (and (not (at-end? lex)) (char=? (current-char lex) #\-))
            (begin (advance! lex)
                   (make-token 'DLESSDASH "<<-" pos))
            (if (and (not (at-end? lex)) (char=? (current-char lex) #\<))
              (begin (advance! lex)
                     (make-token 'TLESS "<<<" pos))
              (make-token 'DLESS "<<" pos))))
         ((and (not (at-end? lex)) (char=? (current-char lex) #\&))
          (advance! lex)
          (make-token 'LESSAND "<&" pos))
         ((and (not (at-end? lex)) (char=? (current-char lex) #\>))
          (advance! lex)
          (make-token 'LESSGREAT "<>" pos))
         ((and (not (at-end? lex)) (char=? (current-char lex) #\())
          (advance! lex)
          (make-token 'PROCSUB_IN (read-procsub-body! lex) pos))
         (else
          (make-token 'LESS "<" pos))))
      ((#\>)
       (cond
         ((and (not (at-end? lex)) (char=? (current-char lex) #\>))
          (advance! lex)
          (make-token 'DGREAT ">>" pos))
         ((and (not (at-end? lex)) (char=? (current-char lex) #\&))
          (advance! lex)
          (make-token 'GREATAND ">&" pos))
         ((and (not (at-end? lex)) (char=? (current-char lex) #\|))
          (advance! lex)
          (make-token 'CLOBBER ">|" pos))
         ((and (not (at-end? lex)) (char=? (current-char lex) #\())
          (advance! lex)
          (make-token 'PROCSUB_OUT (read-procsub-body! lex) pos))
         (else
          (make-token 'GREAT ">" pos))))
      (else
       (make-token 'WORD (string ch) pos)))))

;;; --- Extglob helpers ---

;; Check if the previous character in the input (before current pos) is an extglob prefix
(def (extglob-prefix-before? lex)
  (let ((p (lexer-pos lex)))
    (and (> p 0)
         (let ((prev-ch (char-at lex (- p 1))))
           (or (char=? prev-ch #\?) (char=? prev-ch #\*)
               (char=? prev-ch #\+) (char=? prev-ch #\@)
               (char=? prev-ch #\!))))))

;; Read an extglob body: ( already peeked, reads everything up to matching )
;; Writes the ( ... ) into buf, respecting nested parens and quoting
(def (read-extglob-body! lex buf)
  (display (current-char lex) buf)  ;; write the (
  (advance! lex)
  (let loop ((depth 1))
    (cond
      ((at-end? lex)
       (set! (lexer-want-more? lex) #t))
      (else
       (let ((ch (current-char lex)))
         (cond
           ((char=? ch #\\)
            (display ch buf) (advance! lex)
            (unless (at-end? lex)
              (display (current-char lex) buf) (advance! lex))
            (loop depth))
           ((char=? ch #\')
            ;; Read single-quoted content preserving markers
            (let ((quoted (read-single-quoted-content! lex)))
              (display "'" buf)
              (display quoted buf)
              (display "'" buf))
            (loop depth))
           ((char=? ch #\")
            ;; Read double-quoted content preserving markers
            (let ((quoted (read-double-quoted-content! lex)))
              (display "\"" buf)
              (display quoted buf)
              (display "\"" buf))
            (loop depth))
           ((char=? ch #\()
            (display ch buf) (advance! lex)
            (loop (+ depth 1)))
           ((char=? ch #\))
            (display ch buf) (advance! lex)
            (when (> depth 1) (loop (- depth 1))))
           ;; | is a separator inside extglob, write it as-is
           ((char=? ch #\|)
            (display ch buf) (advance! lex)
            (loop depth))
           ;; $ for variables/substitutions
           ((char=? ch #\$)
            (display (read-dollar! lex) buf)
            (loop depth))
           ;; Backtick
           ((char=? ch #\`)
            (display (read-backtick! lex) buf)
            (loop depth))
           (else
            (display ch buf) (advance! lex)
            (loop depth))))))))

;;; --- Word reading ---

(def (read-word! lex)
  (let* ((pos (cons (lexer-line lex) (lexer-col lex)))
         (buf (open-output-string)))
    (let loop ((has-content? #f))
      (if (at-end? lex)
        (finish-word buf pos)
        (let ((ch (current-char lex)))
          (cond
            ;; Word delimiters
            ((or (char-whitespace? ch)
                 (and (operator-start? ch)
                      ;; Don't break on ! inside a word (already has content)
                      (not (and (char=? ch #\!) has-content?))
                      ;; Don't break on ! at word start when followed by non-delimiter
                      ;; (e.g. "!!!" should be a single word, not three BANGs)
                      (not (and (char=? ch #\!)
                                (not has-content?)
                                (< (+ (lexer-pos lex) 1) (lexer-len lex))
                                (let ((next (char-at lex (+ (lexer-pos lex) 1))))
                                  (not (or (char-whitespace? next)
                                           (memq next '(#\; #\| #\& #\( #\) #\< #\> #\newline)))))))
                      ;; Don't break on !( when extglob is enabled
                      (not (and (lexer-extglob? lex)
                                (char=? ch #\!)
                                (< (+ (lexer-pos lex) 1) (lexer-len lex))
                                (char=? (char-at lex (+ (lexer-pos lex) 1)) #\()))
                      ;; Don't break on ( when extglob prefix char precedes it
                      (not (and (lexer-extglob? lex)
                                (char=? ch #\()
                                (extglob-prefix-before? lex)))))
             (finish-word buf pos))
            ;; Extglob: ?(pat|...) *(pat|...) +(pat|...) @(pat|...) !(pat|...)
            ((and (lexer-extglob? lex)
                  (char=? ch #\()
                  (extglob-prefix-before? lex))
             (read-extglob-body! lex buf)
             (loop #t))
            ;; Single quote inside word — preserve quote markers
            ((char=? ch #\')
             (let ((quoted (read-single-quoted-content! lex)))
               (display "'" buf)
               (display quoted buf)
               (display "'" buf)
               (loop #t)))
            ;; Double quote inside word — preserve quote markers
            ((char=? ch #\")
             (let ((quoted (read-double-quoted-content! lex)))
               (display "\"" buf)
               (display quoted buf)
               (display "\"" buf)
               (loop #t)))
            ;; Backslash escape — preserve backslash in token for expander
            ((char=? ch #\\)
             (advance! lex)
             (if (at-end? lex)
               (begin (display "\\" buf)
                      (finish-word buf pos))
               (if (char=? (current-char lex) #\newline)
                 ;; Line continuation inside word
                 (begin (advance! lex) (loop has-content?))
                 (begin
                   (display "\\" buf)
                   (display (current-char lex) buf)
                   (advance! lex)
                   (loop #t)))))
            ;; Dollar (variable, command sub, arith, $'...', $"...")
            ((char=? ch #\$)
             (if (< (+ (lexer-pos lex) 1) (lexer-len lex))
               (let ((next (char-at lex (+ (lexer-pos lex) 1))))
                 (cond
                   ((char=? next #\')
                    ;; $'...' ANSI-C quoting inside word
                    (display (read-ansi-c-quoted-content! lex) buf)
                    (loop #t))
                   ((char=? next #\")
                    ;; $"..." locale translation (treated as double-quote)
                    (advance! lex) ;; skip $
                    (let ((quoted (read-double-quoted-content! lex)))
                      (display "\"" buf)
                      (display quoted buf)
                      (display "\"" buf)
                      (loop #t)))
                   (else
                    (display (read-dollar! lex) buf)
                    (loop #t))))
               (begin
                 (display (read-dollar! lex) buf)
                 (loop #t))))
            ;; Backtick command substitution
            ((char=? ch #\`)
             (display (read-backtick! lex) buf)
             (loop #t))
            ;; Regular character
            (else
             (display ch buf)
             (advance! lex)
             (loop #t))))))))

(def (finish-word buf pos)
  (let ((word (get-output-string buf)))
    (if (= (string-length word) 0)
      (error "lexer: empty word")
      ;; Classify the token
      (cond
        ;; IO_NUMBER: digits followed by < or >
        ;; Bash treats multi-digit numbers as IO_NUMBERs too (e.g. 20> file)
        ((and (> (string-length word) 0)
              (let check-digits ((i 0))
                (if (>= i (string-length word)) #t
                  (and (char-numeric? (string-ref word i))
                       (check-digits (+ i 1)))))
              (not (at-end? (current-lexer)))
              (let ((ch (current-char (current-lexer))))
                (or (char=? ch #\<) (char=? ch #\>))))
         (make-token 'IO_NUMBER word pos))
        ;; IO_VARNAME: {varname} followed by < or >
        ;; Bash named fd syntax: exec {fd}>file allocates fd and stores in $fd
        ((and (> (string-length word) 2)
              (char=? (string-ref word 0) #\{)
              (char=? (string-ref word (- (string-length word) 1)) #\})
              (let* ((name (substring word 1 (- (string-length word) 1)))
                     (len (string-length name)))
                (and (> len 0)
                     ;; First char: letter or underscore
                     (let ((c0 (string-ref name 0)))
                       (or (char-alphabetic? c0) (char=? c0 #\_)))
                     ;; Rest: alphanumeric or underscore
                     (let check-name ((i 1))
                       (if (>= i len) #t
                         (let ((c (string-ref name i)))
                           (and (or (char-alphabetic? c)
                                    (char-numeric? c)
                                    (char=? c #\_))
                                (check-name (+ i 1))))))))
              (not (at-end? (current-lexer)))
              (let ((ch (current-char (current-lexer))))
                (or (char=? ch #\<) (char=? ch #\>))))
         ;; Extract the variable name (without braces)
         (make-token 'IO_VARNAME (substring word 1 (- (string-length word) 1)) pos))
        ;; Assignment: NAME= or NAME+=
        ((assignment-word? word)
         (make-token 'ASSIGNMENT_WORD word pos))
        ;; Regular word (parser will check for reserved words)
        (else
         (make-token 'WORD word pos))))))

;; Thread-local current lexer for finish-word to access
(def current-lexer (make-parameter #f))

;; Wrap read-word! to set current-lexer
(def (read-word-with-context! lex)
  (parameterize ((current-lexer lex))
    (read-word! lex)))

;; Override the main token reader to use parameterize
(def (lexer-read-token-impl! lex)
  (parameterize ((current-lexer lex))
    ;; Return queued tokens first (from heredoc collection)
    (if (pair? (lexer-tokens lex))
      (let ((tok (car (lexer-tokens lex))))
        (set! (lexer-tokens lex) (cdr (lexer-tokens lex)))
        tok)
    (begin
    (skip-whitespace! lex)
    (if (>= (lexer-pos lex) (lexer-len lex))
      (if (pair? (lexer-heredocs lex))
        (begin (set! (lexer-want-more? lex) #t) 'eof)
        'eof)
      (let ((ch (current-char lex)))
        (cond
          ((char=? ch #\#) (skip-comment! lex) (lexer-read-token-impl! lex))
          ((char=? ch #\newline)
           (advance! lex)
           (when (pair? (lexer-heredocs lex)) (collect-heredocs! lex))
           (make-token 'NEWLINE "\n" (cons (lexer-line lex) (lexer-col lex))))
          ;; Extglob: !( at start of token should be read as word, not BANG
          ((and (lexer-extglob? lex)
                (char=? ch #\!)
                (< (+ (lexer-pos lex) 1) (lexer-len lex))
                (char=? (char-at lex (+ (lexer-pos lex) 1)) #\())
           (read-word! lex))
          ;; ! is only BANG (pipeline negation) when followed by whitespace,
          ;; EOF, or a delimiter. Otherwise it's part of a word (e.g. "!!!")
          ((and (char=? ch #\!)
                (or (>= (+ (lexer-pos lex) 1) (lexer-len lex))
                    (let ((next (char-at lex (+ (lexer-pos lex) 1))))
                      (or (char-whitespace? next)
                          (memq next '(#\; #\| #\& #\( #\) #\< #\> #\newline))))))
           (read-operator! lex))
          ((and (char=? ch #\!) ;; ! followed by non-delimiter → word
                (< (+ (lexer-pos lex) 1) (lexer-len lex)))
           (read-word! lex))
          ((operator-start? ch) (read-operator! lex))
          ((char=? ch #\') (read-word! lex))
          ((char=? ch #\") (read-word! lex))
          ((and (char=? ch #\$)
                (< (+ (lexer-pos lex) 1) (lexer-len lex))
                (char=? (char-at lex (+ (lexer-pos lex) 1)) #\'))
           (read-word! lex))
          ((and (char=? ch #\$)
                (< (+ (lexer-pos lex) 1) (lexer-len lex))
                (char=? (char-at lex (+ (lexer-pos lex) 1)) #\"))
           (read-word! lex))
          ((char=? ch #\\)
           (if (and (< (+ (lexer-pos lex) 1) (lexer-len lex))
                    (char=? (char-at lex (+ (lexer-pos lex) 1)) #\newline))
             (begin (advance! lex) (advance! lex) (lexer-read-token-impl! lex))
             (read-word! lex)))
          (else (read-word! lex)))))))))

;; Replace the main entry point
(set! lexer-read-token! lexer-read-token-impl!)

;;; --- Quoting ---

(def (read-single-quoted! lex)
  (let ((pos (cons (lexer-line lex) (lexer-col lex)))
        (content (read-single-quoted-content! lex)))
    (make-token 'WORD (string-append "'" content "'") pos)))

(def (read-single-quoted-content! lex)
  (advance! lex)  ;; skip opening '
  (let ((buf (open-output-string)))
    (let loop ()
      (cond
        ((at-end? lex)
         (set! (lexer-want-more? lex) #t)
         (get-output-string buf))
        ((char=? (current-char lex) #\')
         (advance! lex)
         (get-output-string buf))
        (else
         (display (current-char lex) buf)
         (advance! lex)
         (loop))))))

(def (read-double-quoted! lex)
  (let ((pos (cons (lexer-line lex) (lexer-col lex)))
        (content (read-double-quoted-content! lex)))
    (make-token 'WORD (string-append "\"" content "\"") pos)))

(def (read-double-quoted-content! lex)
  (advance! lex)  ;; skip opening "
  (let ((buf (open-output-string)))
    (let loop ()
      (cond
        ((at-end? lex)
         (set! (lexer-want-more? lex) #t)
         (get-output-string buf))
        ((char=? (current-char lex) #\")
         (advance! lex)
         (get-output-string buf))
        ((char=? (current-char lex) #\\)
         (advance! lex)
         (if (at-end? lex)
           (begin (display "\\" buf) (get-output-string buf))
           (let ((next (current-char lex)))
             ;; Inside double quotes, only these chars are special after backslash
             ;; Preserve backslash-escapes so the expander can handle them
             (if (memq next '(#\$ #\` #\" #\\ #\newline))
               (if (char=? next #\newline)
                 (begin (advance! lex) (loop))  ;; line continuation
                 (begin (display "\\" buf) (display next buf) (advance! lex) (loop)))
               (begin (display "\\" buf) (display next buf) (advance! lex) (loop))))))
        ((char=? (current-char lex) #\$)
         (display (read-dollar! lex) buf)
         (loop))
        ((char=? (current-char lex) #\`)
         (display (read-backtick! lex) buf)
         (loop))
        (else
         (display (current-char lex) buf)
         (advance! lex)
         (loop))))))

;; Write a literal character to buf for ANSI-C quoted output.
;; The output is wrapped in double quotes, so escape chars that
;; expand-string would interpret inside double quotes: $ ` \ "
(def (ansi-c-emit-char! ch buf)
  (if (memq ch '(#\" #\$ #\` #\\))
    (begin (display "\\" buf) (display ch buf))
    (display ch buf)))

;; Read $'...' content and return the double-quote-wrapped string.
;; Advances past $ and '.  Returns "\"...\"" string.
(def (read-ansi-c-quoted-content! lex)
  (advance! lex)  ;; skip $
  (advance! lex)  ;; skip '
  (let ((buf (open-output-string)))
    (display "\"" buf)
    (let loop ()
      (cond
        ((at-end? lex)
         (set! (lexer-want-more? lex) #t)
         (display "\"" buf)
         (get-output-string buf))
        ((char=? (current-char lex) #\')
         (advance! lex)
         (display "\"" buf)
         (get-output-string buf))
        ((char=? (current-char lex) #\\)
         (advance! lex)
         (if (at-end? lex)
           (begin (display "\\\\" buf)
                  (display "\"" buf)
                  (get-output-string buf))
           (let ((esc (current-char lex)))
             (advance! lex)
             (case esc
               ((#\n) (ansi-c-emit-char! #\newline buf))
               ((#\t) (ansi-c-emit-char! #\tab buf))
               ((#\r) (ansi-c-emit-char! #\return buf))
               ((#\a) (display (string (integer->char 7)) buf))
               ((#\b) (display (string (integer->char 8)) buf))
               ((#\e #\E) (display (string (integer->char #x1b)) buf))
               ((#\f) (display (string (integer->char #xc)) buf))
               ((#\v) (display (string (integer->char #xb)) buf))
               ((#\\) (display "\\\\" buf))
               ((#\') (display "'" buf))  ;; ' is safe inside "..."
               ((#\") (display "\\\"" buf))
               ((#\x)
                ;; Hex escape — byte value, use raw byte encoding for >= 128
                (let-values (((val cnt) (read-hex-digits lex 2)))
                  (if (> cnt 0)
                    (ansi-c-emit-char! (byte->raw-char (bitwise-and val #xff)) buf)
                    (begin (display "\\\\x" buf)))))
               ((#\u #\U)
                ;; Unicode escape
                (let* ((max-digits (if (char=? esc #\u) 4 8)))
                  (let-values (((val cnt) (read-hex-digits lex max-digits)))
                    (if (> cnt 0)
                      (ansi-c-emit-char! (integer->char val) buf)
                      (begin (display "\\\\" buf) (display esc buf))))))
               ((#\c)
                ;; Control character: \cX = X & 0x1f
                (if (at-end? lex)
                  (begin (display "\\\\c" buf))
                  (let ((ctrl-ch (current-char lex)))
                    (advance! lex)
                    (ansi-c-emit-char!
                     (integer->char (bitwise-and (char->integer ctrl-ch) #x1f))
                     buf))))
               ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
                ;; Octal escape: \nnn (up to 3 total digits)
                ;; esc is the first digit, read up to 2 more — use raw byte encoding
                (let* ((first-digit (- (char->integer esc) (char->integer #\0)))
                       (more (read-octal-digits-with-init lex 2 first-digit)))
                  (ansi-c-emit-char! (byte->raw-char (bitwise-and more #xff)) buf)))
               (else
                ;; Unknown escape — keep literal
                (display "\\" buf)
                (ansi-c-emit-char! esc buf)))
             (loop))))
        (else
         (ansi-c-emit-char! (current-char lex) buf)
         (advance! lex)
         (loop))))))

(def (read-ansi-c-quoted! lex)
  (let ((pos (cons (lexer-line lex) (lexer-col lex)))
        (content (read-ansi-c-quoted-content! lex)))
    (make-token 'WORD content pos)))

;;; --- Dollar expansions (kept as literal text in tokens) ---

(def (read-dollar! lex)
  (let ((buf (open-output-string)))
    (display "$" buf)
    (advance! lex)  ;; skip $
    (cond
      ((at-end? lex) (get-output-string buf))
      ;; $((  arithmetic
      ((and (char=? (current-char lex) #\()
            (< (+ (lexer-pos lex) 1) (lexer-len lex))
            (char=? (char-at lex (+ (lexer-pos lex) 1)) #\())
       (display "((" buf)
       (advance! lex) (advance! lex)
       ;; Track both (( )) nesting and single ( ) nesting
       (let loop ((paren-depth 0))
         (cond
           ((at-end? lex) (set! (lexer-want-more? lex) #t))
           ;; )) when no open single parens — close the arith
           ((and (= paren-depth 0)
                 (char=? (current-char lex) #\))
                 (< (+ (lexer-pos lex) 1) (lexer-len lex))
                 (char=? (char-at lex (+ (lexer-pos lex) 1)) #\)))
            (display "))" buf)
            (advance! lex) (advance! lex))
           ;; Nested $(( — recurse
           ((and (char=? (current-char lex) #\$)
                 (< (+ (lexer-pos lex) 2) (lexer-len lex))
                 (char=? (char-at lex (+ (lexer-pos lex) 1)) #\()
                 (char=? (char-at lex (+ (lexer-pos lex) 2)) #\())
            (display "$((" buf)
            (advance! lex) (advance! lex) (advance! lex)
            ;; Scan nested arith
            (let inner ((pd 0))
              (cond
                ((at-end? lex) (set! (lexer-want-more? lex) #t))
                ((and (= pd 0)
                      (char=? (current-char lex) #\))
                      (< (+ (lexer-pos lex) 1) (lexer-len lex))
                      (char=? (char-at lex (+ (lexer-pos lex) 1)) #\)))
                 (display "))" buf)
                 (advance! lex) (advance! lex))
                ((char=? (current-char lex) #\()
                 (display (current-char lex) buf) (advance! lex) (inner (+ pd 1)))
                ((and (char=? (current-char lex) #\)) (> pd 0))
                 (display (current-char lex) buf) (advance! lex) (inner (- pd 1)))
                (else
                 (display (current-char lex) buf) (advance! lex) (inner pd))))
            (loop paren-depth))
           ;; Single ( — increase paren depth
           ((char=? (current-char lex) #\()
            (display (current-char lex) buf) (advance! lex) (loop (+ paren-depth 1)))
           ;; Single ) — decrease paren depth
           ((and (char=? (current-char lex) #\)) (> paren-depth 0))
            (display (current-char lex) buf) (advance! lex) (loop (- paren-depth 1)))
           ;; Backslash: handle line continuation
           ((char=? (current-char lex) #\\)
            (advance! lex)
            (cond
              ((at-end? lex)
               (display "\\" buf))
              ;; Line continuation: backslash-newline consumed silently
              ((char=? (current-char lex) #\newline)
               (advance! lex)
               (loop paren-depth))
              (else
               (display "\\" buf)
               (display (current-char lex) buf)
               (advance! lex)
               (loop paren-depth))))
           (else
            (display (current-char lex) buf)
            (advance! lex)
            (loop paren-depth))))
       (get-output-string buf))
      ;; $(  command substitution
      ;; Tracks paren depth AND case/esac nesting so that ) in case
      ;; patterns doesn't prematurely close the command substitution.
      ((char=? (current-char lex) #\()
       (display "(" buf)
       (advance! lex)
       (let loop ((depth 1) (case-depth 0) (word ""))
         (define (update-case-depth w cd)
           (cond ((string=? w "case") (+ cd 1))
                 ((string=? w "esac") (max 0 (- cd 1)))
                 (else cd)))
         (cond
           ((at-end? lex) (set! (lexer-want-more? lex) #t))
           ((char=? (current-char lex) #\()
            (let ((cd (update-case-depth word case-depth)))
              (display "(" buf) (advance! lex)
              (loop (+ depth 1) cd "")))
           ((char=? (current-char lex) #\))
            (let ((cd (update-case-depth word case-depth)))
              (if (and (= depth 1) (> cd 0))
                ;; Inside a case block: ) is a case pattern delimiter, not closing
                (begin (display ")" buf) (advance! lex)
                       (loop depth cd ""))
                ;; Normal: close paren
                (begin (display ")" buf) (advance! lex)
                       (when (> depth 1) (loop (- depth 1) cd ""))))))
           ((char=? (current-char lex) #\')
            (let ((cd (update-case-depth word case-depth)))
              (display "'" buf)
              (advance! lex)
              (let inner ()
                (cond
                  ((at-end? lex) #!void)
                  ((char=? (current-char lex) #\')
                   (display "'" buf) (advance! lex))
                  (else (display (current-char lex) buf) (advance! lex) (inner))))
              (loop depth cd "")))
           ((char=? (current-char lex) #\")
            (let ((cd (update-case-depth word case-depth)))
              (display "\"" buf)
              (advance! lex)
              (let inner ()
                (cond
                  ((at-end? lex) #!void)
                  ((char=? (current-char lex) #\\)
                   (display "\\" buf) (advance! lex)
                   (unless (at-end? lex)
                     (display (current-char lex) buf) (advance! lex))
                   (inner))
                  ((char=? (current-char lex) #\")
                   (display "\"" buf) (advance! lex))
                  (else (display (current-char lex) buf) (advance! lex) (inner))))
              (loop depth cd "")))
           ((char=? (current-char lex) #\\)
            (let ((cd (update-case-depth word case-depth)))
              (display "\\" buf) (advance! lex)
              (unless (at-end? lex)
                (display (current-char lex) buf) (advance! lex))
              (loop depth cd "")))
           ;; Word characters: accumulate for keyword detection
           ((or (char-alphabetic? (current-char lex))
                (char=? (current-char lex) #\_))
            (let ((ch (current-char lex)))
              (display ch buf)
              (advance! lex)
              (loop depth case-depth (string-append word (string ch)))))
           ;; Non-word char: flush word and check keywords
           (else
            (let ((cd (update-case-depth word case-depth)))
              (display (current-char lex) buf) (advance! lex)
              (loop depth cd "")))))
       (get-output-string buf))
      ;; ${  parameter expansion
      ((char=? (current-char lex) #\{)
       (display "{" buf)
       (advance! lex)
       (let loop ((depth 1))
         (cond
           ((at-end? lex) (set! (lexer-want-more? lex) #t))
           ((char=? (current-char lex) #\{)
            (display "{" buf) (advance! lex) (loop (+ depth 1)))
           ((char=? (current-char lex) #\})
            (display "}" buf) (advance! lex)
            (when (> depth 1) (loop (- depth 1))))
           ;; Backslash: handle line continuation and escapes
           ((char=? (current-char lex) #\\)
            (advance! lex)
            (cond
              ((at-end? lex) (display "\\" buf))
              ;; Line continuation: backslash-newline is consumed silently
              ((char=? (current-char lex) #\newline)
               (advance! lex)
               (loop depth))
              (else
               (display "\\" buf)
               (display (current-char lex) buf)
               (advance! lex)
               (loop depth))))
           ;; Single quotes: read verbatim until closing quote
           ((char=? (current-char lex) #\')
            (display "'" buf) (advance! lex)
            (let inner ()
              (cond
                ((at-end? lex) (set! (lexer-want-more? lex) #t))
                ((char=? (current-char lex) #\')
                 (display "'" buf) (advance! lex))
                (else (display (current-char lex) buf) (advance! lex) (inner))))
            (loop depth))
           ;; Double quotes: read until closing, handling backslash escapes
           ((char=? (current-char lex) #\")
            (display "\"" buf) (advance! lex)
            (let inner ()
              (cond
                ((at-end? lex) (set! (lexer-want-more? lex) #t))
                ((char=? (current-char lex) #\")
                 (display "\"" buf) (advance! lex))
                ((char=? (current-char lex) #\\)
                 (display "\\" buf) (advance! lex)
                 (unless (at-end? lex)
                   (display (current-char lex) buf) (advance! lex))
                 (inner))
                (else (display (current-char lex) buf) (advance! lex) (inner))))
            (loop depth))
           ;; Nested $... expressions
           ((char=? (current-char lex) #\$)
            (display (read-dollar! lex) buf)
            (loop depth))
           ;; Backtick command substitution
           ((char=? (current-char lex) #\`)
            (display (read-backtick! lex) buf)
            (loop depth))
           (else
            (display (current-char lex) buf) (advance! lex) (loop depth))))
       (get-output-string buf))
      ;; $name or $special
      ((or (char-alphabetic? (current-char lex))
           (char=? (current-char lex) #\_))
       (let loop ()
         (if (and (not (at-end? lex))
                  (let ((ch (current-char lex)))
                    (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_))))
           (begin (display (current-char lex) buf) (advance! lex) (loop))
           (get-output-string buf))))
      ;; $? $$ $! $# $0-$9 $* $@ $-
      ((memq (current-char lex) '(#\? #\$ #\! #\# #\* #\@ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
       (display (current-char lex) buf)
       (advance! lex)
       (get-output-string buf))
      (else
       (get-output-string buf)))))

;;; --- Backtick command substitution ---

(def (read-backtick! lex)
  (let ((buf (open-output-string)))
    (display "`" buf)
    (advance! lex)  ;; skip `
    (let loop ()
      (cond
        ((at-end? lex)
         (set! (lexer-want-more? lex) #t)
         (get-output-string buf))
        ((char=? (current-char lex) #\`)
         (display "`" buf)
         (advance! lex)
         (get-output-string buf))
        ((char=? (current-char lex) #\\)
         (display "\\" buf)
         (advance! lex)
         (unless (at-end? lex)
           (display (current-char lex) buf)
           (advance! lex))
         (loop))
        (else
         (display (current-char lex) buf)
         (advance! lex)
         (loop))))))

;;; --- Here-documents ---

(def (collect-heredocs! lex)
  ;; Collect heredoc bodies after a newline, in registration order
  (let loop ((specs (reverse (lexer-heredocs lex)))
             (acc []))
    (if (pair? specs)
      (let* ((spec (car specs))
             (delimiter (car spec))
             (strip-tabs? (cadr spec))
             (body (collect-heredoc-body! lex delimiter strip-tabs?)))
        (loop (cdr specs)
              (cons (make-token 'HEREDOC_BODY body
                               (cons (lexer-line lex) (lexer-col lex)))
                    acc)))
      ;; acc is reversed (last processed first), reverse for correct order
      (set! (lexer-tokens lex)
        (append (reverse acc) (lexer-tokens lex)))))
  (set! (lexer-heredocs lex) []))

(def (collect-heredoc-body! lex delimiter strip-tabs?)
  (let ((buf (open-output-string)))
    (let loop ()
      (if (at-end? lex)
        (begin
          (set! (lexer-want-more? lex) #t)
          (get-output-string buf))
        ;; Read a line
        (let ((line (read-line-from-lexer! lex)))
          (let ((trimmed (if strip-tabs? (string-trim-tabs line) line)))
            (if (string=? trimmed delimiter)
              (get-output-string buf)
              (begin
                (display trimmed buf)
                (display "\n" buf)
                (loop)))))))))

(def (read-line-from-lexer! lex)
  (let ((buf (open-output-string)))
    (let loop ()
      (cond
        ((at-end? lex) (get-output-string buf))
        ((char=? (current-char lex) #\newline)
         (advance! lex)
         (get-output-string buf))
        (else
         (display (current-char lex) buf)
         (advance! lex)
         (loop))))))

(def (string-trim-tabs str)
  (let loop ((i 0))
    (if (and (< i (string-length str))
             (char=? (string-ref str i) #\tab))
      (loop (+ i 1))
      (substring str i (string-length str)))))

;;; --- Token classification helpers ---

(def (all-digits? str)
  (and (> (string-length str) 0)
       (let loop ((i 0))
         (or (>= i (string-length str))
             (and (char-numeric? (string-ref str i))
                  (loop (+ i 1)))))))

(def (assignment-word? word)
  ;; Check for NAME=value, NAME+=value, NAME[expr]=value, NAME[expr]+=value pattern
  (let ((eq-pos (string-find-char word #\=)))
    (and eq-pos
         (> eq-pos 0)
         (let ((name-part (if (and (> eq-pos 1)
                                   (char=? (string-ref word (- eq-pos 1)) #\+))
                           (substring word 0 (- eq-pos 1))
                           (substring word 0 eq-pos))))
           ;; Check for NAME or NAME[expr]
           (let ((bracket-pos (string-find-char-in-name name-part #\[)))
             (if bracket-pos
               ;; NAME[expr] — validate name before bracket
               (and (> bracket-pos 0)
                    (valid-shell-name? (substring name-part 0 bracket-pos))
                    ;; Must have closing ]
                    (let ((close (string-find-char-in-name name-part #\])))
                      (and close (= close (- (string-length name-part) 1)))))
               ;; Plain NAME
               (valid-shell-name? name-part)))))))

(def (string-find-char-in-name str ch)
  (let loop ((i 0))
    (cond ((>= i (string-length str)) #f)
          ((char=? (string-ref str i) ch) i)
          (else (loop (+ i 1))))))

(def (valid-shell-name? str)
  (and (> (string-length str) 0)
       (let ((ch (string-ref str 0)))
         (or (char-alphabetic? ch) (char=? ch #\_)))
       (let loop ((i 1))
         (or (>= i (string-length str))
             (let ((ch (string-ref str i)))
               (and (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_))
                    (loop (+ i 1))))))))

(def (string-find-char str ch)
  (let loop ((i 0))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) ch) i)
      (else (loop (+ i 1))))))

;;; --- Hex/octal digit reading ---

;; Returns (values val count) — val is the accumulated value, count is digits read
(def (read-hex-digits lex max-count)
  (let loop ((count 0) (val 0))
    (if (or (>= count max-count) (at-end? lex)
            (not (hex-char? (current-char lex))))
      (values val count)
      (let ((digit (hex-val (current-char lex))))
        (advance! lex)
        (loop (+ count 1) (+ (* val 16) digit))))))

(def (read-octal-digits lex max-count)
  (read-octal-digits-with-init lex max-count 0))

(def (read-octal-digits-with-init lex max-count init)
  (let loop ((count 0) (val init))
    (if (or (>= count max-count) (at-end? lex)
            (not (octal-char? (current-char lex))))
      val
      (let ((digit (- (char->integer (current-char lex)) (char->integer #\0))))
        (advance! lex)
        (loop (+ count 1) (+ (* val 8) digit))))))

(def (hex-val ch)
  (cond
    ((and (char>=? ch #\0) (char<=? ch #\9))
     (- (char->integer ch) (char->integer #\0)))
    ((and (char>=? ch #\a) (char<=? ch #\f))
     (+ 10 (- (char->integer ch) (char->integer #\a))))
    ((and (char>=? ch #\A) (char<=? ch #\F))
     (+ 10 (- (char->integer ch) (char->integer #\A))))
    (else 0)))

(def (octal-char? ch)
  (and (char>=? ch #\0) (char<=? ch #\7)))

;;; --- Reserved words (for parser use) ---

(def *reserved-words*
  '("if" "then" "elif" "else" "fi"
    "do" "done" "case" "esac"
    "while" "until" "for" "in"
    "select" "function" "time" "coproc"
    "{" "}" "!" "[[" "]]"))

(def (reserved-word? word)
  (member word *reserved-words*))
