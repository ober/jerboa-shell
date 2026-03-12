;;; parser.ss — Recursive-descent AST builder for gsh
;;; Consumes tokens from the lexer and produces AST nodes.

(export #t)
(import :std/sugar
        :std/format
        :gsh/ast
        :gsh/lexer)

;;; --- Parser state ---

(defstruct parser-state
  (lexer         ;; lexer instance
   peeked        ;; peeked token or #f
   needs-more?   ;; #t if input is incomplete
   heredoc-queue ;; list of heredoc tokens waiting to be consumed
   alias-fn      ;; (word -> string-or-#f) alias lookup, or #f to disable
   )
  transparent: #t)

;;; --- Public interface ---

;; Parse a complete command from input string
;; Returns an AST node (command-list, and-or-list, etc.) or #f for empty input
(def (parse-complete-command input (extglob? #f) (alias-fn #f))
  (let* ((lex (if (string? input) (make-shell-lexer input extglob?) input))
         (ps (make-parser-state lex #f #f [] alias-fn)))
    (let ((result (parse-list ps)))
      (when (lexer-needs-more? lex)
        (set! (parser-state-needs-more? ps) #t))
      ;; Check for unconsumed tokens — syntax error if non-NEWLINE/EOF remain
      (let ((tok (parser-peek ps)))
        (when (and (token? tok) (not (eq? (token-type tok) 'NEWLINE)))
          (error (string-append "parse error near `"
                                (if (token-value tok) (token-value tok)
                                    (symbol->string (token-type tok)))
                                "'"))))
      result)))

;; Parse one "line" — semicolon-separated commands, stopping at newline.
;; This allows execution between lines so shopt changes take effect.
(def (parse-one-line input (extglob? #f) (alias-fn #f))
  (let* ((lex (if (string? input) (make-shell-lexer input extglob?) input))
         (ps (make-parser-state lex #f #f [] alias-fn)))
    (skip-newlines! ps)
    (let ((first (parse-and-or ps)))
      (if (not first)
        (begin
          (when (lexer-needs-more? lex)
            (set! (parser-state-needs-more? ps) #t))
          ;; If there are unconsumed tokens (not newline/EOF), syntax error
          (let ((tok (parser-peek ps)))
            (when (and (token? tok)
                       (not (eq? (token-type tok) 'NEWLINE)))
              (error (string-append "parse error near `"
                                    (if (token-value tok) (token-value tok)
                                        (symbol->string (token-type tok)))
                                    "'"))))
          #f)
        (let loop ((items [(cons 'sequential first)]))
          (cond
            ;; Semicolon — continue on same line
            ((parser-check? ps 'SEMI)
             (parser-consume! ps)
             (let ((next (parse-and-or ps)))
               (if next
                 (loop (cons (cons 'sequential next) items))
                 ;; No more commands after ; — check for remaining tokens
                 ;; that indicate a syntax error (e.g. "do", "done" at top level)
                 (begin
                   (let ((tok (parser-peek ps)))
                     (when (and (token? tok)
                                (not (eq? (token-type tok) 'NEWLINE)))
                       (error (string-append "syntax error near unexpected token `"
                                             (if (token-value tok) (token-value tok)
                                                 (symbol->string (token-type tok)))
                                             "'"))))
                   (if (= (length items) 1) (cdar items)
                       (make-command-list (reverse items)))))))
            ;; Ampersand — background (always wrap in command-list to preserve mode)
            ((parser-check? ps 'AMP)
             (parser-consume! ps)
             ;; Consume any pending heredoc bodies before returning
             (when (pair? (parser-state-heredoc-queue ps))
               (consume-heredoc-bodies! ps))
             (let ((updated (cons (cons 'background (cdar items)) (cdr items))))
               (make-command-list (reverse updated))))
            ;; Newline — stop, let caller execute before parsing next line
            ((parser-check? ps 'NEWLINE)
             (parser-consume! ps)
             ;; Consume any pending heredoc bodies before returning
             (when (pair? (parser-state-heredoc-queue ps))
               (consume-heredoc-bodies! ps))
             (if (= (length items) 1) (cdar items)
                 (make-command-list (reverse items))))
            (else
             ;; Consume any pending heredoc bodies before returning
             (when (pair? (parser-state-heredoc-queue ps))
               (consume-heredoc-bodies! ps))
             (when (lexer-needs-more? lex)
               (set! (parser-state-needs-more? ps) #t))
             (if (= (length items) 1) (cdar items)
                 (make-command-list (reverse items))))))))))

;; Check if parser needs more input (incomplete command)
(def (parser-needs-more? ps)
  (parser-state-needs-more? ps))

;;; --- Token access ---

(def (parser-peek ps)
  (unless (parser-state-peeked ps)
    (set! (parser-state-peeked ps) (lexer-next! (parser-state-lexer ps))))
  (parser-state-peeked ps))

(def (parser-next! ps)
  (if (parser-state-peeked ps)
    (let ((tok (parser-state-peeked ps)))
      (set! (parser-state-peeked ps) #f)
      tok)
    (lexer-next! (parser-state-lexer ps))))

(def (parser-consume! ps)
  (parser-next! ps))

;; Check if next token is a specific type
(def (parser-check? ps type)
  (let ((tok (parser-peek ps)))
    (and (token? tok) (eq? (token-type tok) type))))

;; Check if next token is a word with specific value
(def (parser-check-word? ps word)
  (let ((tok (parser-peek ps)))
    (and (token? tok)
         (eq? (token-type tok) 'WORD)
         (string=? (token-value tok) word))))

;; Expect a specific word, error if not found
(def (parser-expect-word! ps word)
  (let ((tok (parser-next! ps)))
    (unless (and (token? tok)
                 (eq? (token-type tok) 'WORD)
                 (string=? (token-value tok) word))
      (error (format "parse error: expected '~a', got ~a"
                     word (if (token? tok) (token-value tok) tok))))))

;; Skip newline tokens
(def (skip-newlines! ps)
  (let loop ()
    (cond
      ((parser-check? ps 'NEWLINE)
       (parser-consume! ps)
       (loop))
      ((and (pair? (parser-state-heredoc-queue ps))
            (parser-check? ps 'HEREDOC_BODY))
       (consume-heredoc-bodies! ps)
       (loop))
      (else (void)))))

;; After a newline triggers heredoc body collection, consume the
;; HEREDOC_BODY tokens and patch the pending redirect targets.
(def (consume-heredoc-bodies! ps)
  (let loop ((queue (parser-state-heredoc-queue ps)))
    (when (pair? queue)
      (let ((redir (car queue)))
        (when (parser-check? ps 'HEREDOC_BODY)
          (let ((tok (parser-next! ps)))
            (set! (redir-target redir) (token-value tok)))))
      (loop (cdr queue))))
  (set! (parser-state-heredoc-queue ps) []))

;; Check if at end of input or separator
(def (at-end-or-sep? ps)
  (let ((tok (parser-peek ps)))
    (or (eq? tok 'eof)
        (not (token? tok)))))

;;; --- Grammar productions ---

;; list : and_or ((';' | '&') and_or)* [';' | '&']
(def (parse-list ps)
  (skip-newlines! ps)
  (let ((first (parse-and-or ps)))
    (if (not first)
      #f
      (let loop ((items [(cons 'sequential first)]))
        (let ((tok (parser-peek ps)))
          (cond
            ;; Semicolon — sequential
            ((parser-check? ps 'SEMI)
             (parser-consume! ps)
             (skip-newlines! ps)
             (let ((next (parse-and-or ps)))
               (if next
                 (loop (cons (cons 'sequential next) items))
                 (make-command-list (reverse items)))))
            ;; Ampersand — background
            ((parser-check? ps 'AMP)
             (parser-consume! ps)
             ;; Mark the last item as background
             (let ((updated (cons (cons 'background (cdar items)) (cdr items))))
               (skip-newlines! ps)
               (let ((next (parse-and-or ps)))
                 (if next
                   (loop (cons (cons 'sequential next) updated))
                   (make-command-list (reverse updated))))))
            ;; Newline — continue looking
            ((parser-check? ps 'NEWLINE)
             (parser-consume! ps)
             (skip-newlines! ps)
             (let ((next (parse-and-or ps)))
               (if next
                 (loop (cons (cons 'sequential next) items))
                 (make-command-list (reverse items)))))
            (else
             (if (= (length items) 1)
               ;; Single command — unwrap
               (cdar items)
               (make-command-list (reverse items))))))))))

;; and_or : pipeline (('&&' | '||') newline* pipeline)*
(def (parse-and-or ps)
  (let ((first (parse-pipeline ps)))
    (if (not first)
      #f
      (let loop ((rest []))
        (cond
          ((parser-check? ps 'AND_IF)
           (parser-consume! ps)
           (skip-newlines! ps)
           (let ((next (parse-pipeline ps)))
             (if next
               (loop (cons (cons 'and next) rest))
               (error "parse error: expected command after &&"))))
          ((parser-check? ps 'OR_IF)
           (parser-consume! ps)
           (skip-newlines! ps)
           (let ((next (parse-pipeline ps)))
             (if next
               (loop (cons (cons 'or next) rest))
               (error "parse error: expected command after ||"))))
          (else
           (if (null? rest)
             first
             (make-and-or-list first (reverse rest)))))))))

;; pipeline : ['time' ['-p']] ['!'] command ('|' newline* command)*
(def (parse-pipeline ps)
  ;; Check for 'time' keyword before the pipeline
  (let* ((time? (and (parser-check-word? ps "time")
                     (begin (parser-consume! ps) #t)))
         (posix-time? (and time?
                           (parser-check-word? ps "-p")
                           (begin (parser-consume! ps) #t)))
         (bang? (and (or (parser-check-word? ps "!")
                         (parser-check? ps 'BANG))
                     (begin (parser-consume! ps) #t)))
         (first (parse-command ps)))
    (if (not first)
      (cond
        (bang? (error "parse error: expected command after !"))
        ;; Bare 'time' with no command — return time-command with #f pipeline
        (time? (make-time-command posix-time? #f))
        (else #f))
      (let loop ((cmds [first]) (ptypes []))
        (cond
          ((or (parser-check? ps 'PIPE) (parser-check? ps 'PIPEAMP))
           (let ((pipe-type (token-type (parser-next! ps))))
             (skip-newlines! ps)
             (let ((next (parse-command ps)))
               (if next
                 (loop (cons next cmds) (cons pipe-type ptypes))
                 (error "parse error: expected command after |")))))
          (else
           (let* ((commands (reverse cmds))
                  (pipe-types (reverse ptypes))
                  (pipeline (if (and (= (length commands) 1) (not bang?))
                              (car commands)
                              (make-ast-pipeline commands bang? pipe-types))))
             (if time?
               (make-time-command posix-time? pipeline)
               pipeline))))))))

;; command : compound_command redirect*
;;         | function_def
;;         | simple_command
(def (parse-command ps)
  (let command-loop ()
    (let ((tok (parser-peek ps)))
      (cond
        ((not (token? tok)) #f)
        ((eq? tok 'eof) #f)
        ;; Compound commands
        ((parser-check-word? ps "{")
         (parse-brace-group ps))
        ((parser-check? ps 'LPAREN)
         ;; Consume ( and check for (( = arithmetic command
         (parser-consume! ps)
         (if (parser-check? ps 'LPAREN)
           (let ((cmd (parse-arith-command ps))
                 (redirs (parse-redirect-list ps)))
             (if (pair? redirs)
               (make-redirected-command cmd redirs)
               cmd))
           ;; Regular subshell — ( already consumed, parse body directly
           (let ((body (parse-list ps)))
             (skip-newlines! ps)
             (unless (parser-check? ps 'RPAREN)
               (error "parse error: expected ')'"))
             (parser-consume! ps)
             (let ((redirs (parse-redirect-list ps)))
               (if (pair? redirs)
                 (make-redirected-command (make-subshell body) redirs)
                 (make-subshell body))))))
        ((parser-check-word? ps "if")
         (parse-if ps))
        ((parser-check-word? ps "while")
         (parse-while ps))
        ((parser-check-word? ps "until")
         (parse-until ps))
        ((parser-check-word? ps "for")
         (parse-for ps))
        ((parser-check-word? ps "case")
         (parse-case ps))
        ((parser-check-word? ps "select")
         (parse-select ps))
        ((parser-check-word? ps "[[")
         (let ((cmd (parse-cond-command ps))
               (redirs (parse-redirect-list ps)))
           (if (pair? redirs)
             (make-redirected-command cmd redirs)
             cmd)))
        ((parser-check-word? ps "function")
         (parse-function-def-keyword ps))
        ((parser-check-word? ps "coproc")
         (parse-coproc ps))
        ;; Check for function def: word ( )
        ;; (handled in simple-command when we see WORD LPAREN RPAREN)
        (else
         (let ((result (parse-simple-command ps)))
           ;; If alias expansion changed the token stream and needs
           ;; re-dispatch (e.g. alias expanded to a reserved word),
           ;; loop back to check the new first token
           (if (eq? result 'alias-restart)
             (command-loop)
             result)))))))

;;; --- Alias helpers (inlined to avoid circular import with functions.ss) ---

;; Check if alias value ends with space (triggers next-word alias expansion)
(def (alias-value-continues? value)
  (and (string? value)
       (> (string-length value) 0)
       (char=? (string-ref value (- (string-length value) 1)) #\space)))

;;; --- Simple command ---

;; simple_command : cmd_prefix? WORD cmd_suffix?
;; cmd_prefix : (ASSIGNMENT_WORD | redirect)+
;; cmd_suffix : (WORD | redirect)+
(def (parse-simple-command ps)
  (let/cc return  ;; early return for function definitions and alias restart
  (let ((assignments [])
        (words [])
        (redirections [])
        ;; Alias expansion state
        (alias-fn (parser-state-alias-fn ps))
        (expanded-aliases (make-hash-table))  ;; cycle detection
        (check-next-for-alias #f)   ;; trailing-space next-word expansion
        (alias-text-end #f))        ;; lexer boundary for trailing-space tracking
    ;; Parse prefix: assignments and redirections before command word
    (let prefix-loop ()
      (let ((tok (parser-peek ps)))
        (cond
          ((parser-check? ps 'ASSIGNMENT_WORD)
           (let* ((tok (parser-next! ps))
                  (asgn (parse-assignment-token tok)))
             ;; Check for compound array assignment: NAME=( ... )
             (if (and (string=? (assignment-value asgn) "")
                      (not (assignment-index asgn))
                      (parser-check? ps 'LPAREN))
               ;; Compound assignment: NAME=(val1 val2 ...)
               (begin
                 (parser-consume! ps) ;; skip (
                 (let ((values (parse-compound-array-values ps)))
                   (set! assignments
                     (cons (make-assignment (assignment-name asgn) #f values
                                           (assignment-op asgn))
                           assignments))
                   (prefix-loop)))
               ;; Regular assignment
               (begin
                 (set! assignments (cons asgn assignments))
                 (prefix-loop)))))
          ((redirect-token? tok)
           (set! redirections (cons (parse-redirect! ps) redirections))
           (prefix-loop))
          (else #!void))))
    ;; Parse command word and suffix
    (let ((tok (parser-peek ps)))
      (when (and (token? tok)
                 (or (eq? (token-type tok) 'WORD)
                     (eq? (token-type tok) 'IO_NUMBER)))
        ;; Check for function definition: name ( )
        ;; When prefix assignments exist, reserved words lose their special
        ;; meaning and become regular command words (bash behavior).
        ;; e.g. FOO=bar for → tries to run "for" as command (127)
        (when (and (eq? (token-type tok) 'WORD)
                   (or (pair? assignments)
                       (not (reserved-word? (token-value tok)))))
          ;; --- Alias expansion on first word ---
          (let ((word-tok (parser-next! ps)))
            (let alias-check ((wtok word-tok))
              (let* ((word-val (token-value wtok))
                     (alias-val (and alias-fn
                                     (not (hash-get expanded-aliases word-val))
                                     (alias-fn word-val))))
                (if alias-val
                  ;; Alias match — expand
                  (begin
                    (hash-put! expanded-aliases word-val #t)
                    (let ((lex (parser-state-lexer ps)))
                      ;; Prepend alias value text to lexer input
                      (lexer-prepend-text! lex alias-val)
                      ;; Track boundary for trailing-space next-word expansion
                      (set! alias-text-end (string-length alias-val))
                      (set! check-next-for-alias (alias-value-continues? alias-val))
                      ;; Clear parser peeked token
                      (set! (parser-state-peeked ps) #f)
                      ;; Check what the expansion starts with
                      (let ((new-tok (parser-peek ps)))
                        (cond
                          ;; Expansion starts with a reserved word (and no prefix assignments)
                          ;; → need to restart from parse-command level
                          ((and (token? new-tok)
                                (eq? (token-type new-tok) 'WORD)
                                (null? assignments)
                                (null? redirections)
                                (reserved-word? (token-value new-tok)))
                           (return 'alias-restart))
                          ;; Expansion starts with LPAREN — could be subshell or (( ))
                          ((and (token? new-tok)
                                (eq? (token-type new-tok) 'LPAREN)
                                (null? assignments)
                                (null? redirections))
                           (return 'alias-restart))
                          ;; Expansion starts with a regular WORD → recurse alias check
                          ((and (token? new-tok)
                                (eq? (token-type new-tok) 'WORD))
                           (alias-check (parser-next! ps)))
                          ;; Other token (operator, etc.) → restart from parse-command
                          ((and (token? new-tok)
                                (not (eq? (token-type new-tok) 'WORD))
                                (null? assignments)
                                (null? redirections))
                           (return 'alias-restart))
                          ;; Has prefix assignments/redirections — stay in simple command
                          (else
                           ;; Read new first word if it's a WORD
                           (when (and (token? new-tok) (eq? (token-type new-tok) 'WORD))
                             (set! words (cons (token-value (parser-next! ps)) words))))))))
                  ;; Not an alias — use this word as-is
                  (set! words (cons (token-value wtok) words)))))
            ;; Check for function def
            (when (and (pair? words) (parser-check? ps 'LPAREN))
              (parser-consume! ps)  ;; (
              (if (parser-check? ps 'RPAREN)
                (begin
                  (parser-consume! ps)  ;; )
                  ;; This is a function definition
                  (let ((func-line (lexer-line (parser-state-lexer ps))))
                    (skip-newlines! ps)
                    (let ((body (parse-command ps))
                          (redirs (parse-redirect-list ps)))
                      (return
                       (make-function-def
                        (car words)  ;; function name
                        body
                        redirs
                        func-line)))))  ;; line number
                ;; Bare ( after command word is a syntax error (bash behavior)
                (error (string-append "parse error near unexpected token `('"))))
            ;; Parse suffix: more words and redirections
            (let suffix-loop ()
              (let ((tok (parser-peek ps)))
                (cond
                  ;; --- Trailing-space alias expansion on next word ---
                  ((and check-next-for-alias alias-fn
                        (token? tok) (eq? (token-type tok) 'WORD)
                        (number? alias-text-end)
                        (>= (lexer-pos (parser-state-lexer ps)) alias-text-end))
                   ;; This word is the first one from the original input after alias text.
                   ;; Check it for alias expansion with FRESH cycle detection
                   ;; (each expansion position has its own cycle set, per bash behavior).
                   (set! check-next-for-alias #f)
                   (let expand-next-alias ((next-seen (make-hash-table)))
                     (let* ((tok2 (parser-peek ps)))
                       (if (not (and (token? tok2) (eq? (token-type tok2) 'WORD)))
                         (suffix-loop)  ;; not a word, continue normally
                         (let* ((next-tok (parser-next! ps))
                                (next-val (token-value next-tok))
                                (next-alias (and (not (hash-get next-seen next-val))
                                                 (alias-fn next-val))))
                           (if next-alias
                             (begin
                               (hash-put! next-seen next-val #t)
                               (let ((lex (parser-state-lexer ps)))
                                 (lexer-prepend-text! lex next-alias)
                                 ;; If this alias also ends with space, set up for next word
                                 (when (alias-value-continues? next-alias)
                                   (set! check-next-for-alias #t)
                                   (set! alias-text-end (string-length next-alias)))
                                 (set! (parser-state-peeked ps) #f)
                                 ;; Recursively check first word of this expansion
                                 (expand-next-alias next-seen)))
                             ;; Not an alias — add as regular word
                             (begin
                               (set! words (cons next-val words))
                               (suffix-loop))))))))
                  ;; ASSIGNMENT_WORD as argument to builtins like
                  ;; local, export, readonly, declare
                  ;; Check for compound array: NAME=( ... )
                  ((and (token? tok)
                        (eq? (token-type tok) 'ASSIGNMENT_WORD))
                   (let* ((t (parser-next! ps))
                          (asgn (parse-assignment-token t)))
                     (if (and (string=? (assignment-value asgn) "")
                              (not (assignment-index asgn))
                              (parser-check? ps 'LPAREN))
                       ;; Compound array assignment in suffix: arr=(val1 val2 ...)
                       ;; Encode as a string "name=(v1 v2 ...)" for declare to parse
                       (begin
                         (parser-consume! ps) ;; skip (
                         (let ((values (parse-compound-array-values ps)))
                           (let* ((val-strs (map (lambda (v)
                                                   (if (string? v) v ""))
                                                 values))
                                  (compound-str
                                   (string-append
                                    (assignment-name asgn) "=("
                                    (let loop ((vs val-strs) (acc ""))
                                      (if (null? vs) acc
                                          (loop (cdr vs)
                                                (if (string=? acc "")
                                                  (car vs)
                                                  (string-append acc " " (car vs))))))
                                    ")")))
                             (set! words (cons compound-str words))
                             (suffix-loop))))
                       ;; Regular assignment word
                       (begin
                         (set! words (cons (token-value t) words))
                         (suffix-loop)))))
                  ((and (token? tok)
                        (or (eq? (token-type tok) 'WORD)
                            ;; BANG (!) as argument — only special at pipeline
                            ;; start, not inside command arguments like [ ! -z x ]
                            (eq? (token-type tok) 'BANG)))
                   (set! words (cons (token-value (parser-next! ps)) words))
                   (suffix-loop))
                  ;; Process substitution <(cmd) or >(cmd)
                  ((and (token? tok)
                        (eq? (token-type tok) 'PROCSUB_IN))
                   (let ((t (parser-next! ps)))
                     (set! words (cons (make-word-process-sub 'in (token-value t)) words)))
                   (suffix-loop))
                  ((and (token? tok)
                        (eq? (token-type tok) 'PROCSUB_OUT))
                   (let ((t (parser-next! ps)))
                     (set! words (cons (make-word-process-sub 'out (token-value t)) words)))
                   (suffix-loop))
                  ((redirect-token? tok)
                   (set! redirections (cons (parse-redirect! ps) redirections))
                   (suffix-loop))
                  (else #!void))))))))
    ;; Build result
    (if (and (null? words) (null? assignments) (null? redirections))
      #f
      (make-simple-command
       (reverse assignments)
       (reverse words)
       (reverse redirections))))))

;; Parse an assignment token "NAME=VALUE", "NAME+=VALUE",
;; "NAME[idx]=VALUE", or "NAME[idx]+=VALUE"
(def (parse-assignment-token tok)
  (let* ((word (token-value tok))
         (eq-pos (string-find-eq word))
         (append? (and eq-pos (> eq-pos 0)
                       (char=? (string-ref word (- eq-pos 1)) #\+)))
         (name-end (if append? (- eq-pos 1) eq-pos))
         (name-part (substring word 0 name-end))
         (value (substring word (+ eq-pos 1) (string-length word)))
         (op (if append? '+= '=)))
    ;; Check for array index: NAME[expr]
    (let ((bracket-pos (string-find-bracket name-part)))
      (if bracket-pos
        ;; NAME[expr]=VALUE
        (let* ((name (substring name-part 0 bracket-pos))
               (close (string-find-close-bracket name-part (+ bracket-pos 1)))
               (index (if close
                        (substring name-part (+ bracket-pos 1) close)
                        "")))
          (make-assignment name index value op))
        ;; NAME=VALUE (no index)
        (make-assignment name-part #f value op)))))

(def (string-find-bracket str)
  (let loop ((i 0))
    (cond ((>= i (string-length str)) #f)
          ((char=? (string-ref str i) #\[) i)
          (else (loop (+ i 1))))))

(def (string-find-close-bracket str start)
  (let loop ((i start))
    (cond ((>= i (string-length str)) #f)
          ((char=? (string-ref str i) #\]) i)
          (else (loop (+ i 1))))))

(def (string-find-eq str)
  (let loop ((i 0))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) #\=) i)
      (else (loop (+ i 1))))))

;; Parse compound array values between ( and )
;; Returns a list of strings (words or [key]=val)
(def (parse-compound-array-values ps)
  (skip-newlines! ps)
  (let loop ((values []))
    (cond
      ((parser-check? ps 'RPAREN)
       (parser-consume! ps)
       (reverse values))
      ((parser-check? ps 'NEWLINE)
       (parser-consume! ps)
       (skip-newlines! ps)
       (loop values))
      ((or (parser-check? ps 'WORD)
           (parser-check? ps 'ASSIGNMENT_WORD))
       (let ((tok (parser-next! ps)))
         (loop (cons (token-value tok) values))))
      (else
       ;; Unexpected token — close the compound
       (reverse values)))))

;;; --- Redirections ---

;; Check if a heredoc delimiter contains quoting characters
(def (heredoc-delimiter-quoted? delim)
  (let ((len (string-length delim)))
    (let loop ((i 0))
      (if (>= i len)
        #f
        (let ((ch (string-ref delim i)))
          (if (or (char=? ch #\') (char=? ch #\"))
            #t
            (loop (+ i 1))))))))

;; Strip quote characters from a heredoc delimiter
(def (strip-heredoc-quotes delim)
  (let ((out (open-output-string))
        (len (string-length delim)))
    (let loop ((i 0))
      (if (>= i len)
        (get-output-string out)
        (let ((ch (string-ref delim i)))
          (if (or (char=? ch #\') (char=? ch #\"))
            (loop (+ i 1))
            (begin (display ch out)
                   (loop (+ i 1)))))))))

(def (redirect-token? tok)
  (and (token? tok)
       (memq (token-type tok)
             '(LESS GREAT DGREAT DLESS DLESSDASH TLESS
               LESSAND GREATAND LESSGREAT CLOBBER
               AMPGREAT AMPGREAT_GREAT IO_NUMBER IO_VARNAME))))

(def (parse-redirect! ps)
  (let ((fd #f)
        (fd-var #f))
    ;; Optional IO_NUMBER or IO_VARNAME prefix
    (cond
      ((parser-check? ps 'IO_NUMBER)
       (set! fd (string->number (token-value (parser-next! ps)))))
      ((parser-check? ps 'IO_VARNAME)
       (set! fd-var (token-value (parser-next! ps)))))
    (let ((op-tok (parser-next! ps)))
      (let ((op-type (token-type op-tok)))
        ;; Target word is required — syntax error if missing
        ;; Exception: {fd}>&- doesn't need a target beyond "-"
        (let* ((target-tok (parser-peek ps))
               (_ (when (or (not target-tok) (not (token? target-tok))
                            (memq (token-type target-tok) '(NEWLINE SEMI EOF)))
                    (error "parse error: redirect target missing")))
               (target-tok (parser-next! ps))
               (target (token-value target-tok))
               (r (make-redir
                   (case op-type
                     ((LESS) '<)
                     ((GREAT) '>)
                     ((DGREAT) '>>)
                     ((CLOBBER) 'clobber)
                     ((LESSAND) '<&)
                     ((GREATAND) '>&)
                     ((LESSGREAT) '<>)
                     ((DLESS) '<<)
                     ((DLESSDASH) '<<-)
                     ((TLESS) '<<<)
                     ((AMPGREAT) '&>)
                     ((AMPGREAT_GREAT) '&>>)
                     (else '>))
                   fd
                   target
                   fd-var)))
          ;; For heredocs, register spec with lexer for body collection
          (when (memq op-type '(DLESS DLESSDASH))
            (let* ((lex (parser-state-lexer ps))
                   (strip-tabs? (eq? op-type 'DLESSDASH))
                   (quoted? (heredoc-delimiter-quoted? target))
                   (bare-delim (if quoted?
                                 (strip-heredoc-quotes target)
                                 target)))
              ;; Register with lexer using the unquoted delimiter
              (set! (lexer-heredocs lex)
                (cons (list bare-delim strip-tabs? quoted?)
                      (lexer-heredocs lex)))
              ;; Mark quoted heredocs with different op (no expansion)
              (when quoted?
                (set! (redir-op r) (if strip-tabs? '<<-q '<<q)))
              (set! (parser-state-heredoc-queue ps)
                (append (parser-state-heredoc-queue ps) (list r)))))
          r)))))

(def (parse-redirect-list ps)
  (let loop ((redirs []))
    (if (redirect-token? (parser-peek ps))
      (loop (cons (parse-redirect! ps) redirs))
      (reverse redirs))))

;;; --- Compound commands ---

;; { list ; }
;; coproc [NAME] compound-command
;; coproc simple-command    (NAME defaults to COPROC)
;; With a simple command, no NAME is allowed.
;; With a compound command, an optional NAME can precede it.
;;
;; Strategy: parse what follows "coproc" as a normal command. If the result is
;; a simple-command with exactly one bare word and the next token starts a
;; compound command, treat that word as the NAME and parse the compound command
;; as the body. Otherwise, use the default name COPROC.
(def (parse-coproc ps)
  (parser-expect-word! ps "coproc")
  (skip-newlines! ps)
  ;; Parse the body (which might actually be a name followed by a compound command)
  (let ((first (parse-command ps)))
    ;; Check if "first" is really a name: simple-command with one bare word,
    ;; no assignments, no redirections, followed by a compound command
    (if (and (simple-command? first)
             (let ((words (simple-command-words first)))
               (and (pair? words) (null? (cdr words))
                    (string? (car words))))
             (null? (simple-command-assignments first))
             (null? (simple-command-redirections first))
             (coproc-compound-start? ps))
      ;; The single word was a name — parse the real body
      (let ((name (car (simple-command-words first)))
            (body (parse-command ps)))
        (make-coproc-command name body))
      ;; Use default name
      (make-coproc-command "COPROC" first))))

;; Check if the next token starts a compound command (for coproc name detection)
(def (coproc-compound-start? ps)
  (let ((tok (parser-peek ps)))
    (and (token? tok)
         (or (parser-check? ps 'LPAREN)
             (and (eq? (token-type tok) 'WORD)
                  (member (token-value tok) '("{" "while" "until" "if" "for")))))))

(def (parse-brace-group ps)
  (parser-expect-word! ps "{")
  (let ((body (parse-list ps)))
    (skip-newlines! ps)
    (parser-expect-word! ps "}")
    (let ((redirs (parse-redirect-list ps)))
      (if (pair? redirs)
        (make-redirected-command (make-brace-group body) redirs)
        (make-brace-group body)))))

;; ( list )
(def (parse-subshell ps)
  (parser-consume! ps)  ;; skip (
  (let ((body (parse-list ps)))
    (skip-newlines! ps)
    (unless (parser-check? ps 'RPAREN)
      (error "parse error: expected ')'"))
    (parser-consume! ps)
    (let ((redirs (parse-redirect-list ps)))
      (if (pair? redirs)
        (make-redirected-command (make-subshell body) redirs)
        (make-subshell body)))))

;; if list ; then list [elif list ; then list]* [else list] fi
(def (parse-if ps)
  (parser-expect-word! ps "if")
  (let loop ((clauses []))
    (let ((condition (parse-list ps)))
      (skip-newlines! ps)
      (parser-expect-word! ps "then")
      (let ((body (parse-list ps)))
        (skip-newlines! ps)
        (let ((new-clauses (cons (cons condition body) clauses)))
          (cond
            ((parser-check-word? ps "elif")
             (parser-consume! ps)
             (loop new-clauses))
            ((parser-check-word? ps "else")
             (parser-consume! ps)
             (let ((else-body (parse-list ps)))
               (skip-newlines! ps)
               (parser-expect-word! ps "fi")
               (let* ((cmd (make-if-command (reverse new-clauses) else-body))
                      (redirs (parse-redirect-list ps)))
                 (if (pair? redirs) (make-redirected-command cmd redirs) cmd))))
            (else
             (parser-expect-word! ps "fi")
             (let* ((cmd (make-if-command (reverse new-clauses) #f))
                    (redirs (parse-redirect-list ps)))
               (if (pair? redirs) (make-redirected-command cmd redirs) cmd)))))))))

;; while list ; do list ; done
(def (parse-while ps)
  (parser-expect-word! ps "while")
  (let ((test (parse-list ps)))
    (skip-newlines! ps)
    (parser-expect-word! ps "do")
    (let ((body (parse-list ps)))
      (skip-newlines! ps)
      (parser-expect-word! ps "done")
      (let* ((cmd (make-while-command test body))
             (redirs (parse-redirect-list ps)))
        (if (pair? redirs) (make-redirected-command cmd redirs) cmd)))))

;; until list ; do list ; done
(def (parse-until ps)
  (parser-expect-word! ps "until")
  (let ((test (parse-list ps)))
    (skip-newlines! ps)
    (parser-expect-word! ps "do")
    (let ((body (parse-list ps)))
      (skip-newlines! ps)
      (parser-expect-word! ps "done")
      (let* ((cmd (make-until-command test body))
             (redirs (parse-redirect-list ps)))
        (if (pair? redirs) (make-redirected-command cmd redirs) cmd)))))

;; for name [in word ...] ; do list ; done
;; for (( init; test; update )) ; do list ; done
(def (parse-for ps)
  (parser-expect-word! ps "for")
  ;; Check for (( = arithmetic for-loop
  (if (parser-check? ps 'LPAREN)
    (let ((saved (parser-next! ps)))
      (if (parser-check? ps 'LPAREN)
        (parse-arith-for ps)
        (begin
          (set! (parser-state-peeked ps) saved)
          (parse-for-in ps))))
    (parse-for-in ps)))

;; Regular for-in loop
(def (parse-for-in ps)
  (let ((name-tok (parser-next! ps)))
    (unless (and (token? name-tok) (eq? (token-type name-tok) 'WORD))
      (error "parse error: expected variable name after 'for'"))
    (let ((var-name (token-value name-tok)))
      (skip-newlines! ps)
      ;; Optional "in word..."
      (let ((words
             (if (parser-check-word? ps "in")
               (begin
                 (parser-consume! ps)
                 (let loop ((words []))
                   (let ((tok (parser-peek ps)))
                     (cond
                       ((or (parser-check? ps 'SEMI) (parser-check? ps 'NEWLINE))
                        (parser-consume! ps)
                        (reverse words))
                       ((and (token? tok) (eq? (token-type tok) 'WORD)
                             (not (string=? (token-value tok) "do")))
                        (loop (cons (token-value (parser-next! ps)) words)))
                       (else (reverse words))))))
               ;; No "in" clause — iterate over "$@"
               (begin
                 (when (parser-check? ps 'SEMI) (parser-consume! ps))
                 #f))))
        (skip-newlines! ps)
        (parser-expect-word! ps "do")
        (let ((body (parse-list ps)))
          (skip-newlines! ps)
          (parser-expect-word! ps "done")
          (let* ((cmd (make-for-command var-name words body))
                 (redirs (parse-redirect-list ps)))
            (if (pair? redirs) (make-redirected-command cmd redirs) cmd)))))))

;; (( expr )) — arithmetic command (returns 0 if non-zero, 1 if zero)
;; Called after consuming both opening LPAREN tokens
(def (parse-arith-command ps)
  (parser-consume! ps)  ;; skip second (
  ;; Read tokens until we see )) — collect as raw text
  (let ((expr (read-arith-body ps)))
    (make-arith-command expr)))

;; for (( init; test; update )) ; do list ; done
;; Called after consuming "for" and both LPAREN tokens
(def (parse-arith-for ps)
  (parser-consume! ps)  ;; skip second (
  ;; Read three semicolon-separated expressions until ))
  (let* ((init-expr (read-arith-until-semi ps))
         (test-expr (read-arith-until-semi ps))
         (update-expr (read-arith-until-close ps)))
    ;; Skip optional ; or newlines before do or {
    (when (parser-check? ps 'SEMI) (parser-consume! ps))
    (skip-newlines! ps)
    ;; Accept both do..done and { .. } syntax
    (if (parser-check-word? ps "{")
      (begin
        (parser-consume! ps)  ;; skip {
        (let ((body (parse-list ps)))
          (skip-newlines! ps)
          (parser-expect-word! ps "}")  ;; expect }
          (make-arith-for-command init-expr test-expr update-expr body)))
      (begin
        (parser-expect-word! ps "do")
        (let ((body (parse-list ps)))
          (skip-newlines! ps)
          (parser-expect-word! ps "done")
          (make-arith-for-command init-expr test-expr update-expr body))))))

;; Read raw characters from lexer input, bypassing shell tokenization.
;; Arithmetic expressions contain <, >, |, & which the shell lexer
;; would incorrectly treat as operators.

;; Helper: get current char from lexer, or #f at end
(def (lexer-raw-char lex)
  (if (>= (lexer-pos lex) (lexer-len lex))
    #f
    (string-ref (lexer-input lex) (lexer-pos lex))))

;; Helper: advance lexer position by 1
(def (lexer-raw-advance! lex)
  (set! (lexer-pos lex) (+ 1 (lexer-pos lex))))

;; Helper: clear any peeked token from both parser and lexer
;; (since we're reading raw, any peeked token is stale)
(def (parser-clear-peeked! ps)
  (set! (parser-state-peeked ps) #f)
  (set! (lexer-peeked (parser-state-lexer ps)) #f))

;; Read arithmetic body until )) — returns expression string
;; Reads raw characters from lexer input, tracking nested parens.
(def (read-arith-body ps)
  (parser-clear-peeked! ps)
  (let ((lex (parser-state-lexer ps))
        (buf (open-output-string)))
    (let loop ((depth 0))
      (let ((ch (lexer-raw-char lex)))
        (cond
          ((not ch)
           (error "parse error: expected '))'"))
          ((char=? ch #\()
           (lexer-raw-advance! lex)
           (display ch buf)
           (loop (+ depth 1)))
          ((char=? ch #\))
           (if (> depth 0)
             (begin (lexer-raw-advance! lex)
                    (display ch buf)
                    (loop (- depth 1)))
             ;; depth=0: check for second )
             (begin
               (lexer-raw-advance! lex)
               (let ((ch2 (lexer-raw-char lex)))
                 (if (and ch2 (char=? ch2 #\)))
                   (begin (lexer-raw-advance! lex)
                          (string-trim-ws (get-output-string buf)))
                   (begin (display ")" buf)
                          (loop depth)))))))
          (else
           (lexer-raw-advance! lex)
           (display ch buf)
           (loop depth)))))))

;; Read arithmetic expression until ; inside (( )) for for-loops
(def (read-arith-until-semi ps)
  (parser-clear-peeked! ps)
  (let ((lex (parser-state-lexer ps))
        (buf (open-output-string)))
    (let loop ((depth 0))
      (let ((ch (lexer-raw-char lex)))
        (cond
          ((not ch)
           (error "parse error: expected ';' in for (( ))"))
          ((char=? ch #\()
           (lexer-raw-advance! lex)
           (display ch buf)
           (loop (+ depth 1)))
          ((char=? ch #\))
           (lexer-raw-advance! lex)
           (display ch buf)
           (loop (if (> depth 0) (- depth 1) depth)))
          ((and (char=? ch #\;) (= depth 0))
           (lexer-raw-advance! lex)
           (string-trim-ws (get-output-string buf)))
          (else
           (lexer-raw-advance! lex)
           (display ch buf)
           (loop depth)))))))

;; Read arithmetic expression until )) — handles the close of for (( ))
(def (read-arith-until-close ps)
  (parser-clear-peeked! ps)
  (let ((lex (parser-state-lexer ps))
        (buf (open-output-string)))
    (let loop ((depth 0))
      (let ((ch (lexer-raw-char lex)))
        (cond
          ((not ch)
           (error "parse error: expected '))' in for (( ))"))
          ((char=? ch #\()
           (lexer-raw-advance! lex)
           (display ch buf)
           (loop (+ depth 1)))
          ((char=? ch #\))
           (if (> depth 0)
             (begin (lexer-raw-advance! lex)
                    (display ch buf)
                    (loop (- depth 1)))
             ;; depth=0: check for ))
             (begin
               (lexer-raw-advance! lex)
               (let ((ch2 (lexer-raw-char lex)))
                 (if (and ch2 (char=? ch2 #\)))
                   (begin (lexer-raw-advance! lex)
                          (string-trim-ws (get-output-string buf)))
                   (begin (display ")" buf)
                          (loop depth)))))))
          (else
           (lexer-raw-advance! lex)
           (display ch buf)
           (loop depth)))))))

(def (string-trim-ws str)
  (let* ((len (string-length str))
         (end (let loop ((i (- len 1)))
                (if (and (>= i 0) (char-whitespace? (string-ref str i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring str 0 end)))

;; case word in [[(] pattern [| pattern]* ) list ;;]* esac
(def (parse-case ps)
  (parser-expect-word! ps "case")
  (let ((word-tok (parser-next! ps)))
    (unless (and (token? word-tok) (eq? (token-type word-tok) 'WORD))
      (error "parse error: expected word after 'case'"))
    (let ((word (token-value word-tok)))
      (skip-newlines! ps)
      (parser-expect-word! ps "in")
      (skip-newlines! ps)
      (let loop ((clauses []))
        (cond
          ((parser-check-word? ps "esac")
           (parser-consume! ps)
           (let* ((cmd (make-case-command word (reverse clauses)))
                  (redirs (parse-redirect-list ps)))
             (if (pair? redirs) (make-redirected-command cmd redirs) cmd)))
          (else
           ;; Parse pattern list
           (when (parser-check? ps 'LPAREN) (parser-consume! ps))  ;; optional (
           (let pattern-loop ((patterns []))
             (let ((pat-tok (parser-next! ps)))
               (unless (and (token? pat-tok) (eq? (token-type pat-tok) 'WORD))
                 (error "parse error: expected pattern in case clause"))
               (let ((new-patterns (cons (token-value pat-tok) patterns)))
                 (if (parser-check? ps 'PIPE)
                   (begin (parser-consume! ps)
                          (pattern-loop new-patterns))
                   ;; Expect )
                   (begin
                     (unless (parser-check? ps 'RPAREN)
                       (error "parse error: expected ')' in case clause"))
                     (parser-consume! ps)
                     (skip-newlines! ps)
                     (let ((body (parse-list ps)))
                       (skip-newlines! ps)
                       ;; Expect ;; or ;& or ;;&
                       (let ((terminator
                              (cond
                                ((parser-check? ps 'DSEMI_AMP)
                                 (parser-consume! ps) 'test-next)
                                ((parser-check? ps 'SEMI_AMP)
                                 (parser-consume! ps) 'fallthrough)
                                ((parser-check? ps 'DSEMI)
                                 (parser-consume! ps) 'break)
                                (else 'break))))
                         (skip-newlines! ps)
                         (loop (cons (make-case-clause
                                      (reverse new-patterns)
                                      body
                                      terminator)
                                     clauses)))))))))))))))

;; select name in word ... ; do list ; done
(def (parse-select ps)
  (parser-expect-word! ps "select")
  (let ((name-tok (parser-next! ps)))
    (unless (and (token? name-tok) (eq? (token-type name-tok) 'WORD))
      (error "parse error: expected variable name after 'select'"))
    (let ((var-name (token-value name-tok)))
      (skip-newlines! ps)
      (parser-expect-word! ps "in")
      (let loop ((words []))
        (let ((tok (parser-peek ps)))
          (cond
            ((or (parser-check? ps 'SEMI) (parser-check? ps 'NEWLINE))
             (parser-consume! ps)
             (skip-newlines! ps)
             (parser-expect-word! ps "do")
             (let ((body (parse-list ps)))
               (skip-newlines! ps)
               (parser-expect-word! ps "done")
               (make-select-command var-name (reverse words) body)))
            ((and (token? tok) (eq? (token-type tok) 'WORD)
                  (not (string=? (token-value tok) "do")))
             (loop (cons (token-value (parser-next! ps)) words)))
            (else
             (parser-expect-word! ps "do")
             (let ((body (parse-list ps)))
               (skip-newlines! ps)
               (parser-expect-word! ps "done")
               (make-select-command var-name (reverse words) body)))))))))

;; function name { body }
(def (parse-function-def-keyword ps)
  (parser-consume! ps)  ;; skip "function"
  (let ((name-tok (parser-next! ps)))
    (unless (and (token? name-tok) (eq? (token-type name-tok) 'WORD))
      (error "parse error: expected function name"))
    ;; Optional ()
    (when (parser-check? ps 'LPAREN)
      (parser-consume! ps)
      (unless (parser-check? ps 'RPAREN)
        (error "parse error: expected ')' after '(' in function definition"))
      (parser-consume! ps))
    (let ((func-line (lexer-line (parser-state-lexer ps))))
      (skip-newlines! ps)
      (let ((body (parse-command ps))
            (redirs (parse-redirect-list ps)))
        (make-function-def (token-value name-tok) body redirs func-line)))))

;;; --- [[ ]] conditional command ---

;; [[ expr ]]
;; Grammar (precedence low to high):
;;   or_expr   : and_expr ('||' and_expr)*
;;   and_expr  : not_expr ('&&' not_expr)*
;;   not_expr  : '!' not_expr | primary
;;   primary   : '(' or_expr ')' | unary_test | binary_test | word
(def (parse-cond-command ps)
  (parser-consume! ps)  ;; skip [[
  (let ((expr (parse-cond-or ps)))
    (unless (parser-check-word? ps "]]")
      (error "parse error: expected ']]'"))
    (parser-consume! ps)
    (make-cond-command expr)))

(def (parse-cond-or ps)
  (let loop ((left (parse-cond-and ps)))
    (cond
      ((parser-check? ps 'OR_IF)
       (parser-consume! ps)
       (loop (make-cond-binary "||" left (parse-cond-and ps))))
      (else left))))

(def (parse-cond-and ps)
  (let loop ((left (parse-cond-not ps)))
    (cond
      ((parser-check? ps 'AND_IF)
       (parser-consume! ps)
       (loop (make-cond-binary "&&" left (parse-cond-not ps))))
      (else left))))

(def (parse-cond-not ps)
  (if (parser-check? ps 'BANG)
    (begin
      (parser-consume! ps)
      (make-cond-not (parse-cond-not ps)))
    (parse-cond-primary ps)))

(def (cond-unary-op? s)
  (member s '("-a" "-b" "-c" "-d" "-e" "-f" "-g" "-h" "-k" "-p" "-r" "-s"
              "-t" "-u" "-w" "-x" "-G" "-L" "-N" "-O" "-S" "-z" "-n"
              "-o" "-v" "-R")))

(def (cond-binary-op? s)
  (member s '("=" "==" "!=" "<" ">" "=~"
              "-eq" "-ne" "-lt" "-gt" "-le" "-ge"
              "-ef" "-nt" "-ot")))

;; Peek at next token to see if it's a binary operator
;; Handles WORD-based ops, LESS/GREAT tokens, and BANG= (!=)
(def (cond-peek-binary-op ps)
  (let ((peek (parser-peek ps)))
    (and peek
         (cond
           ((and (eq? (token-type peek) 'WORD) (cond-binary-op? (token-value peek)))
            (token-value peek))
           ((eq? (token-type peek) 'LESS) "<")
           ((eq? (token-type peek) 'GREAT) ">")
           ;; != is tokenized as BANG + WORD "="
           ((eq? (token-type peek) 'BANG) "!=")
           (else #f)))))

(def (parse-cond-primary ps)
  ;; Grouping
  (cond
    ((parser-check? ps 'LPAREN)
     (parser-consume! ps)
     (let ((expr (parse-cond-or ps)))
       (unless (parser-check? ps 'RPAREN)
         (error "parse error: expected ')' in [[ ]] expression"))
       (parser-consume! ps)
       expr))
    ;; Need a word token
    ((or (parser-check? ps 'WORD) (parser-check? ps 'ASSIGNMENT_WORD))
     (let* ((tok (parser-next! ps))
            (val (token-value tok)))
       ;; Check for unary operator
       (if (cond-unary-op? val)
         ;; Unary test: -f file, -z str, etc.
         (let ((arg-tok (parser-next! ps)))
           (make-cond-unary-test val (token-value arg-tok)))
         ;; Could be binary test: word OP word
         ;; Or bare word
         (let ((op (cond-peek-binary-op ps)))
           (if op
             ;; Binary test
             (begin
               (parser-consume! ps) ;; skip operator token
               ;; != is BANG + WORD "=" — need to consume the "=" too
               (when (string=? op "!=")
                 (parser-consume! ps))
               ;; For =~ and pattern ops, collect RHS liberally
               ;; (parens can appear in regex patterns)
               (let ((right (cond-collect-rhs ps op)))
                 (make-cond-binary-test op val right)))
             ;; Bare word (truthy if non-empty after expansion)
             (make-cond-word val))))))
    (else
     (error "parse error: unexpected token in [[ ]] expression"
            (let ((t (parser-peek ps))) (and t (token-value t)))))))

;; Collect right-hand side for binary ops in [[ ]]
;; For =~, ==, !=: parens and other operators can appear as part of the value
(def (cond-collect-rhs ps op)
  (if (member op '("=~" "==" "=" "!="))
    ;; Collect tokens liberally — parens/angles are part of pattern/regex
    (let loop ((parts []))
      (let ((peek (parser-peek ps)))
        (cond
          ((not peek) (apply string-append (reverse parts)))
          ;; Stop at ]] && ||
          ((and (eq? (token-type peek) 'WORD)
                (member (token-value peek) '("]]")))
           (apply string-append (reverse parts)))
          ((memq (token-type peek) '(AND_AND OR_OR))
           (apply string-append (reverse parts)))
          ;; RPAREN at top level could be grouping close — stop
          ;; But for regex, we want to include parens.
          ;; Heuristic: if we already have content, RPAREN is part of it
          ((eq? (token-type peek) 'RPAREN)
           (if (null? parts)
             ;; standalone ) — probably grouping, stop
             (apply string-append (reverse parts))
             ;; continuation — include it
             (begin (parser-consume! ps) (loop (cons ")" parts)))))
          ((eq? (token-type peek) 'LPAREN)
           (parser-consume! ps) (loop (cons "(" parts)))
          ((eq? (token-type peek) 'LESS)
           (parser-consume! ps) (loop (cons "<" parts)))
          ((eq? (token-type peek) 'GREAT)
           (parser-consume! ps) (loop (cons ">" parts)))
          ((eq? (token-type peek) 'BANG)
           (parser-consume! ps) (loop (cons "!" parts)))
          ((eq? (token-type peek) 'PIPE)
           (parser-consume! ps) (loop (cons "|" parts)))
          ((memq (token-type peek) '(WORD ASSIGNMENT_WORD))
           (parser-consume! ps) (loop (cons (token-value peek) parts)))
          (else
           (apply string-append (reverse parts))))))
    ;; For other ops (<, >, -eq, etc.), just read one token
    (let ((tok (parser-next! ps)))
      (token-value tok))))

;;; --- Helpers ---
