#!chezscheme
(library (jsh ast)
  (export token::t make-token token? token-type token-value
   token-pos token-type-set! token-value-set! token-pos-set!
   &token-type &token-value &token-pos &token-type-set!
   &token-value-set! &token-pos-set! simple-command::t
   make-simple-command simple-command?
   simple-command-assignments simple-command-words
   simple-command-redirections simple-command-assignments-set!
   simple-command-words-set! simple-command-redirections-set!
   &simple-command-assignments &simple-command-words
   &simple-command-redirections
   &simple-command-assignments-set! &simple-command-words-set!
   &simple-command-redirections-set! ast-pipeline::t
   make-ast-pipeline ast-pipeline? ast-pipeline-commands
   ast-pipeline-bang? ast-pipeline-pipe-types
   ast-pipeline-commands-set! ast-pipeline-bang?-set!
   ast-pipeline-pipe-types-set! &ast-pipeline-commands
   &ast-pipeline-bang? &ast-pipeline-pipe-types
   &ast-pipeline-commands-set! &ast-pipeline-bang?-set!
   &ast-pipeline-pipe-types-set! and-or-list::t
   make-and-or-list and-or-list? and-or-list-first
   and-or-list-rest and-or-list-first-set!
   and-or-list-rest-set! &and-or-list-first &and-or-list-rest
   &and-or-list-first-set! &and-or-list-rest-set!
   command-list::t make-command-list command-list?
   command-list-items command-list-items-set!
   &command-list-items &command-list-items-set!
   redirected-command::t make-redirected-command
   redirected-command? redirected-command-command
   redirected-command-redirections
   redirected-command-command-set!
   redirected-command-redirections-set!
   &redirected-command-command &redirected-command-redirections
   &redirected-command-command-set!
   &redirected-command-redirections-set! subshell::t
   make-subshell subshell? subshell-body subshell-body-set!
   &subshell-body &subshell-body-set! brace-group::t
   make-brace-group brace-group? brace-group-body
   brace-group-body-set! &brace-group-body
   &brace-group-body-set! if-command::t make-if-command
   if-command? if-command-clauses if-command-else-part
   if-command-clauses-set! if-command-else-part-set!
   &if-command-clauses &if-command-else-part
   &if-command-clauses-set! &if-command-else-part-set!
   for-command::t make-for-command for-command? for-command-var
   for-command-words for-command-body for-command-var-set!
   for-command-words-set! for-command-body-set!
   &for-command-var &for-command-words &for-command-body
   &for-command-var-set! &for-command-words-set!
   &for-command-body-set! while-command::t make-while-command
   while-command? while-command-test while-command-body
   while-command-test-set! while-command-body-set!
   &while-command-test &while-command-body
   &while-command-test-set! &while-command-body-set!
   until-command::t make-until-command until-command?
   until-command-test until-command-body
   until-command-test-set! until-command-body-set!
   &until-command-test &until-command-body
   &until-command-test-set! &until-command-body-set!
   case-command::t make-case-command case-command?
   case-command-word case-command-clauses
   case-command-word-set! case-command-clauses-set!
   &case-command-word &case-command-clauses
   &case-command-word-set! &case-command-clauses-set!
   select-command::t make-select-command select-command?
   select-command-var select-command-words select-command-body
   select-command-var-set! select-command-words-set!
   select-command-body-set! &select-command-var
   &select-command-words &select-command-body
   &select-command-var-set! &select-command-words-set!
   &select-command-body-set! arith-command::t
   make-arith-command arith-command? arith-command-expression
   arith-command-expression-set! &arith-command-expression
   &arith-command-expression-set! arith-for-command::t
   make-arith-for-command arith-for-command?
   arith-for-command-init arith-for-command-test
   arith-for-command-update arith-for-command-body
   arith-for-command-init-set! arith-for-command-test-set!
   arith-for-command-update-set! arith-for-command-body-set!
   &arith-for-command-init &arith-for-command-test
   &arith-for-command-update &arith-for-command-body
   &arith-for-command-init-set! &arith-for-command-test-set!
   &arith-for-command-update-set! &arith-for-command-body-set!
   cond-command::t make-cond-command cond-command?
   cond-command-expr cond-command-expr-set! &cond-command-expr
   &cond-command-expr-set! coproc-command::t
   make-coproc-command coproc-command? coproc-command-name
   coproc-command-command coproc-command-name-set!
   coproc-command-command-set! &coproc-command-name
   &coproc-command-command &coproc-command-name-set!
   &coproc-command-command-set! time-command::t
   make-time-command time-command? time-command-posix?
   time-command-pipeline time-command-posix?-set!
   time-command-pipeline-set! &time-command-posix?
   &time-command-pipeline &time-command-posix?-set!
   &time-command-pipeline-set! cond-binary::t make-cond-binary
   cond-binary? cond-binary-op cond-binary-left
   cond-binary-right cond-binary-op-set! cond-binary-left-set!
   cond-binary-right-set! &cond-binary-op &cond-binary-left
   &cond-binary-right &cond-binary-op-set!
   &cond-binary-left-set! &cond-binary-right-set! cond-not::t
   make-cond-not cond-not? cond-not-expr cond-not-expr-set!
   &cond-not-expr &cond-not-expr-set! cond-unary-test::t
   make-cond-unary-test cond-unary-test? cond-unary-test-op
   cond-unary-test-arg cond-unary-test-op-set!
   cond-unary-test-arg-set! &cond-unary-test-op
   &cond-unary-test-arg &cond-unary-test-op-set!
   &cond-unary-test-arg-set! cond-binary-test::t
   make-cond-binary-test cond-binary-test? cond-binary-test-op
   cond-binary-test-left cond-binary-test-right
   cond-binary-test-op-set! cond-binary-test-left-set!
   cond-binary-test-right-set! &cond-binary-test-op
   &cond-binary-test-left &cond-binary-test-right
   &cond-binary-test-op-set! &cond-binary-test-left-set!
   &cond-binary-test-right-set! cond-word::t make-cond-word
   cond-word? cond-word-value cond-word-value-set!
   &cond-word-value &cond-word-value-set! case-clause::t
   make-case-clause case-clause? case-clause-patterns
   case-clause-body case-clause-terminator
   case-clause-patterns-set! case-clause-body-set!
   case-clause-terminator-set! &case-clause-patterns
   &case-clause-body &case-clause-terminator
   &case-clause-patterns-set! &case-clause-body-set!
   &case-clause-terminator-set! function-def::t
   make-function-def function-def? function-def-name
   function-def-body function-def-redirections
   function-def-lineno function-def-name-set!
   function-def-body-set! function-def-redirections-set!
   function-def-lineno-set! &function-def-name
   &function-def-body &function-def-redirections
   &function-def-lineno &function-def-name-set!
   &function-def-body-set! &function-def-redirections-set!
   &function-def-lineno-set! redir::t make-redir redir?
   redir-op redir-fd redir-target redir-fd-var redir-op-set!
   redir-fd-set! redir-target-set! redir-fd-var-set! &redir-op
   &redir-fd &redir-target &redir-fd-var &redir-op-set!
   &redir-fd-set! &redir-target-set! &redir-fd-var-set!
   assignment::t make-assignment assignment? assignment-name
   assignment-index assignment-value assignment-op
   assignment-name-set! assignment-index-set!
   assignment-value-set! assignment-op-set! &assignment-name
   &assignment-index &assignment-value &assignment-op
   &assignment-name-set! &assignment-index-set!
   &assignment-value-set! &assignment-op-set! word-literal::t
   make-word-literal word-literal? word-literal-text
   word-literal-text-set! &word-literal-text
   &word-literal-text-set! word-single-quoted::t
   make-word-single-quoted word-single-quoted?
   word-single-quoted-text word-single-quoted-text-set!
   &word-single-quoted-text &word-single-quoted-text-set!
   word-double-quoted::t make-word-double-quoted
   word-double-quoted? word-double-quoted-parts
   word-double-quoted-parts-set! &word-double-quoted-parts
   &word-double-quoted-parts-set! word-variable::t
   make-word-variable word-variable? word-variable-name
   word-variable-modifier word-variable-arg
   word-variable-name-set! word-variable-modifier-set!
   word-variable-arg-set! &word-variable-name
   &word-variable-modifier &word-variable-arg
   &word-variable-name-set! &word-variable-modifier-set!
   &word-variable-arg-set! word-command-sub::t
   make-word-command-sub word-command-sub?
   word-command-sub-command word-command-sub-quoted?
   word-command-sub-command-set! word-command-sub-quoted?-set!
   &word-command-sub-command &word-command-sub-quoted?
   &word-command-sub-command-set!
   &word-command-sub-quoted?-set! word-arith-sub::t
   make-word-arith-sub word-arith-sub?
   word-arith-sub-expression word-arith-sub-expression-set!
   &word-arith-sub-expression &word-arith-sub-expression-set!
   word-process-sub::t make-word-process-sub word-process-sub?
   word-process-sub-direction word-process-sub-command
   word-process-sub-direction-set!
   word-process-sub-command-set! &word-process-sub-direction
   &word-process-sub-command &word-process-sub-direction-set!
   &word-process-sub-command-set! word-glob::t make-word-glob
   word-glob? word-glob-pattern word-glob-pattern-set!
   &word-glob-pattern &word-glob-pattern-set! word-tilde::t
   make-word-tilde word-tilde? word-tilde-user
   word-tilde-user-set! &word-tilde-user &word-tilde-user-set!
   word-brace-expand::t make-word-brace-expand
   word-brace-expand? word-brace-expand-parts
   word-brace-expand-parts-set! &word-brace-expand-parts
   &word-brace-expand-parts-set!)
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
    (except (std format) format) (std sort) (std pregexp))
  (begin
    (define token::t
      (make-class-type 'gerbil\x23;token::t 'token (list object::t)
        '(type value pos) '((struct: . #t) (transparent: . #t))
        '#f))
    (define (make-token . args)
      (let* ([type token::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (token? obj)
      (\x23;\x23;structure-instance-of? obj 'gerbil\x23;token::t))
    (define (token-type obj) (unchecked-slot-ref obj 'type))
    (define (token-value obj) (unchecked-slot-ref obj 'value))
    (define (token-pos obj) (unchecked-slot-ref obj 'pos))
    (define (token-type-set! obj val)
      (unchecked-slot-set! obj 'type val))
    (define (token-value-set! obj val)
      (unchecked-slot-set! obj 'value val))
    (define (token-pos-set! obj val)
      (unchecked-slot-set! obj 'pos val))
    (define (&token-type obj) (unchecked-slot-ref obj 'type))
    (define (&token-value obj) (unchecked-slot-ref obj 'value))
    (define (&token-pos obj) (unchecked-slot-ref obj 'pos))
    (define (&token-type-set! obj val)
      (unchecked-slot-set! obj 'type val))
    (define (&token-value-set! obj val)
      (unchecked-slot-set! obj 'value val))
    (define (&token-pos-set! obj val)
      (unchecked-slot-set! obj 'pos val)))
  (begin
    (define simple-command::t
      (make-class-type 'gerbil\x23;simple-command::t 'simple-command
        (list object::t) '(assignments words redirections)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-simple-command . args)
      (let* ([type simple-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (simple-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;simple-command::t))
    (define (simple-command-assignments obj)
      (unchecked-slot-ref obj 'assignments))
    (define (simple-command-words obj)
      (unchecked-slot-ref obj 'words))
    (define (simple-command-redirections obj)
      (unchecked-slot-ref obj 'redirections))
    (define (simple-command-assignments-set! obj val)
      (unchecked-slot-set! obj 'assignments val))
    (define (simple-command-words-set! obj val)
      (unchecked-slot-set! obj 'words val))
    (define (simple-command-redirections-set! obj val)
      (unchecked-slot-set! obj 'redirections val))
    (define (&simple-command-assignments obj)
      (unchecked-slot-ref obj 'assignments))
    (define (&simple-command-words obj)
      (unchecked-slot-ref obj 'words))
    (define (&simple-command-redirections obj)
      (unchecked-slot-ref obj 'redirections))
    (define (&simple-command-assignments-set! obj val)
      (unchecked-slot-set! obj 'assignments val))
    (define (&simple-command-words-set! obj val)
      (unchecked-slot-set! obj 'words val))
    (define (&simple-command-redirections-set! obj val)
      (unchecked-slot-set! obj 'redirections val)))
  (begin
    (define ast-pipeline::t
      (make-class-type 'gerbil\x23;ast-pipeline::t 'ast-pipeline (list object::t)
        '(commands bang? pipe-types)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-ast-pipeline . args)
      (let* ([type ast-pipeline::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (ast-pipeline? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;ast-pipeline::t))
    (define (ast-pipeline-commands obj)
      (unchecked-slot-ref obj 'commands))
    (define (ast-pipeline-bang? obj)
      (unchecked-slot-ref obj 'bang?))
    (define (ast-pipeline-pipe-types obj)
      (unchecked-slot-ref obj 'pipe-types))
    (define (ast-pipeline-commands-set! obj val)
      (unchecked-slot-set! obj 'commands val))
    (define (ast-pipeline-bang?-set! obj val)
      (unchecked-slot-set! obj 'bang? val))
    (define (ast-pipeline-pipe-types-set! obj val)
      (unchecked-slot-set! obj 'pipe-types val))
    (define (&ast-pipeline-commands obj)
      (unchecked-slot-ref obj 'commands))
    (define (&ast-pipeline-bang? obj)
      (unchecked-slot-ref obj 'bang?))
    (define (&ast-pipeline-pipe-types obj)
      (unchecked-slot-ref obj 'pipe-types))
    (define (&ast-pipeline-commands-set! obj val)
      (unchecked-slot-set! obj 'commands val))
    (define (&ast-pipeline-bang?-set! obj val)
      (unchecked-slot-set! obj 'bang? val))
    (define (&ast-pipeline-pipe-types-set! obj val)
      (unchecked-slot-set! obj 'pipe-types val)))
  (begin
    (define and-or-list::t
      (make-class-type 'gerbil\x23;and-or-list::t 'and-or-list (list object::t)
        '(first rest) '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-and-or-list . args)
      (let* ([type and-or-list::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (and-or-list? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;and-or-list::t))
    (define (and-or-list-first obj)
      (unchecked-slot-ref obj 'first))
    (define (and-or-list-rest obj)
      (unchecked-slot-ref obj 'rest))
    (define (and-or-list-first-set! obj val)
      (unchecked-slot-set! obj 'first val))
    (define (and-or-list-rest-set! obj val)
      (unchecked-slot-set! obj 'rest val))
    (define (&and-or-list-first obj)
      (unchecked-slot-ref obj 'first))
    (define (&and-or-list-rest obj)
      (unchecked-slot-ref obj 'rest))
    (define (&and-or-list-first-set! obj val)
      (unchecked-slot-set! obj 'first val))
    (define (&and-or-list-rest-set! obj val)
      (unchecked-slot-set! obj 'rest val)))
  (begin
    (define command-list::t
      (make-class-type 'gerbil\x23;command-list::t 'command-list (list object::t)
        '(items) '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-command-list . args)
      (let* ([type command-list::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (command-list? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;command-list::t))
    (define (command-list-items obj)
      (unchecked-slot-ref obj 'items))
    (define (command-list-items-set! obj val)
      (unchecked-slot-set! obj 'items val))
    (define (&command-list-items obj)
      (unchecked-slot-ref obj 'items))
    (define (&command-list-items-set! obj val)
      (unchecked-slot-set! obj 'items val)))
  (begin
    (define redirected-command::t
      (make-class-type 'gerbil\x23;redirected-command::t 'redirected-command
        (list object::t) '(command redirections)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-redirected-command . args)
      (let* ([type redirected-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (redirected-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;redirected-command::t))
    (define (redirected-command-command obj)
      (unchecked-slot-ref obj 'command))
    (define (redirected-command-redirections obj)
      (unchecked-slot-ref obj 'redirections))
    (define (redirected-command-command-set! obj val)
      (unchecked-slot-set! obj 'command val))
    (define (redirected-command-redirections-set! obj val)
      (unchecked-slot-set! obj 'redirections val))
    (define (&redirected-command-command obj)
      (unchecked-slot-ref obj 'command))
    (define (&redirected-command-redirections obj)
      (unchecked-slot-ref obj 'redirections))
    (define (&redirected-command-command-set! obj val)
      (unchecked-slot-set! obj 'command val))
    (define (&redirected-command-redirections-set! obj val)
      (unchecked-slot-set! obj 'redirections val)))
  (begin
    (define subshell::t
      (make-class-type 'gerbil\x23;subshell::t 'subshell (list object::t) '(body)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-subshell . args)
      (let* ([type subshell::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (subshell? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;subshell::t))
    (define (subshell-body obj) (unchecked-slot-ref obj 'body))
    (define (subshell-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (&subshell-body obj) (unchecked-slot-ref obj 'body))
    (define (&subshell-body-set! obj val)
      (unchecked-slot-set! obj 'body val)))
  (begin
    (define brace-group::t
      (make-class-type 'gerbil\x23;brace-group::t 'brace-group (list object::t)
        '(body) '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-brace-group . args)
      (let* ([type brace-group::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (brace-group? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;brace-group::t))
    (define (brace-group-body obj)
      (unchecked-slot-ref obj 'body))
    (define (brace-group-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (&brace-group-body obj)
      (unchecked-slot-ref obj 'body))
    (define (&brace-group-body-set! obj val)
      (unchecked-slot-set! obj 'body val)))
  (begin
    (define if-command::t
      (make-class-type 'gerbil\x23;if-command::t 'if-command (list object::t)
        '(clauses else-part) '((struct: . #t) (transparent: . #t))
        '#f))
    (define (make-if-command . args)
      (let* ([type if-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (if-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;if-command::t))
    (define (if-command-clauses obj)
      (unchecked-slot-ref obj 'clauses))
    (define (if-command-else-part obj)
      (unchecked-slot-ref obj 'else-part))
    (define (if-command-clauses-set! obj val)
      (unchecked-slot-set! obj 'clauses val))
    (define (if-command-else-part-set! obj val)
      (unchecked-slot-set! obj 'else-part val))
    (define (&if-command-clauses obj)
      (unchecked-slot-ref obj 'clauses))
    (define (&if-command-else-part obj)
      (unchecked-slot-ref obj 'else-part))
    (define (&if-command-clauses-set! obj val)
      (unchecked-slot-set! obj 'clauses val))
    (define (&if-command-else-part-set! obj val)
      (unchecked-slot-set! obj 'else-part val)))
  (begin
    (define for-command::t
      (make-class-type 'gerbil\x23;for-command::t 'for-command (list object::t)
        '(var words body) '((struct: . #t) (transparent: . #t))
        '#f))
    (define (make-for-command . args)
      (let* ([type for-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (for-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;for-command::t))
    (define (for-command-var obj) (unchecked-slot-ref obj 'var))
    (define (for-command-words obj)
      (unchecked-slot-ref obj 'words))
    (define (for-command-body obj)
      (unchecked-slot-ref obj 'body))
    (define (for-command-var-set! obj val)
      (unchecked-slot-set! obj 'var val))
    (define (for-command-words-set! obj val)
      (unchecked-slot-set! obj 'words val))
    (define (for-command-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (&for-command-var obj)
      (unchecked-slot-ref obj 'var))
    (define (&for-command-words obj)
      (unchecked-slot-ref obj 'words))
    (define (&for-command-body obj)
      (unchecked-slot-ref obj 'body))
    (define (&for-command-var-set! obj val)
      (unchecked-slot-set! obj 'var val))
    (define (&for-command-words-set! obj val)
      (unchecked-slot-set! obj 'words val))
    (define (&for-command-body-set! obj val)
      (unchecked-slot-set! obj 'body val)))
  (begin
    (define while-command::t
      (make-class-type 'gerbil\x23;while-command::t 'while-command
        (list object::t) '(test body)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-while-command . args)
      (let* ([type while-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (while-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;while-command::t))
    (define (while-command-test obj)
      (unchecked-slot-ref obj 'test))
    (define (while-command-body obj)
      (unchecked-slot-ref obj 'body))
    (define (while-command-test-set! obj val)
      (unchecked-slot-set! obj 'test val))
    (define (while-command-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (&while-command-test obj)
      (unchecked-slot-ref obj 'test))
    (define (&while-command-body obj)
      (unchecked-slot-ref obj 'body))
    (define (&while-command-test-set! obj val)
      (unchecked-slot-set! obj 'test val))
    (define (&while-command-body-set! obj val)
      (unchecked-slot-set! obj 'body val)))
  (begin
    (define until-command::t
      (make-class-type 'gerbil\x23;until-command::t 'until-command
        (list object::t) '(test body)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-until-command . args)
      (let* ([type until-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (until-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;until-command::t))
    (define (until-command-test obj)
      (unchecked-slot-ref obj 'test))
    (define (until-command-body obj)
      (unchecked-slot-ref obj 'body))
    (define (until-command-test-set! obj val)
      (unchecked-slot-set! obj 'test val))
    (define (until-command-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (&until-command-test obj)
      (unchecked-slot-ref obj 'test))
    (define (&until-command-body obj)
      (unchecked-slot-ref obj 'body))
    (define (&until-command-test-set! obj val)
      (unchecked-slot-set! obj 'test val))
    (define (&until-command-body-set! obj val)
      (unchecked-slot-set! obj 'body val)))
  (begin
    (define case-command::t
      (make-class-type 'gerbil\x23;case-command::t 'case-command (list object::t)
        '(word clauses) '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-case-command . args)
      (let* ([type case-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (case-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;case-command::t))
    (define (case-command-word obj)
      (unchecked-slot-ref obj 'word))
    (define (case-command-clauses obj)
      (unchecked-slot-ref obj 'clauses))
    (define (case-command-word-set! obj val)
      (unchecked-slot-set! obj 'word val))
    (define (case-command-clauses-set! obj val)
      (unchecked-slot-set! obj 'clauses val))
    (define (&case-command-word obj)
      (unchecked-slot-ref obj 'word))
    (define (&case-command-clauses obj)
      (unchecked-slot-ref obj 'clauses))
    (define (&case-command-word-set! obj val)
      (unchecked-slot-set! obj 'word val))
    (define (&case-command-clauses-set! obj val)
      (unchecked-slot-set! obj 'clauses val)))
  (begin
    (define select-command::t
      (make-class-type 'gerbil\x23;select-command::t 'select-command
        (list object::t) '(var words body)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-select-command . args)
      (let* ([type select-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (select-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;select-command::t))
    (define (select-command-var obj)
      (unchecked-slot-ref obj 'var))
    (define (select-command-words obj)
      (unchecked-slot-ref obj 'words))
    (define (select-command-body obj)
      (unchecked-slot-ref obj 'body))
    (define (select-command-var-set! obj val)
      (unchecked-slot-set! obj 'var val))
    (define (select-command-words-set! obj val)
      (unchecked-slot-set! obj 'words val))
    (define (select-command-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (&select-command-var obj)
      (unchecked-slot-ref obj 'var))
    (define (&select-command-words obj)
      (unchecked-slot-ref obj 'words))
    (define (&select-command-body obj)
      (unchecked-slot-ref obj 'body))
    (define (&select-command-var-set! obj val)
      (unchecked-slot-set! obj 'var val))
    (define (&select-command-words-set! obj val)
      (unchecked-slot-set! obj 'words val))
    (define (&select-command-body-set! obj val)
      (unchecked-slot-set! obj 'body val)))
  (begin
    (define arith-command::t
      (make-class-type 'gerbil\x23;arith-command::t 'arith-command
        (list object::t) '(expression)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-arith-command . args)
      (let* ([type arith-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (arith-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;arith-command::t))
    (define (arith-command-expression obj)
      (unchecked-slot-ref obj 'expression))
    (define (arith-command-expression-set! obj val)
      (unchecked-slot-set! obj 'expression val))
    (define (&arith-command-expression obj)
      (unchecked-slot-ref obj 'expression))
    (define (&arith-command-expression-set! obj val)
      (unchecked-slot-set! obj 'expression val)))
  (begin
    (define arith-for-command::t
      (make-class-type 'gerbil\x23;arith-for-command::t 'arith-for-command
        (list object::t) '(init test update body)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-arith-for-command . args)
      (let* ([type arith-for-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (arith-for-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;arith-for-command::t))
    (define (arith-for-command-init obj)
      (unchecked-slot-ref obj 'init))
    (define (arith-for-command-test obj)
      (unchecked-slot-ref obj 'test))
    (define (arith-for-command-update obj)
      (unchecked-slot-ref obj 'update))
    (define (arith-for-command-body obj)
      (unchecked-slot-ref obj 'body))
    (define (arith-for-command-init-set! obj val)
      (unchecked-slot-set! obj 'init val))
    (define (arith-for-command-test-set! obj val)
      (unchecked-slot-set! obj 'test val))
    (define (arith-for-command-update-set! obj val)
      (unchecked-slot-set! obj 'update val))
    (define (arith-for-command-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (&arith-for-command-init obj)
      (unchecked-slot-ref obj 'init))
    (define (&arith-for-command-test obj)
      (unchecked-slot-ref obj 'test))
    (define (&arith-for-command-update obj)
      (unchecked-slot-ref obj 'update))
    (define (&arith-for-command-body obj)
      (unchecked-slot-ref obj 'body))
    (define (&arith-for-command-init-set! obj val)
      (unchecked-slot-set! obj 'init val))
    (define (&arith-for-command-test-set! obj val)
      (unchecked-slot-set! obj 'test val))
    (define (&arith-for-command-update-set! obj val)
      (unchecked-slot-set! obj 'update val))
    (define (&arith-for-command-body-set! obj val)
      (unchecked-slot-set! obj 'body val)))
  (begin
    (define cond-command::t
      (make-class-type 'gerbil\x23;cond-command::t 'cond-command (list object::t)
        '(expr) '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-cond-command . args)
      (let* ([type cond-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (cond-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;cond-command::t))
    (define (cond-command-expr obj)
      (unchecked-slot-ref obj 'expr))
    (define (cond-command-expr-set! obj val)
      (unchecked-slot-set! obj 'expr val))
    (define (&cond-command-expr obj)
      (unchecked-slot-ref obj 'expr))
    (define (&cond-command-expr-set! obj val)
      (unchecked-slot-set! obj 'expr val)))
  (begin
    (define coproc-command::t
      (make-class-type 'gerbil\x23;coproc-command::t 'coproc-command
        (list object::t) '(name command)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-coproc-command . args)
      (let* ([type coproc-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (coproc-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;coproc-command::t))
    (define (coproc-command-name obj)
      (unchecked-slot-ref obj 'name))
    (define (coproc-command-command obj)
      (unchecked-slot-ref obj 'command))
    (define (coproc-command-name-set! obj val)
      (unchecked-slot-set! obj 'name val))
    (define (coproc-command-command-set! obj val)
      (unchecked-slot-set! obj 'command val))
    (define (&coproc-command-name obj)
      (unchecked-slot-ref obj 'name))
    (define (&coproc-command-command obj)
      (unchecked-slot-ref obj 'command))
    (define (&coproc-command-name-set! obj val)
      (unchecked-slot-set! obj 'name val))
    (define (&coproc-command-command-set! obj val)
      (unchecked-slot-set! obj 'command val)))
  (begin
    (define time-command::t
      (make-class-type 'gerbil\x23;time-command::t 'time-command (list object::t)
        '(posix? pipeline) '((struct: . #t) (transparent: . #t))
        '#f))
    (define (make-time-command . args)
      (let* ([type time-command::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (time-command? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;time-command::t))
    (define (time-command-posix? obj)
      (unchecked-slot-ref obj 'posix?))
    (define (time-command-pipeline obj)
      (unchecked-slot-ref obj 'pipeline))
    (define (time-command-posix?-set! obj val)
      (unchecked-slot-set! obj 'posix? val))
    (define (time-command-pipeline-set! obj val)
      (unchecked-slot-set! obj 'pipeline val))
    (define (&time-command-posix? obj)
      (unchecked-slot-ref obj 'posix?))
    (define (&time-command-pipeline obj)
      (unchecked-slot-ref obj 'pipeline))
    (define (&time-command-posix?-set! obj val)
      (unchecked-slot-set! obj 'posix? val))
    (define (&time-command-pipeline-set! obj val)
      (unchecked-slot-set! obj 'pipeline val)))
  (begin
    (define cond-binary::t
      (make-class-type 'gerbil\x23;cond-binary::t 'cond-binary (list object::t)
        '(op left right) '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-cond-binary . args)
      (let* ([type cond-binary::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (cond-binary? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;cond-binary::t))
    (define (cond-binary-op obj) (unchecked-slot-ref obj 'op))
    (define (cond-binary-left obj)
      (unchecked-slot-ref obj 'left))
    (define (cond-binary-right obj)
      (unchecked-slot-ref obj 'right))
    (define (cond-binary-op-set! obj val)
      (unchecked-slot-set! obj 'op val))
    (define (cond-binary-left-set! obj val)
      (unchecked-slot-set! obj 'left val))
    (define (cond-binary-right-set! obj val)
      (unchecked-slot-set! obj 'right val))
    (define (&cond-binary-op obj) (unchecked-slot-ref obj 'op))
    (define (&cond-binary-left obj)
      (unchecked-slot-ref obj 'left))
    (define (&cond-binary-right obj)
      (unchecked-slot-ref obj 'right))
    (define (&cond-binary-op-set! obj val)
      (unchecked-slot-set! obj 'op val))
    (define (&cond-binary-left-set! obj val)
      (unchecked-slot-set! obj 'left val))
    (define (&cond-binary-right-set! obj val)
      (unchecked-slot-set! obj 'right val)))
  (begin
    (define cond-not::t
      (make-class-type 'gerbil\x23;cond-not::t 'cond-not (list object::t) '(expr)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-cond-not . args)
      (let* ([type cond-not::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (cond-not? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;cond-not::t))
    (define (cond-not-expr obj) (unchecked-slot-ref obj 'expr))
    (define (cond-not-expr-set! obj val)
      (unchecked-slot-set! obj 'expr val))
    (define (&cond-not-expr obj) (unchecked-slot-ref obj 'expr))
    (define (&cond-not-expr-set! obj val)
      (unchecked-slot-set! obj 'expr val)))
  (begin
    (define cond-unary-test::t
      (make-class-type 'gerbil\x23;cond-unary-test::t 'cond-unary-test
        (list object::t) '(op arg)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-cond-unary-test . args)
      (let* ([type cond-unary-test::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (cond-unary-test? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;cond-unary-test::t))
    (define (cond-unary-test-op obj)
      (unchecked-slot-ref obj 'op))
    (define (cond-unary-test-arg obj)
      (unchecked-slot-ref obj 'arg))
    (define (cond-unary-test-op-set! obj val)
      (unchecked-slot-set! obj 'op val))
    (define (cond-unary-test-arg-set! obj val)
      (unchecked-slot-set! obj 'arg val))
    (define (&cond-unary-test-op obj)
      (unchecked-slot-ref obj 'op))
    (define (&cond-unary-test-arg obj)
      (unchecked-slot-ref obj 'arg))
    (define (&cond-unary-test-op-set! obj val)
      (unchecked-slot-set! obj 'op val))
    (define (&cond-unary-test-arg-set! obj val)
      (unchecked-slot-set! obj 'arg val)))
  (begin
    (define cond-binary-test::t
      (make-class-type 'gerbil\x23;cond-binary-test::t 'cond-binary-test
        (list object::t) '(op left right)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-cond-binary-test . args)
      (let* ([type cond-binary-test::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (cond-binary-test? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;cond-binary-test::t))
    (define (cond-binary-test-op obj)
      (unchecked-slot-ref obj 'op))
    (define (cond-binary-test-left obj)
      (unchecked-slot-ref obj 'left))
    (define (cond-binary-test-right obj)
      (unchecked-slot-ref obj 'right))
    (define (cond-binary-test-op-set! obj val)
      (unchecked-slot-set! obj 'op val))
    (define (cond-binary-test-left-set! obj val)
      (unchecked-slot-set! obj 'left val))
    (define (cond-binary-test-right-set! obj val)
      (unchecked-slot-set! obj 'right val))
    (define (&cond-binary-test-op obj)
      (unchecked-slot-ref obj 'op))
    (define (&cond-binary-test-left obj)
      (unchecked-slot-ref obj 'left))
    (define (&cond-binary-test-right obj)
      (unchecked-slot-ref obj 'right))
    (define (&cond-binary-test-op-set! obj val)
      (unchecked-slot-set! obj 'op val))
    (define (&cond-binary-test-left-set! obj val)
      (unchecked-slot-set! obj 'left val))
    (define (&cond-binary-test-right-set! obj val)
      (unchecked-slot-set! obj 'right val)))
  (begin
    (define cond-word::t
      (make-class-type 'gerbil\x23;cond-word::t 'cond-word (list object::t)
        '(value) '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-cond-word . args)
      (let* ([type cond-word::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (cond-word? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;cond-word::t))
    (define (cond-word-value obj)
      (unchecked-slot-ref obj 'value))
    (define (cond-word-value-set! obj val)
      (unchecked-slot-set! obj 'value val))
    (define (&cond-word-value obj)
      (unchecked-slot-ref obj 'value))
    (define (&cond-word-value-set! obj val)
      (unchecked-slot-set! obj 'value val)))
  (begin
    (define case-clause::t
      (make-class-type 'gerbil\x23;case-clause::t 'case-clause (list object::t)
        '(patterns body terminator)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-case-clause . args)
      (let* ([type case-clause::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (case-clause? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;case-clause::t))
    (define (case-clause-patterns obj)
      (unchecked-slot-ref obj 'patterns))
    (define (case-clause-body obj)
      (unchecked-slot-ref obj 'body))
    (define (case-clause-terminator obj)
      (unchecked-slot-ref obj 'terminator))
    (define (case-clause-patterns-set! obj val)
      (unchecked-slot-set! obj 'patterns val))
    (define (case-clause-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (case-clause-terminator-set! obj val)
      (unchecked-slot-set! obj 'terminator val))
    (define (&case-clause-patterns obj)
      (unchecked-slot-ref obj 'patterns))
    (define (&case-clause-body obj)
      (unchecked-slot-ref obj 'body))
    (define (&case-clause-terminator obj)
      (unchecked-slot-ref obj 'terminator))
    (define (&case-clause-patterns-set! obj val)
      (unchecked-slot-set! obj 'patterns val))
    (define (&case-clause-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (&case-clause-terminator-set! obj val)
      (unchecked-slot-set! obj 'terminator val)))
  (begin
    (define function-def::t
      (make-class-type 'gerbil\x23;function-def::t 'function-def (list object::t)
        '(name body redirections lineno)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-function-def . args)
      (let* ([type function-def::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (function-def? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;function-def::t))
    (define (function-def-name obj)
      (unchecked-slot-ref obj 'name))
    (define (function-def-body obj)
      (unchecked-slot-ref obj 'body))
    (define (function-def-redirections obj)
      (unchecked-slot-ref obj 'redirections))
    (define (function-def-lineno obj)
      (unchecked-slot-ref obj 'lineno))
    (define (function-def-name-set! obj val)
      (unchecked-slot-set! obj 'name val))
    (define (function-def-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (function-def-redirections-set! obj val)
      (unchecked-slot-set! obj 'redirections val))
    (define (function-def-lineno-set! obj val)
      (unchecked-slot-set! obj 'lineno val))
    (define (&function-def-name obj)
      (unchecked-slot-ref obj 'name))
    (define (&function-def-body obj)
      (unchecked-slot-ref obj 'body))
    (define (&function-def-redirections obj)
      (unchecked-slot-ref obj 'redirections))
    (define (&function-def-lineno obj)
      (unchecked-slot-ref obj 'lineno))
    (define (&function-def-name-set! obj val)
      (unchecked-slot-set! obj 'name val))
    (define (&function-def-body-set! obj val)
      (unchecked-slot-set! obj 'body val))
    (define (&function-def-redirections-set! obj val)
      (unchecked-slot-set! obj 'redirections val))
    (define (&function-def-lineno-set! obj val)
      (unchecked-slot-set! obj 'lineno val)))
  (begin
    (define redir::t
      (make-class-type 'gerbil\x23;redir::t 'redir (list object::t)
        '(op fd target fd-var) '((struct: . #t) (transparent: . #t))
        '#f))
    (define (make-redir . args)
      (let* ([type redir::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (redir? obj)
      (\x23;\x23;structure-instance-of? obj 'gerbil\x23;redir::t))
    (define (redir-op obj) (unchecked-slot-ref obj 'op))
    (define (redir-fd obj) (unchecked-slot-ref obj 'fd))
    (define (redir-target obj) (unchecked-slot-ref obj 'target))
    (define (redir-fd-var obj) (unchecked-slot-ref obj 'fd-var))
    (define (redir-op-set! obj val)
      (unchecked-slot-set! obj 'op val))
    (define (redir-fd-set! obj val)
      (unchecked-slot-set! obj 'fd val))
    (define (redir-target-set! obj val)
      (unchecked-slot-set! obj 'target val))
    (define (redir-fd-var-set! obj val)
      (unchecked-slot-set! obj 'fd-var val))
    (define (&redir-op obj) (unchecked-slot-ref obj 'op))
    (define (&redir-fd obj) (unchecked-slot-ref obj 'fd))
    (define (&redir-target obj)
      (unchecked-slot-ref obj 'target))
    (define (&redir-fd-var obj)
      (unchecked-slot-ref obj 'fd-var))
    (define (&redir-op-set! obj val)
      (unchecked-slot-set! obj 'op val))
    (define (&redir-fd-set! obj val)
      (unchecked-slot-set! obj 'fd val))
    (define (&redir-target-set! obj val)
      (unchecked-slot-set! obj 'target val))
    (define (&redir-fd-var-set! obj val)
      (unchecked-slot-set! obj 'fd-var val)))
  (begin
    (define assignment::t
      (make-class-type 'gerbil\x23;assignment::t 'assignment (list object::t)
        '(name index value op) '((struct: . #t) (transparent: . #t))
        '#f))
    (define (make-assignment . args)
      (let* ([type assignment::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (assignment? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;assignment::t))
    (define (assignment-name obj)
      (unchecked-slot-ref obj 'name))
    (define (assignment-index obj)
      (unchecked-slot-ref obj 'index))
    (define (assignment-value obj)
      (unchecked-slot-ref obj 'value))
    (define (assignment-op obj) (unchecked-slot-ref obj 'op))
    (define (assignment-name-set! obj val)
      (unchecked-slot-set! obj 'name val))
    (define (assignment-index-set! obj val)
      (unchecked-slot-set! obj 'index val))
    (define (assignment-value-set! obj val)
      (unchecked-slot-set! obj 'value val))
    (define (assignment-op-set! obj val)
      (unchecked-slot-set! obj 'op val))
    (define (&assignment-name obj)
      (unchecked-slot-ref obj 'name))
    (define (&assignment-index obj)
      (unchecked-slot-ref obj 'index))
    (define (&assignment-value obj)
      (unchecked-slot-ref obj 'value))
    (define (&assignment-op obj) (unchecked-slot-ref obj 'op))
    (define (&assignment-name-set! obj val)
      (unchecked-slot-set! obj 'name val))
    (define (&assignment-index-set! obj val)
      (unchecked-slot-set! obj 'index val))
    (define (&assignment-value-set! obj val)
      (unchecked-slot-set! obj 'value val))
    (define (&assignment-op-set! obj val)
      (unchecked-slot-set! obj 'op val)))
  (begin
    (define word-literal::t
      (make-class-type 'gerbil\x23;word-literal::t 'word-literal (list object::t)
        '(text) '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-word-literal . args)
      (let* ([type word-literal::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (word-literal? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;word-literal::t))
    (define (word-literal-text obj)
      (unchecked-slot-ref obj 'text))
    (define (word-literal-text-set! obj val)
      (unchecked-slot-set! obj 'text val))
    (define (&word-literal-text obj)
      (unchecked-slot-ref obj 'text))
    (define (&word-literal-text-set! obj val)
      (unchecked-slot-set! obj 'text val)))
  (begin
    (define word-single-quoted::t
      (make-class-type 'gerbil\x23;word-single-quoted::t 'word-single-quoted
        (list object::t) '(text)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-word-single-quoted . args)
      (let* ([type word-single-quoted::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (word-single-quoted? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;word-single-quoted::t))
    (define (word-single-quoted-text obj)
      (unchecked-slot-ref obj 'text))
    (define (word-single-quoted-text-set! obj val)
      (unchecked-slot-set! obj 'text val))
    (define (&word-single-quoted-text obj)
      (unchecked-slot-ref obj 'text))
    (define (&word-single-quoted-text-set! obj val)
      (unchecked-slot-set! obj 'text val)))
  (begin
    (define word-double-quoted::t
      (make-class-type 'gerbil\x23;word-double-quoted::t 'word-double-quoted
        (list object::t) '(parts)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-word-double-quoted . args)
      (let* ([type word-double-quoted::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (word-double-quoted? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;word-double-quoted::t))
    (define (word-double-quoted-parts obj)
      (unchecked-slot-ref obj 'parts))
    (define (word-double-quoted-parts-set! obj val)
      (unchecked-slot-set! obj 'parts val))
    (define (&word-double-quoted-parts obj)
      (unchecked-slot-ref obj 'parts))
    (define (&word-double-quoted-parts-set! obj val)
      (unchecked-slot-set! obj 'parts val)))
  (begin
    (define word-variable::t
      (make-class-type 'gerbil\x23;word-variable::t 'word-variable
        (list object::t) '(name modifier arg)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-word-variable . args)
      (let* ([type word-variable::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (word-variable? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;word-variable::t))
    (define (word-variable-name obj)
      (unchecked-slot-ref obj 'name))
    (define (word-variable-modifier obj)
      (unchecked-slot-ref obj 'modifier))
    (define (word-variable-arg obj)
      (unchecked-slot-ref obj 'arg))
    (define (word-variable-name-set! obj val)
      (unchecked-slot-set! obj 'name val))
    (define (word-variable-modifier-set! obj val)
      (unchecked-slot-set! obj 'modifier val))
    (define (word-variable-arg-set! obj val)
      (unchecked-slot-set! obj 'arg val))
    (define (&word-variable-name obj)
      (unchecked-slot-ref obj 'name))
    (define (&word-variable-modifier obj)
      (unchecked-slot-ref obj 'modifier))
    (define (&word-variable-arg obj)
      (unchecked-slot-ref obj 'arg))
    (define (&word-variable-name-set! obj val)
      (unchecked-slot-set! obj 'name val))
    (define (&word-variable-modifier-set! obj val)
      (unchecked-slot-set! obj 'modifier val))
    (define (&word-variable-arg-set! obj val)
      (unchecked-slot-set! obj 'arg val)))
  (begin
    (define word-command-sub::t
      (make-class-type 'gerbil\x23;word-command-sub::t 'word-command-sub
        (list object::t) '(command quoted?)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-word-command-sub . args)
      (let* ([type word-command-sub::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (word-command-sub? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;word-command-sub::t))
    (define (word-command-sub-command obj)
      (unchecked-slot-ref obj 'command))
    (define (word-command-sub-quoted? obj)
      (unchecked-slot-ref obj 'quoted?))
    (define (word-command-sub-command-set! obj val)
      (unchecked-slot-set! obj 'command val))
    (define (word-command-sub-quoted?-set! obj val)
      (unchecked-slot-set! obj 'quoted? val))
    (define (&word-command-sub-command obj)
      (unchecked-slot-ref obj 'command))
    (define (&word-command-sub-quoted? obj)
      (unchecked-slot-ref obj 'quoted?))
    (define (&word-command-sub-command-set! obj val)
      (unchecked-slot-set! obj 'command val))
    (define (&word-command-sub-quoted?-set! obj val)
      (unchecked-slot-set! obj 'quoted? val)))
  (begin
    (define word-arith-sub::t
      (make-class-type 'gerbil\x23;word-arith-sub::t 'word-arith-sub
        (list object::t) '(expression)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-word-arith-sub . args)
      (let* ([type word-arith-sub::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (word-arith-sub? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;word-arith-sub::t))
    (define (word-arith-sub-expression obj)
      (unchecked-slot-ref obj 'expression))
    (define (word-arith-sub-expression-set! obj val)
      (unchecked-slot-set! obj 'expression val))
    (define (&word-arith-sub-expression obj)
      (unchecked-slot-ref obj 'expression))
    (define (&word-arith-sub-expression-set! obj val)
      (unchecked-slot-set! obj 'expression val)))
  (begin
    (define word-process-sub::t
      (make-class-type 'gerbil\x23;word-process-sub::t 'word-process-sub
        (list object::t) '(direction command)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-word-process-sub . args)
      (let* ([type word-process-sub::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (word-process-sub? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;word-process-sub::t))
    (define (word-process-sub-direction obj)
      (unchecked-slot-ref obj 'direction))
    (define (word-process-sub-command obj)
      (unchecked-slot-ref obj 'command))
    (define (word-process-sub-direction-set! obj val)
      (unchecked-slot-set! obj 'direction val))
    (define (word-process-sub-command-set! obj val)
      (unchecked-slot-set! obj 'command val))
    (define (&word-process-sub-direction obj)
      (unchecked-slot-ref obj 'direction))
    (define (&word-process-sub-command obj)
      (unchecked-slot-ref obj 'command))
    (define (&word-process-sub-direction-set! obj val)
      (unchecked-slot-set! obj 'direction val))
    (define (&word-process-sub-command-set! obj val)
      (unchecked-slot-set! obj 'command val)))
  (begin
    (define word-glob::t
      (make-class-type 'gerbil\x23;word-glob::t 'word-glob (list object::t)
        '(pattern) '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-word-glob . args)
      (let* ([type word-glob::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (word-glob? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;word-glob::t))
    (define (word-glob-pattern obj)
      (unchecked-slot-ref obj 'pattern))
    (define (word-glob-pattern-set! obj val)
      (unchecked-slot-set! obj 'pattern val))
    (define (&word-glob-pattern obj)
      (unchecked-slot-ref obj 'pattern))
    (define (&word-glob-pattern-set! obj val)
      (unchecked-slot-set! obj 'pattern val)))
  (begin
    (define word-tilde::t
      (make-class-type 'gerbil\x23;word-tilde::t 'word-tilde (list object::t)
        '(user) '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-word-tilde . args)
      (let* ([type word-tilde::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (word-tilde? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;word-tilde::t))
    (define (word-tilde-user obj)
      (unchecked-slot-ref obj 'user))
    (define (word-tilde-user-set! obj val)
      (unchecked-slot-set! obj 'user val))
    (define (&word-tilde-user obj)
      (unchecked-slot-ref obj 'user))
    (define (&word-tilde-user-set! obj val)
      (unchecked-slot-set! obj 'user val)))
  (begin
    (define word-brace-expand::t
      (make-class-type 'gerbil\x23;word-brace-expand::t 'word-brace-expand
        (list object::t) '(parts)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-word-brace-expand . args)
      (let* ([type word-brace-expand::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (word-brace-expand? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;word-brace-expand::t))
    (define (word-brace-expand-parts obj)
      (unchecked-slot-ref obj 'parts))
    (define (word-brace-expand-parts-set! obj val)
      (unchecked-slot-set! obj 'parts val))
    (define (&word-brace-expand-parts obj)
      (unchecked-slot-ref obj 'parts))
    (define (&word-brace-expand-parts-set! obj val)
      (unchecked-slot-set! obj 'parts val))))
