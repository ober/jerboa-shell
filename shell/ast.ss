;;; ast.ss — AST node definitions for gsh

(export #t)

;;; --- Tokens ---
(defstruct token (type value pos) transparent: #t)
;; type: 'word 'operator 'newline 'io-number 'assignment-word 'heredoc-body

;;; --- AST Nodes ---

;; A simple command: assignments, command word + arguments, redirections
(defstruct simple-command (assignments words redirections) transparent: #t)

;; Pipeline: list of commands connected by | or |&
(defstruct ast-pipeline (commands bang? pipe-types) transparent: #t)
;; bang? = #t if preceded by !
;; pipe-types: list of 'PIPE or 'PIPEAMP for each connection (length = commands - 1)

;; And-or list: pipeline && pipeline || pipeline ...
(defstruct and-or-list (first rest) transparent: #t)
;; rest: list of (cons 'and pipeline) or (cons 'or pipeline)

;; Command list: and-or-lists separated by ; or &
(defstruct command-list (items) transparent: #t)
;; items: list of (cons 'sequential and-or-list) or (cons 'background and-or-list)

;;; --- Compound command redirect wrapper ---
(defstruct redirected-command (command redirections) transparent: #t)

;;; --- Compound commands ---
(defstruct subshell (body) transparent: #t)              ; ( list )
(defstruct brace-group (body) transparent: #t)           ; { list; }
(defstruct if-command (clauses else-part) transparent: #t) ; if/elif/else
(defstruct for-command (var words body) transparent: #t)   ; for var in words; do body; done
(defstruct while-command (test body) transparent: #t)      ; while test; do body; done
(defstruct until-command (test body) transparent: #t)      ; until test; do body; done
(defstruct case-command (word clauses) transparent: #t)    ; case word in pattern) body;; esac
(defstruct select-command (var words body) transparent: #t) ; select var in words; do body; done
(defstruct arith-command (expression) transparent: #t)      ; (( expr ))
(defstruct arith-for-command (init test update body) transparent: #t) ; for ((init; test; update))
(defstruct cond-command (expr) transparent: #t)             ; [[ expr ]]
(defstruct coproc-command (name command) transparent: #t)    ; coproc [NAME] cmd
(defstruct time-command (posix? pipeline) transparent: #t)   ; time [-p] pipeline

;;; --- Conditional expression nodes (for [[ ]]) ---
(defstruct cond-binary (op left right) transparent: #t)     ; expr && expr, expr || expr
(defstruct cond-not (expr) transparent: #t)                 ; ! expr
(defstruct cond-unary-test (op arg) transparent: #t)        ; -f file, -z str, -n str
(defstruct cond-binary-test (op left right) transparent: #t) ; str1 = str2, n1 -eq n2
(defstruct cond-word (value) transparent: #t)               ; bare word (truthy if non-empty)

;; Case clause: list of patterns -> body
(defstruct case-clause (patterns body terminator) transparent: #t)
;; terminator: 'break (;;) 'fallthrough (;&) 'test-next (;;&)

;; Function definition
(defstruct function-def (name body redirections lineno) transparent: #t)

;;; --- Redirections ---
(defstruct redir (op fd target fd-var) transparent: #t)
;; op: '< '> '>> 'clobber '<< '<<- '<<< '<> '>& '<& '&> '&>>
;; fd: integer or #f (default)
;; target: string (word) or heredoc-body string
;; fd-var: string or #f — variable name for {varname}> syntax (auto-allocated fd)

;;; --- Assignment ---
(defstruct assignment (name index value op) transparent: #t)
;; index: #f for scalar, string for array index (e.g. "0", "key")
;; value: string for scalar, list of strings for compound array assignment
;; op: '= or '+=

;;; --- Word parts (a "word" is a list of these for expansion) ---
(defstruct word-literal (text) transparent: #t)
(defstruct word-single-quoted (text) transparent: #t)
(defstruct word-double-quoted (parts) transparent: #t)
(defstruct word-variable (name modifier arg) transparent: #t)
;; modifier: #f '- '+ '= '? ':- ':+ ':= ':?
;;           '% '%% '# '## '^ '^^ ', ',,
(defstruct word-command-sub (command quoted?) transparent: #t)   ; $(cmd) or `cmd`
(defstruct word-arith-sub (expression) transparent: #t)          ; $((expr))
(defstruct word-process-sub (direction command) transparent: #t) ; <(cmd) >(cmd)
(defstruct word-glob (pattern) transparent: #t)
(defstruct word-tilde (user) transparent: #t)                    ; ~ or ~user
(defstruct word-brace-expand (parts) transparent: #t)            ; {a,b,c} or {1..10}
