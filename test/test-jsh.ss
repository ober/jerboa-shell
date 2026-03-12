#!chezscheme
;;; test-jsh.ss — Unit tests for jsh (Jerboa Shell) modules
;;;
;;; Run: scheme --libdirs src:<jerboa>/lib:<gherkin>/src --script test/test-jsh.ss

(import
  (except (chezscheme) box box? unbox set-box! andmap ormap iota
    last-pair find 1+ 1- fx/ fx1+ fx1- error? raise
    with-exception-handler identifier? hash-table? make-hash-table
    sort sort! path-extension printf fprintf void)
  (except (jerboa runtime) bind-method! call-method ~ void cons* make-list)
  (runtime mop) (runtime util)
  (except (compat gambit) number->string make-mutex with-output-to-string thread?)
  (std format) (std sort) (std transducer)
  (only (std log) make-logger logger? logger-level logger-fields log-level?
    log-info log-debug current-logger))

(define pass-count 0)
(define fail-count 0)

(define-syntax check
  (syntax-rules (=>)
    [(_ expr => expected)
     (let ([got expr] [exp expected])
       (if (equal? got exp)
         (set! pass-count (+ 1 pass-count))
         (begin
           (set! fail-count (+ 1 fail-count))
           (printf "FAIL: ~s~n  expected: ~s~n  got:      ~s~n"
             'expr exp got))))]))

(define-syntax check-true
  (syntax-rules ()
    [(_ expr)
     (if expr
       (set! pass-count (+ 1 pass-count))
       (begin (set! fail-count (+ 1 fail-count))
              (printf "FAIL: expected true: ~s~n" 'expr)))]))

;;; ──────────────────────────────────────────────────────────────
;;; 1. Transducer integration (used by history search pipeline)
;;; ──────────────────────────────────────────────────────────────
(printf "--- Transducer tests ---~n")

;; Basic: filter + consecutive-dedup + take via sequence
(check
  (sequence
    (compose-transducers (filtering even?) (deduplicate) (taking 3))
    '(1 2 2 4 4 6 8))
  => '(2 4 6))

;; Prefix-filter pipeline (mirrors history-search)
;; Note: (deduplicate) removes consecutive dups — for full dedup we track seen set
(define (prefix-search prefix entries)
  (let ([plen (string-length prefix)]
        [seen (make-hashtable string-hash string=?)])
    (sequence
      (compose-transducers
        (filtering (lambda (cmd)
          (and (>= (string-length cmd) plen)
               (string=? (substring cmd 0 plen) prefix))))
        (filtering (lambda (cmd)
          (if (hashtable-ref seen cmd #f)
            #f
            (begin (hashtable-set! seen cmd #t) #t))))
        (taking 100))
      entries)))

(check (prefix-search "git" '("git status" "git commit" "git status" "ls" "git push"))
       => '("git status" "git commit" "git push"))
(check (prefix-search "ls" '("ls -la" "ls" "cat" "ls")) => '("ls -la" "ls"))
(check (prefix-search "no" '("ls" "cat" "echo")) => '())
(check (prefix-search "" '("a" "b" "a" "c")) => '("a" "b" "c"))

;; Windowing transducer
(check
  (sequence (windowing 3) '(1 2 3 4 5))
  => '((1 2 3) (2 3 4) (3 4 5)))

;; Flat-mapping
(check
  (sequence (flat-mapping (lambda (x) (list x (* x x)))) '(1 2 3))
  => '(1 1 2 4 3 9))

;; Mapping
(check
  (sequence (mapping (lambda (x) (* x 2))) '(1 2 3 4 5))
  => '(2 4 6 8 10))

;; dropping
(check (sequence (dropping 2) '(1 2 3 4 5)) => '(3 4 5))

;; into with list destination
(check (into '() (filtering odd?) '(1 2 3 4 5)) => '(1 3 5))

;; Sum via transduce + rf-sum factory
(let ([sum-rf (rf-sum)])
  (check
    (transduce (mapping (lambda (x) (* x x))) sum-rf (sum-rf) '(1 2 3 4))
    => 30))

;;; ──────────────────────────────────────────────────────────────
;;; 2. Structured logging (Jerboa Phase 4 enhancement)
;;; ──────────────────────────────────────────────────────────────
(printf "--- Structured logging tests ---~n")

;; make-logger takes a level ('debug, 'info, 'warn, 'error, 'fatal)
;; make-logger takes a level and optional key-value fields
(let ([logger (make-logger 'debug)])
  (check-true (logger? logger))
  (check (logger-level logger) => 'debug))

(let ([logger (make-logger 'info 'component 'jsh)])
  (check-true (logger? logger))
  (check (logger-level logger) => 'info))

;; log-level? validates known levels
(check-true (log-level? 'debug))
(check-true (log-level? 'info))
(check-true (log-level? 'warn))
(check-true (log-level? 'error))
(check-true (log-level? 'fatal))
(check-true (not (log-level? 'trace)))

;;; ──────────────────────────────────────────────────────────────
;;; 3. History module tests (uses transducer-based search)
;;; ──────────────────────────────────────────────────────────────
(printf "--- History module tests ---~n")

(import (only (jsh history)
  *history* history-init! history-add-raw! history-count
  history-list history-search history-unique-commands))

(history-init! "/dev/null" 500)
(check-true *history*)

;; history-add-raw! takes (timestamp cwd command)
(history-add-raw! 0 "" "echo hello")
(history-add-raw! 0 "" "ls -la")
(history-add-raw! 0 "" "git status")
(history-add-raw! 0 "" "echo world")
(history-add-raw! 0 "" "git commit -m 'test'")

(check (history-count) => 5)
(check-true (= (length (history-list)) 5))

;; Transducer-based prefix search
(check (history-search "echo") => '("echo hello" "echo world"))
(check (history-search "git") => '("git status" "git commit -m 'test'"))
(check (history-search "xyz") => '())

;; Dedup: add duplicate, search result should dedup
(history-add-raw! 0 "" "echo hello")
(let ([results (history-search "echo")])
  (check-true (member "echo hello" results))
  (check-true (member "echo world" results))
  ;; Deduplication should keep only one "echo hello"
  (check (length (filter (lambda (x) (string=? x "echo hello")) results)) => 1))

;; unique commands
(check-true (list? (history-unique-commands)))

;;; ──────────────────────────────────────────────────────────────
;;; 4. AST module tests
;;; ──────────────────────────────────────────────────────────────
(printf "--- AST module tests ---~n")

(import (only (jsh ast)
  make-simple-command simple-command?
  simple-command-words simple-command-assignments simple-command-redirections
  make-token token? token-type token-value))

;; Token construction
(let ([tok (make-token 'word "echo")])
  (check-true (token? tok))
  (check (token-type tok) => 'word)
  (check (token-value tok) => "echo"))

;; Simple command construction: (make-simple-command assignments words redirections)
(let ([cmd (make-simple-command '() (list (make-token 'word "echo") (make-token 'word "hello")) '())])
  (check-true (simple-command? cmd))
  (check-true (list? (simple-command-words cmd)))
  (check (length (simple-command-words cmd)) => 2))

;;; ──────────────────────────────────────────────────────────────
;;; 5. Lexer tests
;;; ──────────────────────────────────────────────────────────────
(printf "--- Lexer tests ---~n")

(import (jsh lexer))

(check-true (list? (tokenize "echo hello world")))
(check-true (>= (length (tokenize "echo hello world")) 3))
(check-true (null? (tokenize "")))
(check-true (list? (tokenize "'hello world'")))
(check-true (list? (tokenize "a|b")))

;;; ──────────────────────────────────────────────────────────────
;;; 6. Environment tests
;;; ──────────────────────────────────────────────────────────────
(printf "--- Environment tests ---~n")

(import (only (jsh environment)
  make-shell-environment shell-environment?
  env-set! env-get
  shell-environment-last-status env-set-last-status!))

(let ([env (make-shell-environment)])
  (check-true (shell-environment? env))
  (env-set! env "TESTVAR" "hello")
  (check (env-get env "TESTVAR") => "hello")
  (check (env-get env "NOTSET") => #f)
  (check (shell-environment-last-status env) => 0)
  (env-set-last-status! env 42)
  (check (shell-environment-last-status env) => 42))

;;; ──────────────────────────────────────────────────────────────
;;; 7. Arithmetic tests
;;; ──────────────────────────────────────────────────────────────
(printf "--- Arithmetic tests ---~n")

(import (only (jsh arithmetic) arith-eval))
(import (only (jsh environment) arith-env-getter arith-env-setter))

(let* ([env (make-shell-environment)]
       [get-fn (arith-env-getter env)]
       [set-fn (arith-env-setter env)])
  (check (arith-eval "1+1" get-fn set-fn) => 2)
  (check (arith-eval "10-3" get-fn set-fn) => 7)
  (check (arith-eval "3*4" get-fn set-fn) => 12)
  (check (arith-eval "10/2" get-fn set-fn) => 5)
  (check (arith-eval "2**8" get-fn set-fn) => 256)
  (check (arith-eval "7%3" get-fn set-fn) => 1)
  (check (arith-eval "5&3" get-fn set-fn) => 1)
  (check (arith-eval "5|2" get-fn set-fn) => 7)
  (check (arith-eval "5^3" get-fn set-fn) => 6)
  (check (arith-eval "3>2" get-fn set-fn) => 1)
  (check (arith-eval "2>3" get-fn set-fn) => 0)
  (check (arith-eval "3==3" get-fn set-fn) => 1))

;;; ──────────────────────────────────────────────────────────────
;;; 8. Glob tests
;;; ──────────────────────────────────────────────────────────────
(printf "--- Glob tests ---~n")

(import (only (jsh glob) glob-expand glob-pattern? glob-match?))

(check-true (list? (glob-expand "/tmp")))
(check-true (list? (glob-expand "/tmp/*")))
(check-true (list? (glob-expand "/tmp/this-does-not-exist-xyz-*")))

;;; ──────────────────────────────────────────────────────────────
;;; 9. Fuzzy match tests
;;; ──────────────────────────────────────────────────────────────
(printf "--- Fuzzy tests ---~n")

(import (only (jsh fuzzy) fuzzy-match-score fuzzy-match?))

(check-true (number? (fuzzy-match-score "git" "git")))
(check-true (> (fuzzy-match-score "git" "git") 0))
(check-true (> (fuzzy-match-score "gi" "git") 0))

;;; ──────────────────────────────────────────────────────────────
;;; 10. Registry tests
;;; ──────────────────────────────────────────────────────────────
(printf "--- Registry tests ---~n")

(import (only (jsh registry) *gsh-tier*))

(check-true (string? (*gsh-tier*)))

;;; ─────────────────────────────────────
;;; Summary
;;; ─────────────────────────────────────
(printf "~njsh unit tests: ~a passed, ~a failed~n" pass-count fail-count)
(when (> fail-count 0) (exit 1))
