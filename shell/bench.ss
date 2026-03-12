(export run-benchmarks)

(import :std/sugar
:std/format
        :std/srfi/1)

;; --- Timing ---

(def (fmt-secs s)
  (let ((ms (inexact->exact (round (* s 1000)))))
    (string-append (number->string ms) "ms")))

(def (time-thunk label thunk)
  (let ((start (##process-statistics)))
    (let ((result (thunk)))
      (let* ((end-stats (##process-statistics))
             (wall (- (f64vector-ref end-stats 2) (f64vector-ref start 2)))
             (user (- (f64vector-ref end-stats 0) (f64vector-ref start 0)))
             (gc   (- (f64vector-ref end-stats 5) (f64vector-ref start 5))))
        (fprintf (current-error-port)
                 "  ~a  ~a wall  ~a cpu  ~a gc~n"
                 label (fmt-secs wall) (fmt-secs user) (fmt-secs gc))
        result))))

;; --- Benchmarks ---

;; 1. Fibonacci — function call overhead
(def (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

;; 2. Ackermann — deep recursion + stack pressure
(def (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

;; 3. String building — allocation + GC pressure
(def (string-build n)
  (let loop ((i 0) (acc ""))
    (if (>= i n) (string-length acc)
      (loop (+ i 1) (string-append acc "x")))))

;; 4. Hash table churn — insert + lookup
(def (hash-churn n)
  (let ((ht (make-hash-table)))
    (let loop ((i 0))
      (when (< i n)
        (hash-put! ht i (* i i))
        (loop (+ i 1))))
    (let loop ((i 0) (sum 0))
      (if (>= i n) sum
        (loop (+ i 1) (+ sum (hash-ref ht i)))))))

;; 5. List map + fold — cons/GC/higher-order
(def (list-ops n)
  (let ((lst (let loop ((i 0) (acc '()))
               (if (>= i n) acc (loop (+ i 1) (cons i acc))))))
    (foldl + 0 (map (lambda (x) (* x x)) lst))))

;; --- Runner ---

(def (run-benchmarks)
  (fprintf (current-error-port) "~n=== bench ===~n")
  (time-thunk "fib(40)      " (lambda () (fib 40)))
  (time-thunk "ack(3,11)    " (lambda () (ack 3 11)))
  (time-thunk "str-build 80k" (lambda () (string-build 80000)))
  (time-thunk "hash-churn 3M" (lambda () (hash-churn 3000000)))
  (time-thunk "list-ops 2M  " (lambda () (list-ops 2000000)))
  (fprintf (current-error-port) "=== done ===~n"))

;; Auto-run on load
(run-benchmarks)