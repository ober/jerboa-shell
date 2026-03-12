(export run-benchmarks-smp)

(import :std/sugar
        :std/format
        :std/srfi/1)

;; --- Timing ---

(def (fmt-secs s)
  (let ((ms (inexact->exact (round (* s 1000)))))
    (string-append (number->string ms) "ms")))

(def (log! msg)
  (display msg (current-error-port))
  (force-output (current-error-port)))

;; --- Benchmarks ---
;; Pure-CPU benchmarks that parallelize well on SMP.
;; Allocation-heavy benchmarks (hash, list, string) don't scale due to
;; Gambit's stop-the-world GC, so we focus on compute-bound workloads.

;; 1. Fibonacci — pure function calls
(def (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

;; 2. Ackermann — deep recursion + stack pressure
(def (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

;; 3. Tak — Takeuchi function, another classic CPU benchmark
(def (tak x y z)
  (if (<= x y) z
    (tak (tak (- x 1) y z)
         (tak (- y 1) z x)
         (tak (- z 1) x y))))

;; 4. Sum of collatz sequence lengths — compute + branching
(def (collatz-len n)
  (let loop ((n n) (len 0))
    (if (<= n 1) len
      (loop (if (even? n) (quotient n 2) (+ (* 3 n) 1))
            (+ len 1)))))

(def (collatz-sum limit)
  (let loop ((i 1) (sum 0))
    (if (> i limit) sum
      (loop (+ i 1) (+ sum (collatz-len i))))))

;; 5. Matrix multiply (vectors) — nested loops, arithmetic
(def (matmul-bench size)
  (let ((a (make-vector (* size size) 1.0))
        (b (make-vector (* size size) 2.0))
        (c (make-vector (* size size) 0.0)))
    (let loopi ((i 0))
      (when (< i size)
        (let loopj ((j 0))
          (when (< j size)
            (let loopk ((k 0) (sum 0.0))
              (if (< k size)
                (loopk (+ k 1)
                       (+ sum (* (vector-ref a (+ (* i size) k))
                                 (vector-ref b (+ (* k size) j)))))
                (vector-set! c (+ (* i size) j) sum)))
            (loopj (+ j 1))))
        (loopi (+ i 1))))
    (vector-ref c 0)))

;; --- Runner ---

(def num-cores 8)

(def (run-benchmarks-smp)
  ;; Enable SMP
  (##set-parallelism-level! num-cores)
  (##startup-parallelism!)
  (thread-sleep! 0.1)

  (log! (format "\n=== bench-smp (~a cores, ~a processors) ===\n"
                num-cores (##current-vm-processor-count)))

  (let ((benchmarks
         (list
          (cons "fib(38)      " (lambda () (fib 38)))
          (cons "ack(3,10)    " (lambda () (ack 3 10)))
          (cons "tak(30,20,10)" (lambda () (tak 30 20 10)))
          (cons "collatz 500k " (lambda () (collatz-sum 500000)))
          (cons "matmul 150   " (lambda () (matmul-bench 150))))))

    ;; Sequential baseline: run each benchmark num-cores times
    (log! "\n--- sequential (8x each) ---\n")
    (let ((seq-times
           (map (lambda (b)
                  (let ((start (##process-statistics)))
                    (let loop ((i 0))
                      (when (< i num-cores)
                        ((cdr b))
                        (loop (+ i 1))))
                    (let* ((end (##process-statistics))
                           (wall (- (f64vector-ref end 2) (f64vector-ref start 2))))
                      (log! (format "  ~a  ~a wall\n" (car b) (fmt-secs wall)))
                      (cons (car b) wall))))
                benchmarks)))

      ;; Parallel: 8 threads simultaneously
      (log! "\n--- parallel (8 threads) ---\n")
      (let ((par-times
             (map (lambda (b)
                    (let* ((start (##process-statistics))
                           (threads (map (lambda (i)
                                           (thread-start!
                                            (make-thread (cdr b))))
                                         (iota num-cores)))
                           (_ (for-each thread-join! threads))
                           (end (##process-statistics))
                           (wall (- (f64vector-ref end 2) (f64vector-ref start 2))))
                      (log! (format "  ~a  ~a wall\n" (car b) (fmt-secs wall)))
                      (cons (car b) wall)))
                  benchmarks)))

        ;; Summary
        (log! "\n--- speedup ---\n")
        (for-each
         (lambda (seq par)
           (let* ((s (cdr seq))
                  (p (cdr par))
                  (speedup (if (> p 0) (/ s p) 0.0)))
             (log! (format "  ~a  ~1,1fx\n" (car seq) speedup))))
         seq-times par-times))))

  (log! "=== done ===\n"))

;; Auto-run on load
(run-benchmarks-smp)
