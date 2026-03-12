#!chezscheme
(library (jsh jobs)
  (export job::t make-job job? job-id job-pgid job-processes
   job-status job-command-text job-notify? job-id-set!
   job-pgid-set! job-processes-set! job-status-set!
   job-command-text-set! job-notify?-set! &job-id &job-pgid
   &job-processes &job-status &job-command-text &job-notify?
   &job-id-set! &job-pgid-set! &job-processes-set!
   &job-status-set! &job-command-text-set! &job-notify?-set!
   job-process::t make-job-process job-process? job-process-pid
   job-process-status job-process-port job-process-pid-set!
   job-process-status-set! job-process-port-set!
   &job-process-pid &job-process-status &job-process-port
   &job-process-pid-set! &job-process-status-set!
   &job-process-port-set! *job-table* *next-job-id*
   *current-job* *previous-job* job-table-add!
   job-table-remove! job-table-list job-table-get
   job-update-status! job-notify! job-table-cleanup!
   wait-for-foreground-process-raw wait-for-foreground-process
   job-wait job-wait-any job-check-finished? job-foreground!
   job-background! job-disown! job-count every any find
   last-element string-contains-substr?)
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
    (except (runtime util) find last-pair iota \x31;- \x31;+
      displayln make-keyword)
    (except (compat gambit) number->string make-mutex
      with-output-to-string string->bytes bytes->string thread?)
    (except (std error) with-exception-handler error-trace
      error-irritants error-message)
    (except (std misc string) string-join string-split
      string-index string-empty?)
    (except (std misc list) any every take drop filter-map)
    (except (std misc alist) pget pgetv pgetq aget agetv agetq)
    (except
      (std os path)
      path-expand
      path-normalize
      path-absolute?)
    (except (std format) format) (std sort) (std pregexp)
    (std sugar) (std os signal) (jsh ffi)
    (except (jsh util) string-index string-join file-directory?
      string-join string-index string-downcase file-regular?
      string-upcase)
    (jsh signals) (jsh environment))
  (begin
    (define job::t
      (make-class-type 'gerbil\x23;job::t 'job (list object::t)
        '(id pgid processes status command-text notify?)
        '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-job . args)
      (let* ([type job::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (job? obj)
      (\x23;\x23;structure-instance-of? obj 'gerbil\x23;job::t))
    (define (job-id obj) (unchecked-slot-ref obj 'id))
    (define (job-pgid obj) (unchecked-slot-ref obj 'pgid))
    (define (job-processes obj)
      (unchecked-slot-ref obj 'processes))
    (define (job-status obj) (unchecked-slot-ref obj 'status))
    (define (job-command-text obj)
      (unchecked-slot-ref obj 'command-text))
    (define (job-notify? obj) (unchecked-slot-ref obj 'notify?))
    (define (job-id-set! obj val)
      (unchecked-slot-set! obj 'id val))
    (define (job-pgid-set! obj val)
      (unchecked-slot-set! obj 'pgid val))
    (define (job-processes-set! obj val)
      (unchecked-slot-set! obj 'processes val))
    (define (job-status-set! obj val)
      (unchecked-slot-set! obj 'status val))
    (define (job-command-text-set! obj val)
      (unchecked-slot-set! obj 'command-text val))
    (define (job-notify?-set! obj val)
      (unchecked-slot-set! obj 'notify? val))
    (define (&job-id obj) (unchecked-slot-ref obj 'id))
    (define (&job-pgid obj) (unchecked-slot-ref obj 'pgid))
    (define (&job-processes obj)
      (unchecked-slot-ref obj 'processes))
    (define (&job-status obj) (unchecked-slot-ref obj 'status))
    (define (&job-command-text obj)
      (unchecked-slot-ref obj 'command-text))
    (define (&job-notify? obj)
      (unchecked-slot-ref obj 'notify?))
    (define (&job-id-set! obj val)
      (unchecked-slot-set! obj 'id val))
    (define (&job-pgid-set! obj val)
      (unchecked-slot-set! obj 'pgid val))
    (define (&job-processes-set! obj val)
      (unchecked-slot-set! obj 'processes val))
    (define (&job-status-set! obj val)
      (unchecked-slot-set! obj 'status val))
    (define (&job-command-text-set! obj val)
      (unchecked-slot-set! obj 'command-text val))
    (define (&job-notify?-set! obj val)
      (unchecked-slot-set! obj 'notify? val)))
  (begin
    (define job-process::t
      (make-class-type 'gerbil\x23;job-process::t 'job-process (list object::t)
        '(pid status port) '((struct: . #t) (transparent: . #t))
        '#f))
    (define (make-job-process . args)
      (let* ([type job-process::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (job-process? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;job-process::t))
    (define (job-process-pid obj) (unchecked-slot-ref obj 'pid))
    (define (job-process-status obj)
      (unchecked-slot-ref obj 'status))
    (define (job-process-port obj)
      (unchecked-slot-ref obj 'port))
    (define (job-process-pid-set! obj val)
      (unchecked-slot-set! obj 'pid val))
    (define (job-process-status-set! obj val)
      (unchecked-slot-set! obj 'status val))
    (define (job-process-port-set! obj val)
      (unchecked-slot-set! obj 'port val))
    (define (&job-process-pid obj)
      (unchecked-slot-ref obj 'pid))
    (define (&job-process-status obj)
      (unchecked-slot-ref obj 'status))
    (define (&job-process-port obj)
      (unchecked-slot-ref obj 'port))
    (define (&job-process-pid-set! obj val)
      (unchecked-slot-set! obj 'pid val))
    (define (&job-process-status-set! obj val)
      (unchecked-slot-set! obj 'status val))
    (define (&job-process-port-set! obj val)
      (unchecked-slot-set! obj 'port val)))
  (define *job-table*-cell (vector (list)))
  (define-syntax *job-table*
    (identifier-syntax
      [id (vector-ref *job-table*-cell 0)]
      [(set! id v) (vector-set! *job-table*-cell 0 v)]))
  (define *next-job-id*-cell (vector 1))
  (define-syntax *next-job-id*
    (identifier-syntax
      [id (vector-ref *next-job-id*-cell 0)]
      [(set! id v) (vector-set! *next-job-id*-cell 0 v)]))
  (define *current-job*-cell (vector #f))
  (define-syntax *current-job*
    (identifier-syntax
      [id (vector-ref *current-job*-cell 0)]
      [(set! id v) (vector-set! *current-job*-cell 0 v)]))
  (define *previous-job*-cell (vector #f))
  (define-syntax *previous-job*
    (identifier-syntax
      [id (vector-ref *previous-job*-cell 0)]
      [(set! id v) (vector-set! *previous-job*-cell 0 v)]))
  (define job-table-add!
    (case-lambda
      [(processes command-text)
       (let* ([pgid #f])
         (let* ([id *next-job-id*])
           (let* ([job-procs (map (lambda (pp)
                                    (make-job-process
                                      (car pp)
                                      'running
                                      (cdr pp)))
                                  processes)])
             (let* ([job (make-job id
                           (or pgid
                               (if (pair? processes) (caar processes) 0))
                           job-procs 'running command-text #t)])
               (set! *next-job-id* (+ id 1))
               (set! *previous-job* *current-job*)
               (set! *current-job* id)
               (set! *job-table* (append *job-table* (list job)))
               job))))]
      [(processes command-text pgid)
       (let* ([id *next-job-id*])
         (let* ([job-procs (map (lambda (pp)
                                  (make-job-process
                                    (car pp)
                                    'running
                                    (cdr pp)))
                                processes)])
           (let* ([job (make-job id
                         (or pgid
                             (if (pair? processes) (caar processes) 0))
                         job-procs 'running command-text #t)])
             (set! *next-job-id* (+ id 1))
             (set! *previous-job* *current-job*)
             (set! *current-job* id)
             (set! *job-table* (append *job-table* (list job)))
             job)))]))
  (define (job-table-remove! job-id)
    (set! *job-table*
      (filter
        (lambda (j) (not (= (job-id j) job-id)))
        *job-table*))
    (when (and *current-job* (= *current-job* job-id))
      (set! *current-job* *previous-job*)
      (set! *previous-job* #f)))
  (define (job-table-list) *job-table*)
  (define (job-table-get spec)
    (cond
      [(integer? spec)
       (find (lambda (j) (= (job-id j) spec)) *job-table*)]
      [(string? spec)
       (cond
         [(or (string=? spec "%%")
              (string=? spec "%+")
              (string=? spec "%"))
          (and *current-job* (job-table-get *current-job*))]
         [(string=? spec "%-")
          (and *previous-job* (job-table-get *previous-job*))]
         [(and (> (string-length spec) 1)
               (char=? (string-ref spec 0) #\%)
               (char-numeric? (string-ref spec 1)))
          (let ([n (string->number
                     (substring spec 1 (string-length spec)))])
            (and n (job-table-get n)))]
         [(and (> (string-length spec) 1)
               (char=? (string-ref spec 0) #\%))
          (let ([prefix (substring spec 1 (string-length spec))])
            (find
              (lambda (j)
                (and (>= (string-length (job-command-text j))
                         (string-length prefix))
                     (string=?
                       (substring
                         (job-command-text j)
                         0
                         (string-length prefix))
                       prefix)))
              *job-table*))]
         [(and (> (string-length spec) 2)
               (char=? (string-ref spec 0) #\%)
               (char=? (string-ref spec 1) #\?))
          (let ([substr (substring spec 2 (string-length spec))])
            (find
              (lambda (j)
                (string-contains-substr? (job-command-text j) substr))
              *job-table*))]
         [(string->number spec) =>
          (lambda (pid)
            (or (find (lambda (j) (= (job-pgid j) pid)) *job-table*)
                (find
                  (lambda (j)
                    (any (lambda (p) (= (job-process-pid p) pid))
                         (job-processes j)))
                  *job-table*)))]
         [else #f])]
      [else #f]))
  (define (job-update-status!)
    (for-each
      (lambda (job)
        (for-each
          (lambda (proc)
            (when (eq? (job-process-status proc) 'running)
              (let ([port (job-process-port proc)])
                (if (and port (thread? port))
                    (let ([state (thread-state port)])
                      (when (or (thread-state-normally-terminated? state)
                                (thread-state-abnormally-terminated?
                                  state))
                        (job-process-status-set! proc 'exited)))
                    (let ([result (ffi-waitpid-pid
                                    (job-process-pid proc)
                                    (bitwise-ior WNOHANG WUNTRACED))])
                      (when (> result 0)
                        (let ([raw-status (ffi-waitpid-status)])
                          (cond
                            [(WIFEXITED raw-status)
                             (job-process-status-set! proc 'exited)]
                            [(WIFSIGNALED raw-status)
                             (job-process-status-set! proc 'signaled)]
                            [(WIFSTOPPED raw-status)
                             (job-process-status-set!
                               proc
                               'stopped)]))))))))
          (job-processes job))
        (let ([procs (job-processes job)])
          (cond
            [(every
               (lambda (p)
                 (memq (job-process-status p) '(exited signaled)))
               procs)
             (job-status-set!
               job
               (if (every
                     (lambda (p) (eq? (job-process-status p) 'exited))
                     procs)
                   'done
                   'killed))]
            [(any (lambda (p) (eq? (job-process-status p) 'stopped))
                  procs)
             (job-status-set! job 'stopped)])))
      *job-table*))
  (define (job-notify!)
    (job-update-status!)
    (for-each
      (lambda (job)
        (when (and (job-notify? job)
                   (memq (job-status job) '(done killed)))
          (fprintf (current-error-port) "[~a]  ~a    ~a~n" (job-id job)
            (case (job-status job)
              [(done) "Done"]
              [(killed) "Killed"]
              [(stopped) "Stopped"]
              [else "???"])
            (job-command-text job))
          (job-notify?-set! job #f)))
      *job-table*)
    (set! *job-table*
      (filter
        (lambda (j) (not (memq (job-status j) '(done killed))))
        *job-table*)))
  (define (job-table-cleanup!)
    (set! *job-table*
      (filter
        (lambda (j) (not (memq (job-status j) '(done killed))))
        *job-table*)))
  (define (wait-for-foreground-process-raw pid)
    (let loop ([delay 0.001])
      (let ([result (ffi-waitpid-pid
                      pid
                      (bitwise-ior WNOHANG WUNTRACED))])
        (cond
          [(> result 0)
           (let ([raw (ffi-waitpid-status)])
             (cond
               [(WIFSTOPPED raw) (values (+ 128 (WSTOPSIG raw)) #t)]
               [(WIFEXITED raw) (values (WEXITSTATUS raw) #f)]
               [(WIFSIGNALED raw) (values (+ 128 (WTERMSIG raw)) #f)]
               [else (values 0 #f)]))]
          [(= result 0)
           (thread-sleep! delay)
           (thread-yield!)
           (let ([pending (pending-signals!)])
             (for-each
               (lambda (sig-name)
                 (cond
                   [(or (string=? sig-name "TERM")
                        (string=? sig-name "INT"))
                    (let ([signum (signal-name->number sig-name)])
                      (when signum (kill (- pid) signum)))]
                   [else
                    (set! *pending-signals*
                      (cons sig-name *pending-signals*))]))
               pending))
           (loop (min 0.05 (* delay 1.5)))]
          [else (values 0 #f)]))))
  (define (wait-for-foreground-process pid proc)
    (let loop ([delay 0.001])
      (let ([result (ffi-waitpid-pid
                      pid
                      (bitwise-ior WNOHANG WUNTRACED))])
        (cond
          [(> result 0)
           (let ([raw (ffi-waitpid-status)])
             (cond
               [(WIFSTOPPED raw) (values (+ 128 (WSTOPSIG raw)) #t)]
               [(WIFEXITED raw) (values (WEXITSTATUS raw) #f)]
               [(WIFSIGNALED raw) (values (+ 128 (WTERMSIG raw)) #f)]
               [else (values 0 #f)]))]
          [(= result 0)
           (thread-sleep! delay)
           (thread-yield!)
           (let ([pending (pending-signals!)])
             (for-each
               (lambda (sig-name)
                 (cond
                   [(or (string=? sig-name "TERM")
                        (string=? sig-name "INT"))
                    (let ([signum (signal-name->number sig-name)])
                      (when signum (kill (- pid) signum)))]
                   [else
                    (set! *pending-signals*
                      (cons sig-name *pending-signals*))]))
               pending))
           (loop (min 0.05 (* delay 1.5)))]
          [else
           (let ([raw (guard (__exn [#t ((lambda (e) 0) __exn)])
                        (process-status proc))])
             (values (status->exit-code raw) #f))]))))
  (define (job-wait job)
    (let ([last-exit-code 0])
      (for-each
        (lambda (proc)
          (when (eq? (job-process-status proc) 'running)
            (let ([port (job-process-port proc)])
              (cond
                [(and port (thread? port))
                 (let ([result (guard (__exn [#t ((lambda (e) 1) __exn)])
                                 (thread-join! port))])
                   (set! last-exit-code (if (integer? result) result 0))
                   (job-process-status-set! proc 'exited))]
                [(and port (port? port))
                 (let ([raw (guard (__exn [#t ((lambda (e) 0) __exn)])
                              (process-status port))])
                   (set! last-exit-code (status->exit-code raw))
                   (cond
                     [(WIFEXITED raw)
                      (job-process-status-set! proc 'exited)]
                     [(WIFSIGNALED raw)
                      (job-process-status-set! proc 'signaled)
                      (let ([desc (signal-description (WTERMSIG raw))])
                        (when desc
                          (fprintf (current-error-port) "~a~n" desc)))]
                     [else (job-process-status-set! proc 'exited)]))]
                [else
                 (ffi-sigchld-block)
                 (let ([pid (job-process-pid proc)])
                   (let loop ([delay 0.001])
                     (let ([result (ffi-waitpid-pid
                                     pid
                                     (bitwise-ior WNOHANG WUNTRACED))])
                       (cond
                         [(> result 0)
                          (let ([raw (ffi-waitpid-status)])
                            (ffi-sigchld-unblock)
                            (cond
                              [(WIFSTOPPED raw)
                               (job-process-status-set! proc 'stopped)
                               (set! last-exit-code
                                 (+ 128 (WSTOPSIG raw)))]
                              [(WIFEXITED raw)
                               (job-process-status-set! proc 'exited)
                               (set! last-exit-code (WEXITSTATUS raw))]
                              [(WIFSIGNALED raw)
                               (job-process-status-set! proc 'signaled)
                               (set! last-exit-code (+ 128 (WTERMSIG raw)))
                               (let ([desc (signal-description
                                             (WTERMSIG raw))])
                                 (when desc
                                   (fprintf
                                     (current-error-port)
                                     "~a~n"
                                     desc)))]
                              [else
                               (job-process-status-set! proc 'exited)
                               (set! last-exit-code 0)]))]
                         [(= result 0)
                          (thread-sleep! delay)
                          (loop (min 0.05 (* delay 1.5)))]
                         [else
                          (ffi-sigchld-unblock)
                          (job-process-status-set! proc 'exited)
                          (set! last-exit-code 0)]))))]))))
        (job-processes job))
      (let ([procs (job-processes job)])
        (cond
          [(every
             (lambda (p)
               (memq (job-process-status p) '(exited signaled)))
             procs)
           (job-status-set!
             job
             (if (every
                   (lambda (p) (eq? (job-process-status p) 'exited))
                   procs)
                 'done
                 'killed))]
          [(any (lambda (p) (eq? (job-process-status p) 'stopped))
                procs)
           (job-status-set! job 'stopped)]))
      last-exit-code))
  (define (job-wait-any jobs)
    (if (null? jobs)
        127
        (let ([running (filter
                         (lambda (j) (eq? (job-status j) 'running))
                         jobs)])
          (if (null? running)
              (let ([done-job (find
                                (lambda (j)
                                  (memq (job-status j) '(done killed)))
                                jobs)])
                (if done-job (job-wait done-job) 127))
              (let poll-loop ([n 0])
                (thread-yield!)
                (let jloop ([js running])
                  (cond
                    [(null? js) (thread-sleep! 0.001) (poll-loop (+ n 1))]
                    [else
                     (let* ([job (car js)])
                       (let* ([procs (job-processes job)])
                         (let* ([finished? (job-check-finished? job)])
                           (if finished?
                               (job-wait job)
                               (jloop (cdr js))))))])))))))
  (define (job-check-finished? job)
    (let ([procs (job-processes job)])
      (and (pair? procs)
           (every
             (lambda (proc)
               (or (memq (job-process-status proc) '(exited signaled))
                   (let ([port (job-process-port proc)])
                     (cond
                       [(and port (thread? port))
                        (let ([st (thread-state port)])
                          (or (thread-state-normally-terminated? st)
                              (thread-state-abnormally-terminated? st)))]
                       [(and port (port? port))
                        (let* ([pid (process-pid port)])
                          (let* ([result (ffi-waitpid-pid pid WNOHANG)])
                            (cond
                              [(> result 0)
                               (let ([raw-status (ffi-waitpid-status)])
                                 (job-process-status-set!
                                   proc
                                   (if (WIFSIGNALED raw-status)
                                       'signaled
                                       'exited))
                                 #t)]
                              [(< result 0) #t]
                              [else #f])))]
                       [else
                        (let* ([pid (job-process-pid proc)])
                          (let* ([result (ffi-waitpid-pid pid WNOHANG)])
                            (cond
                              [(> result 0)
                               (let ([raw-status (ffi-waitpid-status)])
                                 (job-process-status-set!
                                   proc
                                   (if (WIFSIGNALED raw-status)
                                       'signaled
                                       'exited))
                                 #t)]
                              [(< result 0) #t]
                              [else #f])))]))))
             procs))))
  (define (job-foreground! job)
    (when (eq? (job-status job) 'stopped)
      (for-each
        (lambda (proc)
          (when (eq? (job-process-status proc) 'stopped)
            (kill (job-process-pid proc) SIGCONT)
            (job-process-status-set! proc 'running)))
        (job-processes job))
      (job-status-set! job 'running))
    (guard (__exn [#t ((lambda (e) (%%void)) __exn)])
      (ffi-tcsetpgrp 0 (job-pgid job)))
    (let* ([procs (job-processes job)])
      (let* ([last-proc (last-element procs)])
        (let* ([pid (job-process-pid last-proc)])
          (let* ([port (job-process-port last-proc)])
            (let-values ([(exit-code stopped?)
                          (cond
                            [(and port (thread? port))
                             (values (job-wait job) #f)]
                            [port (wait-for-foreground-process pid port)]
                            [else (values (job-wait job) #f)])])
              (guard (__exn [#t ((lambda (e) (%%void)) __exn)])
                (ffi-tcsetpgrp 0 (ffi-getpid)))
              (if stopped?
                  (begin
                    (job-status-set! job 'stopped)
                    (for-each
                      (lambda (p)
                        (when (eq? (job-process-status p) 'running)
                          (job-process-status-set! p 'stopped)))
                      procs)
                    (fprintf
                      (current-error-port)
                      "~n[~a]+  Stopped                 ~a~n"
                      (job-id job)
                      (job-command-text job))
                    (+ 128 20))
                  (begin
                    (for-each
                      (lambda (p)
                        (when (eq? (job-process-status p) 'running)
                          (job-process-status-set! p 'exited)))
                      procs)
                    (job-status-set! job 'done)
                    exit-code))))))))
  (define (job-background! job)
    (when (eq? (job-status job) 'stopped)
      (for-each
        (lambda (proc)
          (when (eq? (job-process-status proc) 'stopped)
            (kill (job-process-pid proc) SIGCONT)
            (job-process-status-set! proc 'running)))
        (job-processes job))
      (job-status-set! job 'running))
    (fprintf
      (current-error-port)
      "[~a] ~a~n"
      (job-id job)
      (job-command-text job)))
  (define (job-disown! job-id) (job-table-remove! job-id))
  (define (job-count) (length *job-table*))
  (define (every pred lst)
    (cond
      [(null? lst) #t]
      [(pred (car lst)) (every pred (cdr lst))]
      [else #f]))
  (define (any pred lst)
    (cond
      [(null? lst) #f]
      [(pred (car lst)) #t]
      [else (any pred (cdr lst))]))
  (define (find pred lst)
    (cond
      [(null? lst) #f]
      [(pred (car lst)) (car lst)]
      [else (find pred (cdr lst))]))
  (define (last-element lst)
    (if (null? (cdr lst)) (car lst) (last-element (cdr lst))))
  (define (string-contains-substr? haystack needle)
    (let ([hlen (string-length haystack)]
          [nlen (string-length needle)])
      (let loop ([i 0])
        (cond
          [(> (+ i nlen) hlen) #f]
          [(string=? (substring haystack i (+ i nlen)) needle) #t]
          [else (loop (+ i 1))])))))
