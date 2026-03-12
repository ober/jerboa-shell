;;; jobs.ss — Job control for gsh

(export #t)
(import :std/sugar
        :std/format
        :std/iter
        :std/os/signal
        :gsh/ffi
        :gsh/util
        :gsh/signals
        :gsh/environment)

;;; --- Job structures ---

(defstruct job (id pgid processes status command-text notify?)
  transparent: #t)
;; status: 'running 'stopped 'done 'killed

(defstruct job-process (pid status port)
  transparent: #t)
;; status: 'running 'stopped 'exited 'signaled

;;; --- Job table ---

(def *job-table* [])       ;; list of jobs
(def *next-job-id* 1)
(def *current-job* #f)     ;; most recent job (%%/%)
(def *previous-job* #f)    ;; previous job (%-)

;; Add a new job to the table
;; processes: list of (pid . port) pairs
;; Returns the new job
(def (job-table-add! processes command-text (pgid #f))
  (let* ((id *next-job-id*)
         (job-procs (map (lambda (pp)
                           (make-job-process (car pp) 'running (cdr pp)))
                         processes))
         (job (make-job id
                        (or pgid (if (pair? processes) (caar processes) 0))
                        job-procs
                        'running
                        command-text
                        #t)))  ;; notify on completion
    (set! *next-job-id* (+ id 1))
    (set! *previous-job* *current-job*)
    (set! *current-job* id)
    (set! *job-table* (append *job-table* [job]))
    job))

;; Remove a job from the table
(def (job-table-remove! job-id)
  (set! *job-table*
    (filter (lambda (j) (not (= (job-id j) job-id))) *job-table*))
  (when (and *current-job* (= *current-job* job-id))
    (set! *current-job* *previous-job*)
    (set! *previous-job* #f)))

;; List all jobs
(def (job-table-list)
  *job-table*)

;; Get a job by ID, spec, or PID
(def (job-table-get spec)
  (cond
    ((integer? spec)
     (find (lambda (j) (= (job-id j) spec)) *job-table*))
    ((string? spec)
     (cond
       ;; %% or %+ = current job
       ((or (string=? spec "%%") (string=? spec "%+") (string=? spec "%"))
        (and *current-job* (job-table-get *current-job*)))
       ;; %- = previous job
       ((string=? spec "%-")
        (and *previous-job* (job-table-get *previous-job*)))
       ;; %N = job number N
       ((and (> (string-length spec) 1)
             (char=? (string-ref spec 0) #\%)
             (char-numeric? (string-ref spec 1)))
        (let ((n (string->number (substring spec 1 (string-length spec)))))
          (and n (job-table-get n))))
       ;; %string = job starting with string
       ((and (> (string-length spec) 1)
             (char=? (string-ref spec 0) #\%))
        (let ((prefix (substring spec 1 (string-length spec))))
          (find (lambda (j)
                  (and (>= (string-length (job-command-text j))
                           (string-length prefix))
                       (string=? (substring (job-command-text j)
                                           0 (string-length prefix))
                                 prefix)))
                *job-table*)))
       ;; %?string = job containing string
       ((and (> (string-length spec) 2)
             (char=? (string-ref spec 0) #\%)
             (char=? (string-ref spec 1) #\?))
        (let ((substr (substring spec 2 (string-length spec))))
          (find (lambda (j)
                  (string-contains-substr? (job-command-text j) substr))
                *job-table*)))
       ;; Plain number = look up by PID (check pgid and individual process PIDs)
       ((string->number spec)
        => (lambda (pid)
             (or (find (lambda (j) (= (job-pgid j) pid)) *job-table*)
                 (find (lambda (j)
                         (any (lambda (p) (= (job-process-pid p) pid))
                              (job-processes j)))
                       *job-table*))))
       (else #f)))
    (else #f)))

;;; --- Job status management ---

;; Update status of all jobs by polling with waitpid (or checking thread state)
(def (job-update-status!)
  (for-each
   (lambda (job)
     (for-each
      (lambda (proc)
        (when (eq? (job-process-status proc) 'running)
          (let ((port (job-process-port proc)))
            (if (and port (thread? port))
              ;; Thread-based job: check if thread has terminated
              (let ((state (thread-state port)))
                (when (or (thread-state-normally-terminated? state)
                          (thread-state-abnormally-terminated? state))
                  (set! (job-process-status proc) 'exited)))
              ;; Process-based job: poll with waitpid
              (let ((result (ffi-waitpid-pid (job-process-pid proc)
                                              (bitwise-ior WNOHANG WUNTRACED))))
                (when (> result 0)
                  (let ((raw-status (ffi-waitpid-status)))
                    (cond
                      ((WIFEXITED raw-status)
                       (set! (job-process-status proc) 'exited))
                      ((WIFSIGNALED raw-status)
                       (set! (job-process-status proc) 'signaled))
                      ((WIFSTOPPED raw-status)
                       (set! (job-process-status proc) 'stopped))))))))))
      (job-processes job))
     ;; Update overall job status
     (let ((procs (job-processes job)))
       (cond
         ((every (lambda (p) (memq (job-process-status p) '(exited signaled))) procs)
          (set! (job-status job)
            (if (every (lambda (p) (eq? (job-process-status p) 'exited)) procs)
              'done 'killed)))
         ((any (lambda (p) (eq? (job-process-status p) 'stopped)) procs)
          (set! (job-status job) 'stopped)))))
   *job-table*))

;; Print notifications for completed/stopped jobs
(def (job-notify!)
  (job-update-status!)
  (for-each
   (lambda (job)
     (when (and (job-notify? job)
                (memq (job-status job) '(done killed)))
       (fprintf (current-error-port) "[~a]  ~a    ~a~n"
                (job-id job)
                (case (job-status job)
                  ((done) "Done")
                  ((killed) "Killed")
                  ((stopped) "Stopped")
                  (else "???"))
                (job-command-text job))
       (set! (job-notify? job) #f)))
   *job-table*)
  ;; Remove completed jobs
  (set! *job-table*
    (filter (lambda (j) (not (memq (job-status j) '(done killed))))
            *job-table*)))

;; Remove completed/killed jobs from the table (silent, no notifications)
(def (job-table-cleanup!)
  (set! *job-table*
    (filter (lambda (j) (not (memq (job-status j) '(done killed))))
            *job-table*)))

;; Wait for a foreground process launched via ffi-fork-exec (raw PID, no Gambit port).
;; Same polling logic as wait-for-foreground-process but without ECHILD fallback
;; (Gambit doesn't know about these children so there's no reaping race).
;; Returns (values exit-code stopped?)
(def (wait-for-foreground-process-raw pid)
  (let loop ((delay 0.001))
    (let ((result (ffi-waitpid-pid pid (bitwise-ior WNOHANG WUNTRACED))))
      (cond
        ((> result 0)
         (let ((raw (ffi-waitpid-status)))
           (cond
             ((WIFSTOPPED raw)
              (values (+ 128 (WSTOPSIG raw)) #t))
             ((WIFEXITED raw)
              (values (WEXITSTATUS raw) #f))
             ((WIFSIGNALED raw)
              (values (+ 128 (WTERMSIG raw)) #f))
             (else
              (values 0 #f)))))
        ((= result 0)
         ;; Still running — sleep and poll again
         (thread-sleep! delay)
         (thread-yield!)
         (let ((pending (pending-signals!)))
           (for-each
            (lambda (sig-name)
              (cond
                ((or (string=? sig-name "TERM") (string=? sig-name "INT"))
                 (let ((signum (signal-name->number sig-name)))
                   (when signum
                     (kill (- pid) signum))))
                (else
                 (set! *pending-signals* (cons sig-name *pending-signals*)))))
            pending))
         (loop (min 0.05 (* delay 1.5))))
        (else
         ;; ECHILD — should not happen for ffi-fork-exec children
         ;; but handle gracefully
         (values 0 #f))))))

;; Wait for a foreground process, supporting Ctrl-Z (SIGTSTP detection).
;; Uses polling waitpid with WUNTRACED since Gambit's process-status
;; doesn't report stopped processes.
;; Returns (values exit-code stopped?)
(def (wait-for-foreground-process pid proc)
  (let loop ((delay 0.001))  ;; start at 1ms, back off to 50ms
    (let ((result (ffi-waitpid-pid pid (bitwise-ior WNOHANG WUNTRACED))))
      (cond
        ((> result 0)
         ;; Process state changed
         (let ((raw (ffi-waitpid-status)))
           (cond
             ((WIFSTOPPED raw)
              (values (+ 128 (WSTOPSIG raw)) #t))
             ((WIFEXITED raw)
              (values (WEXITSTATUS raw) #f))
             ((WIFSIGNALED raw)
              (values (+ 128 (WTERMSIG raw)) #f))
             (else
              (values 0 #f)))))
        ((= result 0)
         ;; Still running — sleep and poll again
         (thread-sleep! delay)
         ;; Check for terminating signals (TERM, INT, etc) and forward to child
         (thread-yield!)
         (let ((pending (pending-signals!)))
           (for-each
            (lambda (sig-name)
              (cond
                ;; TERM or INT: forward to child process
                ((or (string=? sig-name "TERM") (string=? sig-name "INT"))
                 (let ((signum (signal-name->number sig-name)))
                   (when signum
                     ;; Send signal to child's process group (negative PID)
                     (kill (- pid) signum))))
                ;; Other signals: re-queue for later processing
                (else
                 (set! *pending-signals* (cons sig-name *pending-signals*)))))
            pending))
         (loop (min 0.05 (* delay 1.5))))
        (else
         ;; ECHILD — Gambit already reaped the child via its internal handler.
         ;; process-status should return immediately with the stored status.
         (let ((raw (with-catch (lambda (e) 0)
                      (lambda () (process-status proc)))))
           (values (status->exit-code raw) #f)))))))

;; Wait for a specific job to complete
;; Returns exit status of last process
(def (job-wait job)
  (let ((last-exit-code 0))
    (for-each
     (lambda (proc)
       (when (eq? (job-process-status proc) 'running)
         (let ((port (job-process-port proc)))
           (cond
             ((and port (thread? port))
              ;; Thread-based job (builtin/function/compound command)
              (let ((result (with-catch (lambda (e) 1)
                              (lambda () (thread-join! port)))))
                (set! last-exit-code (if (integer? result) result 0))
                (set! (job-process-status proc) 'exited)))
             ((and port (port? port))
              ;; Process-based job via open-process (Gambit port)
              (let ((raw (with-catch (lambda (e) 0)
                           (lambda () (process-status port)))))
                (set! last-exit-code (status->exit-code raw))
                (cond
                  ((WIFEXITED raw)
                   (set! (job-process-status proc) 'exited))
                  ((WIFSIGNALED raw)
                   (set! (job-process-status proc) 'signaled)
                   (let ((desc (signal-description (WTERMSIG raw))))
                     (when desc
                       (fprintf (current-error-port) "~a~n" desc))))
                  (else
                   (set! (job-process-status proc) 'exited)))))
             (else
              ;; ffi-fork-exec'd process (port=#f): block with waitpid polling
              ;; Block SIGCHLD to prevent Gambit's handler from reaping our child
              (ffi-sigchld-block)
              (let ((pid (job-process-pid proc)))
                (let loop ((delay 0.001))
                  (let ((result (ffi-waitpid-pid pid (bitwise-ior WNOHANG WUNTRACED))))
                    (cond
                      ((> result 0)
                       (let ((raw (ffi-waitpid-status)))
                         (ffi-sigchld-unblock)
                         (cond
                           ((WIFSTOPPED raw)
                            (set! (job-process-status proc) 'stopped)
                            (set! last-exit-code (+ 128 (WSTOPSIG raw))))
                           ((WIFEXITED raw)
                            (set! (job-process-status proc) 'exited)
                            (set! last-exit-code (WEXITSTATUS raw)))
                           ((WIFSIGNALED raw)
                            (set! (job-process-status proc) 'signaled)
                            (set! last-exit-code (+ 128 (WTERMSIG raw)))
                            (let ((desc (signal-description (WTERMSIG raw))))
                              (when desc
                                (fprintf (current-error-port) "~a~n" desc))))
                           (else
                            (set! (job-process-status proc) 'exited)
                            (set! last-exit-code 0)))))
                      ((= result 0)
                       (thread-sleep! delay)
                       (loop (min 0.05 (* delay 1.5))))
                      (else
                       (ffi-sigchld-unblock)
                       (set! (job-process-status proc) 'exited)
                       (set! last-exit-code 0)))))))))))
     (job-processes job))
    ;; Update overall job status
    (let ((procs (job-processes job)))
      (cond
        ((every (lambda (p) (memq (job-process-status p) '(exited signaled))) procs)
         (set! (job-status job)
           (if (every (lambda (p) (eq? (job-process-status p) 'exited)) procs)
             'done 'killed)))
        ((any (lambda (p) (eq? (job-process-status p) 'stopped)) procs)
         (set! (job-status job) 'stopped))))
    ;; Return exit status
    last-exit-code))

;; Wait for the next job from a list to complete.
;; Returns the exit status of the first job that finishes.
;; Polls all jobs with backoff to detect which finishes first.
(def (job-wait-any jobs)
  (if (null? jobs)
    127  ;; no jobs
    ;; Filter to only running jobs
    (let ((running (filter (lambda (j) (eq? (job-status j) 'running)) jobs)))
      (if (null? running)
        ;; All done, return exit status of first done job
        (let ((done-job (find (lambda (j) (memq (job-status j) '(done killed))) jobs)))
          (if done-job (job-wait done-job) 127))
        ;; Poll running jobs until one finishes.
        ;; Use small fixed delay (no backoff) for responsive wait-n ordering.
        (let poll-loop ((n 0))
          (thread-yield!)
          (let jloop ((js running))
            (cond
              ((null? js)
               ;; None finished yet — brief sleep then retry
               (thread-sleep! 0.001)
               (poll-loop (+ n 1)))
              (else
               (let* ((job (car js))
                      (procs (job-processes job))
                      (finished? (job-check-finished? job)))
                 (if finished?
                   ;; This job finished — return its status
                   (job-wait job)
                   (jloop (cdr js))))))))))))

;; Non-blocking check if a job's processes are all done.
;; Returns #t if finished, #f if still running.
;; Side-effects: updates process status for finished processes.
(def (job-check-finished? job)
  (let ((procs (job-processes job)))
    (and (pair? procs)
         (every
          (lambda (proc)
            (or (memq (job-process-status proc) '(exited signaled))
                (let ((port (job-process-port proc)))
                  (cond
                    ((and port (thread? port))
                     ;; Thread: check if terminated
                     (let ((st (thread-state port)))
                       (or (thread-state-normally-terminated? st)
                           (thread-state-abnormally-terminated? st))))
                    ((and port (port? port))
                     ;; External process via open-process: try non-blocking waitpid
                     (let* ((pid (process-pid port))
                            (result (ffi-waitpid-pid pid WNOHANG)))
                       (cond
                         ((> result 0)
                          (let ((raw-status (ffi-waitpid-status)))
                            (set! (job-process-status proc)
                              (if (WIFSIGNALED raw-status) 'signaled 'exited))
                            #t))
                         ((< result 0)
                          ;; ECHILD: Gambit already reaped. Mark as finished;
                          ;; job-wait will use process-status to get exit code.
                          #t)
                         (else #f))))
                    (else
                     ;; ffi-fork-exec'd process (port=#f): use PID directly
                     (let* ((pid (job-process-pid proc))
                            (result (ffi-waitpid-pid pid WNOHANG)))
                       (cond
                         ((> result 0)
                          (let ((raw-status (ffi-waitpid-status)))
                            (set! (job-process-status proc)
                              (if (WIFSIGNALED raw-status) 'signaled 'exited))
                            #t))
                         ((< result 0) #t)  ;; ECHILD
                         (else #f))))))))
          procs))))

;; Bring a job to the foreground
(def (job-foreground! job)
  ;; Send SIGCONT if stopped
  (when (eq? (job-status job) 'stopped)
    (for-each
     (lambda (proc)
       (when (eq? (job-process-status proc) 'stopped)
         (kill (job-process-pid proc) SIGCONT)
         (set! (job-process-status proc) 'running)))
     (job-processes job))
    (set! (job-status job) 'running))
  ;; Set terminal foreground process group (if we have job control)
  (with-catch (lambda (e) #!void)
    (lambda ()
      (ffi-tcsetpgrp 0 (job-pgid job))))
  ;; Wait for the last process in the job using WUNTRACED-aware wait
  (let* ((procs (job-processes job))
         (last-proc (last-element procs))
         (pid (job-process-pid last-proc))
         (port (job-process-port last-proc)))
    (let-values (((exit-code stopped?)
                  (cond
                    ((and port (thread? port))
                     ;; Thread-based job — use job-wait
                     (values (job-wait job) #f))
                    (port
                     (wait-for-foreground-process pid port))
                    (else
                     ;; No port — use job-wait fallback
                     (values (job-wait job) #f)))))
      ;; Restore shell as foreground
      (with-catch (lambda (e) #!void)
        (lambda ()
          (ffi-tcsetpgrp 0 (##os-getpid))))
      (if stopped?
        ;; Ctrl-Z again: mark job as stopped
        (begin
          (set! (job-status job) 'stopped)
          (for-each (lambda (p)
                      (when (eq? (job-process-status p) 'running)
                        (set! (job-process-status p) 'stopped)))
                    procs)
          (fprintf (current-error-port) "~n[~a]+  Stopped                 ~a~n"
                   (job-id job) (job-command-text job))
          (+ 128 20))
        ;; Normal completion
        (begin
          ;; Mark all processes as exited
          (for-each (lambda (p)
                      (when (eq? (job-process-status p) 'running)
                        (set! (job-process-status p) 'exited)))
                    procs)
          (set! (job-status job) 'done)
          exit-code)))))

;; Resume a job in the background
(def (job-background! job)
  (when (eq? (job-status job) 'stopped)
    (for-each
     (lambda (proc)
       (when (eq? (job-process-status proc) 'stopped)
         (kill (job-process-pid proc) SIGCONT)
         (set! (job-process-status proc) 'running)))
     (job-processes job))
    (set! (job-status job) 'running))
  (fprintf (current-error-port) "[~a] ~a~n"
           (job-id job) (job-command-text job)))

;; Disown a job (remove from table, don't send SIGHUP)
(def (job-disown! job-id)
  (job-table-remove! job-id))

;; Get number of active jobs
(def (job-count)
  (length *job-table*))

;;; --- Helpers ---

(def (every pred lst)
  (cond
    ((null? lst) #t)
    ((pred (car lst)) (every pred (cdr lst)))
    (else #f)))

(def (any pred lst)
  (cond
    ((null? lst) #f)
    ((pred (car lst)) #t)
    (else (any pred (cdr lst)))))

(def (find pred lst)
  (cond
    ((null? lst) #f)
    ((pred (car lst)) (car lst))
    (else (find pred (cdr lst)))))

(def (last-element lst)
  (if (null? (cdr lst)) (car lst) (last-element (cdr lst))))

(def (string-contains-substr? haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? (substring haystack i (+ i nlen)) needle) #t)
        (else (loop (+ i 1)))))))
