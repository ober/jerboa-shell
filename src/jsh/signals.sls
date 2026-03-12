#!chezscheme
(library (jsh signals)
  (export trap-entry::t make-trap-entry trap-entry?
   trap-entry-signal trap-entry-action trap-entry-signal-set!
   trap-entry-action-set! &trap-entry-signal &trap-entry-action
   &trap-entry-signal-set! &trap-entry-action-set! *trap-table*
   *signal-names* *pseudo-signals* *signal-number-to-name*
   normalize-signal-arg signal-display-name signal-name->number
   *signal-descriptions* signal-description trap-set! trap-get
   trap-list has-signal-traps? *pending-signals*
   *flag-trapped-signals* pending-signals!
   clear-pending-signal! signal-number->name
   *initially-ignored-signals* setup-default-signal-handlers!
   setup-noninteractive-signal-handlers! with-signal-context
   signal-name-list)
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
    (std sugar) (std os signal) (jsh ffi)
    (except (jsh util) string-index string-join file-directory?
      string-join string-index string-downcase file-regular?
      string-upcase))
  (begin
    (define trap-entry::t
      (make-class-type 'gerbil\x23;trap-entry::t 'trap-entry (list object::t)
        '(signal action) '((struct: . #t) (transparent: . #t)) '#f))
    (define (make-trap-entry . args)
      (let* ([type trap-entry::t]
             [n (class-type-field-count type)]
             [obj (apply \x23;\x23;structure type (make-list n #f))])
        (let lp ([rest args] [i 1])
          (when (and (pair? rest) (<= i n))
            (\x23;\x23;structure-set! obj i (car rest))
            (lp (cdr rest) (+ i 1))))
        obj))
    (define (trap-entry? obj)
      (\x23;\x23;structure-instance-of?
        obj
        'gerbil\x23;trap-entry::t))
    (define (trap-entry-signal obj)
      (unchecked-slot-ref obj 'signal))
    (define (trap-entry-action obj)
      (unchecked-slot-ref obj 'action))
    (define (trap-entry-signal-set! obj val)
      (unchecked-slot-set! obj 'signal val))
    (define (trap-entry-action-set! obj val)
      (unchecked-slot-set! obj 'action val))
    (define (&trap-entry-signal obj)
      (unchecked-slot-ref obj 'signal))
    (define (&trap-entry-action obj)
      (unchecked-slot-ref obj 'action))
    (define (&trap-entry-signal-set! obj val)
      (unchecked-slot-set! obj 'signal val))
    (define (&trap-entry-action-set! obj val)
      (unchecked-slot-set! obj 'action val)))
  (define *trap-table* (make-hash-table))
  (define *signal-names*
    (let ([ht (make-hash-table)])
      (hash-put! ht "HUP" SIGHUP)
      (hash-put! ht "INT" SIGINT)
      (hash-put! ht "QUIT" SIGQUIT)
      (hash-put! ht "ILL" SIGILL)
      (hash-put! ht "TRAP" SIGTRAP)
      (hash-put! ht "ABRT" SIGABRT)
      (hash-put! ht "FPE" SIGFPE)
      (hash-put! ht "KILL" SIGKILL)
      (hash-put! ht "SEGV" SIGSEGV)
      (hash-put! ht "PIPE" SIGPIPE)
      (hash-put! ht "ALRM" SIGALRM)
      (hash-put! ht "TERM" SIGTERM)
      (hash-put! ht "USR1" SIGUSR1)
      (hash-put! ht "USR2" SIGUSR2)
      (hash-put! ht "CHLD" SIGCHLD)
      (hash-put! ht "CONT" SIGCONT)
      (hash-put! ht "STOP" SIGSTOP)
      (hash-put! ht "TSTP" SIGTSTP)
      (hash-put! ht "TTIN" SIGTTIN)
      (hash-put! ht "TTOU" SIGTTOU)
      (hash-put! ht "WINCH" SIGWINCH)
      (hash-put! ht "URG" SIGURG)
      (hash-put! ht "IO" SIGIO)
      (hash-put! ht "XCPU" SIGXCPU)
      (hash-put! ht "XFSZ" SIGXFSZ)
      (hash-put! ht "VTALRM" SIGVTALRM)
      (hash-put! ht "PROF" SIGPROF)
      (hash-put! ht "SYS" SIGSYS)
      ht))
  (define *pseudo-signals* '("EXIT" "DEBUG" "RETURN" "ERR"))
  (define *signal-number-to-name* (make-hash-table))
  (define (normalize-signal-arg arg)
    (let ([uarg (string-upcase arg)])
      (let ([stripped (if (and (> (string-length uarg) 3)
                               (string=? (substring uarg 0 3) "SIG"))
                          (substring uarg 3 (string-length uarg))
                          uarg)])
        (let ([num (string->number stripped)])
          (cond
            [(and num (= num 0)) "EXIT"]
            [(and num (hash-get *signal-number-to-name* num)) =>
             (lambda (name) name)]
            [(and num (integer? num) (> num 0) (<= num 64))
             (number->string num)]
            [(hash-get *signal-names* stripped) stripped]
            [(member stripped *pseudo-signals*) stripped]
            [else #f])))))
  (define (signal-display-name short-name)
    (if (member short-name *pseudo-signals*)
        short-name
        (string-append "SIG" short-name)))
  (define (signal-name->number name)
    (let ([uname (string-upcase name)])
      (let ([stripped (if (and (> (string-length uname) 3)
                               (string=? (substring uname 0 3) "SIG"))
                          (substring uname 3 (string-length uname))
                          uname)])
        (hash-get *signal-names* stripped))))
  (define *signal-descriptions*
    (let ([ht (make-hash-table)])
      (hash-put! ht "HUP" "Hangup")
      (hash-put! ht "INT" "Interrupt")
      (hash-put! ht "QUIT" "Quit")
      (hash-put! ht "ILL" "Illegal instruction")
      (hash-put! ht "TRAP" "Trace/breakpoint trap")
      (hash-put! ht "ABRT" "Aborted")
      (hash-put! ht "FPE" "Floating point exception")
      (hash-put! ht "KILL" "Killed")
      (hash-put! ht "SEGV" "Segmentation fault")
      (hash-put! ht "PIPE" "Broken pipe")
      (hash-put! ht "ALRM" "Alarm clock")
      (hash-put! ht "TERM" "Terminated")
      (hash-put! ht "USR1" "User defined signal 1")
      (hash-put! ht "USR2" "User defined signal 2")
      (hash-put! ht "CHLD" "Child exited")
      (hash-put! ht "CONT" "Continued")
      (hash-put! ht "STOP" "Stopped (signal)")
      (hash-put! ht "TSTP" "Stopped")
      (hash-put! ht "TTIN" "Stopped (tty input)")
      (hash-put! ht "TTOU" "Stopped (tty output)")
      ht))
  (define (signal-description signum)
    (let ([name (signal-number->name signum)])
      (and name (hash-get *signal-descriptions* name))))
  (define (trap-set! signal-name action)
    (let ([uname (or (normalize-signal-arg signal-name)
                     (string-upcase signal-name))])
      (cond
        [(or (eq? action 'default)
             (not action)
             (string=? (if (string? action) action "") "-"))
         (hash-remove! *trap-table* uname)
         (hash-remove! *flag-trapped-signals* uname)
         (let ([signum (signal-name->number uname)])
           (when (and signum
                      (not (hash-get *initially-ignored-signals* signum)))
             (ffi-signal-set-default signum)
             (guard (__exn [#t ((lambda (e) (%%void)) __exn)])
               (remove-signal-handler! signum))))]
        [(or (eq? action 'ignore)
             (and (string? action) (string=? action "")))
         (hash-put! *trap-table* uname 'ignore)
         (hash-remove! *flag-trapped-signals* uname)
         (let ([signum (signal-name->number uname)])
           (when (and signum
                      (not (hash-get *initially-ignored-signals* signum)))
             (ffi-signal-set-ignore signum)))]
        [(string? action)
         (hash-put! *trap-table* uname action)
         (let ([signum (signal-name->number uname)])
           (when (and signum
                      (not (hash-get *initially-ignored-signals* signum)))
             (guard (__exn [#t ((lambda (e) (%%void)) __exn)])
               (remove-signal-handler! signum))
             (ffi-signal-flag-install signum)
             (hash-put! *flag-trapped-signals* uname signum)))]
        [else
         (error 'gerbil
           (format "trap: invalid action: ~a" action))])))
  (define (trap-get signal-name)
    (let ([uname (or (normalize-signal-arg signal-name)
                     (string-upcase signal-name))])
      (hash-get *trap-table* uname)))
  (define (trap-list)
    (sort
      (hash->list *trap-table*)
      (lambda (a b)
        (let ([na (signal-name->number (car a))]
              [nb (signal-name->number (car b))])
          (cond
            [(and (not na) nb) #t]
            [(and na (not nb)) #f]
            [(and (not na) (not nb)) (string<? (car a) (car b))]
            [else (< na nb)])))))
  (define (has-signal-traps?)
    (call/cc
      (lambda (return)
        (hash-for-each
          (lambda (name action)
            (when (and (string? action)
                       (not (string=? action ""))
                       (signal-name->number name))
              (return #t)))
          *trap-table*)
        #f)))
  (define *pending-signals*-cell (vector (list)))
  (define-syntax *pending-signals*
    (identifier-syntax
      [id (vector-ref *pending-signals*-cell 0)]
      [(set! id v) (vector-set! *pending-signals*-cell 0 v)]))
  (define *flag-trapped-signals* (make-hash-table))
  (define (pending-signals!)
    (hash-for-each
      (lambda (name signum)
        (when (= 1 (ffi-signal-flag-check signum))
          (set! *pending-signals* (cons name *pending-signals*))))
      *flag-trapped-signals*)
    (let ([pending *pending-signals*])
      (set! *pending-signals* (list))
      (reverse pending)))
  (define (clear-pending-signal! sig-name)
    (set! *pending-signals*
      (filter
        (lambda (s) (not (string=? s sig-name)))
        *pending-signals*)))
  (define (signal-number->name num)
    (hash-get *signal-number-to-name* num))
  (define *initially-ignored-signals* (make-hash-table))
  (define (setup-default-signal-handlers!)
    (add-signal-handler!
      SIGINT
      (lambda ()
        (set! *pending-signals* (cons "INT" *pending-signals*))))
    (add-signal-handler! SIGQUIT (lambda () (%%void)))
    (add-signal-handler!
      SIGTERM
      (lambda ()
        (set! *pending-signals* (cons "TERM" *pending-signals*))))
    (add-signal-handler! SIGTSTP (lambda () (%%void)))
    (add-signal-handler! SIGPIPE (lambda () (%%void)))
    (ffi-signal-flag-install SIGXFSZ)
    (hash-put! *flag-trapped-signals* "XFSZ" SIGXFSZ)
    (add-signal-handler!
      SIGWINCH
      (lambda ()
        (set! *pending-signals* (cons "WINCH" *pending-signals*))))
    (add-signal-handler!
      SIGCHLD
      (lambda ()
        (set! *pending-signals* (cons "CHLD" *pending-signals*)))))
  (define (setup-noninteractive-signal-handlers!)
    (for-each
      (lambda (signum)
        (when (= (ffi-signal-was-ignored signum) 1)
          (hash-put! *initially-ignored-signals* signum #t)
          (ffi-signal-set-ignore signum)))
      (list SIGINT SIGQUIT SIGTERM SIGHUP))
    (unless (hash-get *initially-ignored-signals* SIGINT)
      (add-signal-handler!
        SIGINT
        (lambda ()
          (set! *pending-signals* (cons "INT" *pending-signals*)))))
    (unless (hash-get *initially-ignored-signals* SIGTERM)
      (add-signal-handler!
        SIGTERM
        (lambda ()
          (set! *pending-signals* (cons "TERM" *pending-signals*)))))
    (add-signal-handler! SIGPIPE (lambda () (%%void)))
    (ffi-signal-flag-install SIGXFSZ)
    (hash-put! *flag-trapped-signals* "XFSZ" SIGXFSZ))
  (define (with-signal-context thunk)
    (set! *pending-signals* (list))
    (thunk))
  (define (signal-name-list)
    (sort! (hash-keys *signal-names*) string<?))
  (hash-for-each
    (lambda (name num)
      (hash-put! *signal-number-to-name* num name))
    *signal-names*))
