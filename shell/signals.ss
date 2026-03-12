;;; signals.ss — Signal handling and traps for gsh

(export #t)
(import :std/sugar
        :std/format
        :std/sort
        :std/os/signal
        :std/os/signal-handler
        :gsh/ffi
        :gsh/util)

;;; --- Trap table ---
;; Maps signal names to actions:
;;   string -> command to execute
;;   'ignore -> ignore the signal
;;   'default -> restore default behavior

(defstruct trap-entry (signal action) transparent: #t)

;; Global trap table (managed by the shell environment)
(def *trap-table* (make-hash-table))

;; Well-known signal name -> number mapping
(def *signal-names*
  (hash
   ("HUP"    SIGHUP)
   ("INT"    SIGINT)
   ("QUIT"   SIGQUIT)
   ("ILL"    SIGILL)
   ("TRAP"   SIGTRAP)
   ("ABRT"   SIGABRT)
   ("FPE"    SIGFPE)
   ("KILL"   SIGKILL)
   ("SEGV"   SIGSEGV)
   ("PIPE"   SIGPIPE)
   ("ALRM"   SIGALRM)
   ("TERM"   SIGTERM)
   ("USR1"   SIGUSR1)
   ("USR2"   SIGUSR2)
   ("CHLD"   SIGCHLD)
   ("CONT"   SIGCONT)
   ("STOP"   SIGSTOP)
   ("TSTP"   SIGTSTP)
   ("TTIN"   SIGTTIN)
   ("TTOU"   SIGTTOU)
   ("WINCH"  SIGWINCH)
   ("URG"    SIGURG)
   ("IO"     SIGIO)
   ("XCPU"   SIGXCPU)
   ("XFSZ"   SIGXFSZ)
   ("VTALRM" SIGVTALRM)
   ("PROF"   SIGPROF)
   ("SYS"    SIGSYS)))

;; Pseudo-signals (not real OS signals)
(def *pseudo-signals* '("EXIT" "DEBUG" "RETURN" "ERR"))

;; Reverse mapping: signal number -> short name
(def *signal-number-to-name* (make-hash-table))
(hash-for-each (lambda (name num) (hash-put! *signal-number-to-name* num name)) *signal-names*)

;; Normalize a signal argument to canonical short name (e.g. "INT", "EXIT")
;; Handles: SIGINT -> INT, INT -> INT, 2 -> INT, 0 -> EXIT, etc.
(def (normalize-signal-arg arg)
  (let ((uarg (string-upcase arg)))
    ;; Strip SIG prefix
    (let ((stripped (if (and (> (string-length uarg) 3)
                             (string=? (substring uarg 0 3) "SIG"))
                     (substring uarg 3 (string-length uarg))
                     uarg)))
      ;; Check if it's a number
      (let ((num (string->number stripped)))
        (cond
          ;; Signal number: 0 = EXIT, others look up
          ((and num (= num 0)) "EXIT")
          ((and num (hash-get *signal-number-to-name* num))
           => (lambda (name) name))
          ;; Valid signal number but no name in table — return as-is
          ((and num (integer? num) (> num 0) (<= num 64))
           (number->string num))
          ;; Known signal name
          ((hash-get *signal-names* stripped) stripped)
          ;; Pseudo-signal
          ((member stripped *pseudo-signals*) stripped)
          ;; Unknown
          (else #f))))))

;; Get canonical display name for trap -p output
;; Pseudo signals: EXIT, DEBUG, RETURN, ERR (no SIG prefix)
;; Real signals: SIGHUP, SIGINT, SIGTERM, etc.
(def (signal-display-name short-name)
  (if (member short-name *pseudo-signals*)
    short-name
    (string-append "SIG" short-name)))

;; Convert signal name to number (or #f for pseudo/unknown)
(def (signal-name->number name)
  (let ((uname (string-upcase name)))
    ;; Strip SIG prefix if present
    (let ((stripped (if (and (> (string-length uname) 3)
                             (string=? (substring uname 0 3) "SIG"))
                     (substring uname 3 (string-length uname))
                     uname)))
      (hash-get *signal-names* stripped))))

;; Human-readable signal descriptions (matching strsignal/bash output)
(def *signal-descriptions*
  (hash
   ("HUP" "Hangup") ("INT" "Interrupt") ("QUIT" "Quit")
   ("ILL" "Illegal instruction") ("TRAP" "Trace/breakpoint trap")
   ("ABRT" "Aborted") ("FPE" "Floating point exception")
   ("KILL" "Killed") ("SEGV" "Segmentation fault")
   ("PIPE" "Broken pipe") ("ALRM" "Alarm clock") ("TERM" "Terminated")
   ("USR1" "User defined signal 1") ("USR2" "User defined signal 2")
   ("CHLD" "Child exited") ("CONT" "Continued") ("STOP" "Stopped (signal)")
   ("TSTP" "Stopped") ("TTIN" "Stopped (tty input)")
   ("TTOU" "Stopped (tty output)")))

;; Get human-readable description for a signal number
(def (signal-description signum)
  (let ((name (signal-number->name signum)))
    (and name (hash-get *signal-descriptions* name))))

;;; --- Trap operations ---

;; Set a trap for a signal
;; signal-name should be a normalized short name (e.g. "INT", "EXIT")
;; action: string (command), "" or 'ignore (ignore), 'default or #f (reset)
(def (trap-set! signal-name action)
  (let ((uname (or (normalize-signal-arg signal-name)
                   (string-upcase signal-name))))
    (cond
      ;; Reset to default
      ((or (eq? action 'default) (not action) (string=? (if (string? action) action "") "-"))
       (hash-remove! *trap-table* uname)
       (hash-remove! *flag-trapped-signals* uname)
       (let ((signum (signal-name->number uname)))
         (when (and signum (not (hash-get *initially-ignored-signals* signum)))
           (ffi-signal-set-default signum)
           (with-catch (lambda (e) #!void) ;; ignore error if no handler installed
             (lambda () (remove-signal-handler! signum))))))
      ;; Ignore signal
      ((or (eq? action 'ignore) (and (string? action) (string=? action "")))
       (hash-put! *trap-table* uname 'ignore)
       (hash-remove! *flag-trapped-signals* uname)
       (let ((signum (signal-name->number uname)))
         (when (and signum (not (hash-get *initially-ignored-signals* signum)))
           (ffi-signal-set-ignore signum))))
      ;; Set command handler
      ((string? action)
       (hash-put! *trap-table* uname action)
       ;; For real signals, install a C-level signal flag handler.
       ;; This is synchronous (flag set immediately on signal delivery),
       ;; unlike Gerbil's async signalfd-based add-signal-handler! which
       ;; has timing issues with signal delivery.
       ;; POSIX: signals that were SIG_IGN at startup cannot be trapped
       (let ((signum (signal-name->number uname)))
         (when (and signum (not (hash-get *initially-ignored-signals* signum)))
           ;; Remove any existing Gerbil handler first
           (with-catch (lambda (e) #!void)
             (lambda () (remove-signal-handler! signum)))
           ;; Install C-level flag handler (also unblocks the signal)
           (ffi-signal-flag-install signum)
           ;; Track which signals use flag-based handling
           (hash-put! *flag-trapped-signals* uname signum))))
      (else
       (error (format "trap: invalid action: ~a" action))))))

;; Get the trap action for a signal
(def (trap-get signal-name)
  (let ((uname (or (normalize-signal-arg signal-name)
                   (string-upcase signal-name))))
    (hash-get *trap-table* uname)))

;; List all traps as alist of (signal-name . action), sorted.
;; Bash sorts EXIT/ERR/DEBUG/RETURN first, then by signal number.
(def (trap-list)
  (sort (hash->list *trap-table*)
        (lambda (a b)
          (let ((na (signal-name->number (car a)))
                (nb (signal-name->number (car b))))
            (cond
              ;; Pseudo-signals (EXIT, ERR, etc.) have no number — sort first
              ((and (not na) nb) #t)
              ((and na (not nb)) #f)
              ((and (not na) (not nb)) (string<? (car a) (car b)))
              (else (< na nb)))))))

;; Check if any signal command traps are registered (not 'ignore, not EXIT/ERR/DEBUG)
(def (has-signal-traps?)
  (let/cc return
    (hash-for-each
     (lambda (name action)
       (when (and (string? action) (not (string=? action ""))
                  ;; Only real signals, not pseudo-signals
                  (signal-name->number name))
         (return #t)))
     *trap-table*)
    #f))

;;; --- Pending signal queue ---

(def *pending-signals* [])

;; Signals using C-level flag handlers (maps signal-name -> signum)
(def *flag-trapped-signals* (make-hash-table))

;; Check and clear pending signals, return list of signal names.
;; Checks both the Gerbil signalfd-based queue and C-level signal flags.
(def (pending-signals!)
  ;; First, check C-level signal flags (synchronous, no timing issues)
  (hash-for-each
   (lambda (name signum)
     (when (= 1 (ffi-signal-flag-check signum))
       (set! *pending-signals* (cons name *pending-signals*))))
   *flag-trapped-signals*)
  ;; Return combined pending list
  (let ((pending *pending-signals*))
    (set! *pending-signals* [])
    (reverse pending)))

;; Remove a specific signal from the pending queue.
;; Used when a foreground child was killed by a signal — the shell should
;; NOT run the trap for that signal (bash behavior).
(def (clear-pending-signal! sig-name)
  (set! *pending-signals*
    (filter (lambda (s) (not (string=? s sig-name))) *pending-signals*)))

;; Map a signal number to its short name (e.g. 2 -> "INT")
(def (signal-number->name num)
  (hash-get *signal-number-to-name* num))

;;; --- Initially-ignored signals (POSIX) ---
;; Signals that were SIG_IGN when the shell started.
;; Non-interactive shells must not override these (POSIX requirement).
;; Populated by setup-noninteractive-signal-handlers!
(def *initially-ignored-signals* (make-hash-table))

;;; --- Default signal setup for interactive shell ---

(def (setup-default-signal-handlers!)
  ;; SIGINT: interrupt current command
  (add-signal-handler! SIGINT
    (lambda ()
      (set! *pending-signals* (cons "INT" *pending-signals*))))
  ;; SIGQUIT: ignore in interactive mode
  (add-signal-handler! SIGQUIT (lambda () #!void))
  ;; SIGTERM: flag for exit
  (add-signal-handler! SIGTERM
    (lambda ()
      (set! *pending-signals* (cons "TERM" *pending-signals*))))
  ;; SIGTSTP: ignore for the shell itself (children get it)
  (add-signal-handler! SIGTSTP (lambda () #!void))
  ;; SIGPIPE: ignore (let write fail with error)
  (add-signal-handler! SIGPIPE (lambda () #!void))
  ;; SIGXFSZ: install flag handler so write fails instead of killing process,
  ;; and the signal is recorded for script termination (exit 153 = 128+25)
  (ffi-signal-flag-install SIGXFSZ)
  (hash-put! *flag-trapped-signals* "XFSZ" SIGXFSZ)
  ;; SIGWINCH: record for terminal resize
  (add-signal-handler! SIGWINCH
    (lambda ()
      (set! *pending-signals* (cons "WINCH" *pending-signals*))))
  ;; SIGCHLD: record for job status updates
  (add-signal-handler! SIGCHLD
    (lambda ()
      (set! *pending-signals* (cons "CHLD" *pending-signals*)))))

;;; --- Signal setup for non-interactive shell (scripts, -c) ---

(def (setup-noninteractive-signal-handlers!)
  ;; Record which signals were SIG_IGN at startup (before Gambit).
  ;; POSIX: non-interactive shells must not override inherited SIG_IGN.
  (for-each
   (lambda (signum)
     (when (= (ffi-signal-was-ignored signum) 1)
       (hash-put! *initially-ignored-signals* signum #t)
       ;; Restore SIG_IGN that Gambit's startup overrode
       (ffi-signal-set-ignore signum)))
   (list SIGINT SIGQUIT SIGTERM SIGHUP))
  ;; SIGINT: record for processing between commands
  ;; Without this, Gambit's default handler terminates the process
  ;; and EXIT traps never fire.
  (unless (hash-get *initially-ignored-signals* SIGINT)
    (add-signal-handler! SIGINT
      (lambda ()
        (set! *pending-signals* (cons "INT" *pending-signals*)))))
  ;; SIGTERM: record for processing
  (unless (hash-get *initially-ignored-signals* SIGTERM)
    (add-signal-handler! SIGTERM
      (lambda ()
        (set! *pending-signals* (cons "TERM" *pending-signals*)))))
  ;; SIGPIPE: ignore (always, regardless of initial state)
  (add-signal-handler! SIGPIPE (lambda () #!void))
  ;; SIGXFSZ: install flag handler for proper handling (exit 153)
  (ffi-signal-flag-install SIGXFSZ)
  (hash-put! *flag-trapped-signals* "XFSZ" SIGXFSZ))

;;; --- Signal context for command execution ---

;; Run a thunk with appropriate signal handling for foreground command execution
(def (with-signal-context thunk)
  ;; Clear pending signals before running
  (set! *pending-signals* [])
  (thunk))

;;; --- Utility ---

;; List all known signal names
(def (signal-name-list)
  (sort! (hash-keys *signal-names*) string<?))
