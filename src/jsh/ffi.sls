#!chezscheme
;;; ffi.sls -- Chez Scheme FFI bindings for gherkin-shell
;;; Wraps libjsh-ffi.so (compiled from ffi-shim.c)

(library (jsh ffi)
  (export
    ;; waitpid
    ffi-waitpid-pid ffi-waitpid-status
    WNOHANG WUNTRACED WCONTINUED
    WIFEXITED WEXITSTATUS WIFSIGNALED WTERMSIG WIFSTOPPED WSTOPSIG
    ;; fd operations
    ffi-dup ffi-dup-above ffi-dup2 ffi-close-fd ffi-move-gambit-fds
    ffi-gambit-scheduler-rfd ffi-gambit-scheduler-wfd
    ffi-open-raw ffi-mkfifo ffi-unlink
    ffi-pipe-raw
    ffi-fcntl-getfl ffi-fcntl-setfl
    ffi-lseek-end
    ;; process
    ffi-fork ffi-exit
    ffi-execve ffi-fork-exec ffi-getpid ffi-getppid
    ffi-setpgid ffi-getpgid
    ffi-tcsetpgrp ffi-tcgetpgrp
    ffi-setsid
    ;; user/perms
    ffi-umask ffi-getuid ffi-geteuid ffi-getegid ffi-access ffi-isatty
    ;; signal
    ffi-signal-was-ignored ffi-signal-set-ignore ffi-signal-set-default
    ffi-sigpipe-unblock ffi-sigpipe-block
    ffi-sigchld-block ffi-sigchld-unblock
    ffi-signal-flag-install ffi-signal-flag-check
    ;; environment
    ffi-unsetenv
    ;; read
    ffi-read-all-from-fd ffi-read-byte ffi-byte-ready?
    ffi-fdread ffi-fdwrite
    ;; terminal
    ffi-termios-save ffi-termios-restore ffi-set-raw-mode
    ffi-terminal-columns ffi-terminal-rows
    ;; time
    ffi-strftime
    ;; resources
    ffi-getrlimit-soft ffi-getrlimit-hard ffi-setrlimit
    ;; stat
    ffi-file-type ffi-file-size ffi-file-mtime ffi-file-mode)

  (import (chezscheme)
          (only (std foreign) define-foreign ->))

  ;; Load FFI symbols.
  ;; 1. dlopen(NULL) — resolves symbols linked into the binary (-rdynamic)
  ;; 2. If ./libjsh-ffi.so exists, load it (interpreted mode via LD_LIBRARY_PATH or cwd)
  (define _ffi-lib (load-shared-object ""))
  (define _ffi-lib-so
    (if (file-exists? "./libjsh-ffi.so")
      (load-shared-object "./libjsh-ffi.so")
      (void)))

  ;; --- Wait flags ---
  (define WNOHANG 1)
  (define WUNTRACED 2)
  (define WCONTINUED 8)

  ;; --- Status decoders ---
  (define (WIFEXITED s) (= (bitwise-and s #x7f) 0))
  (define (WEXITSTATUS s) (bitwise-arithmetic-shift-right (bitwise-and s #xff00) 8))
  (define (WIFSIGNALED s)
    (let ((lo (bitwise-and s #x7f)))
      (and (not (= lo 0)) (not (= lo #x7f)))))
  (define (WTERMSIG s) (bitwise-and s #x7f))
  (define (WIFSTOPPED s) (= (bitwise-and s #xff) #x7f))
  (define (WSTOPSIG s) (bitwise-arithmetic-shift-right (bitwise-and s #xff00) 8))

  ;; --- Waitpid ---
  (define-foreign c-ffi-do-waitpid "ffi_do_waitpid" (int int) -> int)
  (define-foreign c-ffi-get-waitpid-status "ffi_get_waitpid_status" () -> int)

  (define (ffi-waitpid-pid pid options)
    (c-ffi-do-waitpid pid options))

  (define (ffi-waitpid-status)
    (c-ffi-get-waitpid-status))

  ;; --- FD Operations ---
  (define-foreign ffi-dup "dup" (int) -> int)
  (define-foreign ffi-dup2 "dup2" (int int) -> int)
  (define-foreign ffi-close-fd "close" (int) -> int)
  (define-foreign ffi-open-raw "open" (string int int) -> int)
  (define-foreign ffi-mkfifo "mkfifo" (string int) -> int)
  (define-foreign ffi-unlink "unlink" (string) -> int)

  (define (ffi-dup-above fd min-fd)
    ((foreign-procedure "fcntl" (int int int) int) fd 0 min-fd))  ;; F_DUPFD = 0

  ;; No-op stubs: Gambit's select_abort pipe fds don't exist in Chez
  (define (ffi-move-gambit-fds min-fd) 0)
  (define (ffi-gambit-scheduler-rfd) -1)
  (define (ffi-gambit-scheduler-wfd) -1)

  ;; F_GETFL = 3, F_SETFL = 4
  (define (ffi-fcntl-getfl fd)
    ((foreign-procedure "fcntl" (int int) int) fd 3))

  (define (ffi-fcntl-setfl fd flags)
    ((foreign-procedure "fcntl" (int int int) int) fd 4 flags))

  (define (ffi-lseek-end fd)
    ;; SEEK_END = 2
    ((foreign-procedure "lseek" (int long int) long) fd 0 2))

  ;; --- Pipe ---
  (define-foreign c-ffi-do-pipe "ffi_do_pipe" () -> int)
  (define-foreign c-ffi-pipe-read-fd "ffi_pipe_read_fd" () -> int)
  (define-foreign c-ffi-pipe-write-fd "ffi_pipe_write_fd" () -> int)

  (define (ffi-pipe-raw)
    (let ((rc (c-ffi-do-pipe)))
      (if (= rc 0)
        (values (c-ffi-pipe-read-fd) (c-ffi-pipe-write-fd))
        (error 'ffi-pipe-raw "pipe failed" rc))))

  ;; --- Process ---
  (define-foreign ffi-fork "fork" () -> int)
  (define-foreign ffi-exit "_exit" (int) -> void)
  (define-foreign ffi-execve "ffi_do_execve" (string string string) -> int)
  (define-foreign c-ffi-fork-exec "ffi_fork_exec" (string string string int string string) -> int)

  (define (ffi-fork-exec path packed-argv packed-env pgid
                          . rest)
    ;; Called as: (ffi-fork-exec path argv env pgid sched-rfd sched-wfd keep-fds cwd)
    ;; The sched-rfd/sched-wfd are Gambit scheduler FDs (ignored in Chez).
    ;; Also supports: (ffi-fork-exec path argv env pgid keep-fds cwd) [no scheduler FDs]
    (let-values ([(keep-fds cwd)
                  (cond
                    ;; 8-arg form: skip scheduler-rfd, scheduler-wfd
                    [(and (>= (length rest) 4)
                          (integer? (car rest))
                          (integer? (cadr rest)))
                     (values (caddr rest) (cadddr rest))]
                    ;; 6-arg form: keep-fds and cwd directly
                    [(>= (length rest) 2)
                     (values (car rest) (cadr rest))]
                    ;; Fewer args
                    [(= (length rest) 1)
                     (values (car rest) "")]
                    [else (values "" "")])])
      (c-ffi-fork-exec path packed-argv packed-env pgid keep-fds cwd)))

  (define-foreign ffi-getpid "getpid" () -> int)
  (define-foreign ffi-getppid "getppid" () -> int)
  (define-foreign ffi-setpgid "setpgid" (int int) -> int)
  (define-foreign ffi-getpgid "getpgid" (int) -> int)
  (define-foreign ffi-tcsetpgrp "tcsetpgrp" (int int) -> int)
  (define-foreign ffi-tcgetpgrp "tcgetpgrp" (int) -> int)
  (define-foreign ffi-setsid "setsid" () -> int)

  ;; --- User/Perms ---
  (define-foreign ffi-umask "umask" (int) -> int)
  (define-foreign ffi-getuid "getuid" () -> int)
  (define-foreign ffi-geteuid "geteuid" () -> int)
  (define-foreign ffi-getegid "getegid" () -> int)
  (define-foreign ffi-access "access" (string int) -> int)
  (define-foreign ffi-isatty "isatty" (int) -> int)

  ;; --- Signal ---
  (define-foreign ffi-signal-was-ignored "ffi_signal_was_ignored" (int) -> int)
  (define-foreign ffi-signal-set-ignore "ffi_signal_set_ignore" (int) -> int)
  (define-foreign ffi-signal-set-default "ffi_signal_set_default" (int) -> int)
  (define-foreign ffi-sigpipe-unblock "ffi_sigpipe_unblock" () -> int)
  (define-foreign ffi-sigpipe-block "ffi_sigpipe_block" () -> int)
  (define-foreign ffi-sigchld-block "ffi_sigchld_block" () -> int)
  (define-foreign ffi-sigchld-unblock "ffi_sigchld_unblock" () -> int)
  (define-foreign ffi-signal-flag-install "ffi_signal_flag_install" (int) -> int)
  (define-foreign ffi-signal-flag-check "ffi_signal_flag_check" (int) -> int)

  ;; --- Environment ---
  (define-foreign ffi-unsetenv "unsetenv" (string) -> int)

  ;; --- Read ---
  (define-foreign c-ffi-do-read-all "ffi_do_read_all" (int) -> int)
  ;; c-ffi-copy-read-buf fills a pre-allocated bytevector with raw bytes from
  ;; the read buffer, avoiding Chez's UTF-8 decoding that replaces invalid
  ;; bytes with U+FFFD. Returns the count actually copied.
  (define-foreign c-ffi-copy-read-buf "ffi_copy_read_buf" (u8* int) -> int)

  (define (ffi-read-all-from-fd fd)
    ;; Read all bytes from fd and return a Latin-1 decoded Scheme string.
    ;; Each byte 0x00-0xFF becomes char U+0000-U+00FF, preserving raw bytes.
    ;; This matches Gambit's char-string behavior for ffi_get_read_buf.
    (let* ((len (c-ffi-do-read-all fd))
           (bv (make-bytevector len))
           (_ (c-ffi-copy-read-buf bv len))
           (result (make-string len)))
      (let loop ((i 0))
        (if (>= i len)
          result
          (begin
            (string-set! result i (integer->char (bytevector-u8-ref bv i)))
            (loop (+ i 1)))))))

  (define-foreign ffi-read-byte "ffi_read_byte" (int) -> int)

  (define (ffi-byte-ready? fd)
    (= ((foreign-procedure "ffi_byte_ready" (int) int) fd) 1))

  (define-foreign c-ffi-fdread "ffi_fdread" (int u8* int) -> int)
  (define-foreign c-ffi-fdwrite "ffi_fdwrite" (int u8* int) -> int)

  (define (ffi-fdread fd count)
    (let ((buf (make-bytevector count)))
      (let ((n (c-ffi-fdread fd buf count)))
        (if (> n 0)
          (if (= n count) buf
            (let ((result (make-bytevector n)))
              (bytevector-copy! buf 0 result 0 n)
              result))
          (make-bytevector 0)))))

  (define (ffi-fdwrite fd bv)
    (c-ffi-fdwrite fd bv (bytevector-length bv)))

  ;; --- Terminal ---
  (define-foreign ffi-termios-save "ffi_termios_save" (int int) -> int)
  (define-foreign ffi-termios-restore "ffi_termios_restore" (int int) -> int)
  (define-foreign ffi-set-raw-mode "ffi_set_raw_mode" (int) -> int)
  (define-foreign c-ffi-get-winsize "ffi_get_winsize" (int) -> int)
  (define-foreign c-ffi-ws-col "ffi_ws_col" () -> int)
  (define-foreign c-ffi-ws-row "ffi_ws_row" () -> int)

  (define (ffi-terminal-columns fd)
    (if (= (c-ffi-get-winsize fd) 0) (c-ffi-ws-col) 80))

  (define (ffi-terminal-rows fd)
    (if (= (c-ffi-get-winsize fd) 0) (c-ffi-ws-row) 24))

  ;; --- Time ---
  (define-foreign c-ffi-strftime "ffi_do_strftime" (string int) -> string)
  (define (ffi-strftime fmt epoch)
    (c-ffi-strftime fmt epoch))

  ;; --- Resources ---
  (define-foreign ffi-getrlimit-soft "ffi_getrlimit_soft" (int) -> long-long)
  (define-foreign ffi-getrlimit-hard "ffi_getrlimit_hard" (int) -> long-long)
  (define-foreign c-ffi-setrlimit "ffi_setrlimit" (int long-long long-long int int) -> int)

  (define (ffi-setrlimit resource soft hard only-soft only-hard)
    (c-ffi-setrlimit resource soft hard
      (if only-soft 1 0) (if only-hard 1 0)))

  ;; --- Stat ---
  (define-foreign ffi-file-type "ffi_file_type" (string int) -> int)
  (define-foreign ffi-file-size "ffi_file_size" (string) -> long-long)
  (define-foreign ffi-file-mtime "ffi_file_mtime" (string) -> long-long)
  (define-foreign ffi-file-mode "ffi_file_mode" (string) -> int)

  ) ;; end library
