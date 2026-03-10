#!chezscheme
;;; ffi.sls -- Chez Scheme FFI bindings for gherkin-shell
;;; Wraps libgsh-ffi.so (compiled from ffi-shim.c)

(library (gsh ffi)
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

  (import (chezscheme))

  ;; Load FFI symbols.
  ;; 1. dlopen(NULL) — resolves symbols linked into the binary (-rdynamic)
  ;; 2. If ./libgsh-ffi.so exists, load it (interpreted mode via LD_LIBRARY_PATH or cwd)
  ;; file-exists? is safe during boot init (pure Scheme, no exceptions to catch).
  ;; load-shared-object "./..." uses dlopen with path, which always works when file exists.
  (define ffi-lib (load-shared-object ""))
  (define _ffi-lib-so
    (if (file-exists? "./libgsh-ffi.so")
      (load-shared-object "./libgsh-ffi.so")
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
  (define c-ffi-do-waitpid (foreign-procedure "ffi_do_waitpid" (int int) int))
  (define c-ffi-get-waitpid-status (foreign-procedure "ffi_get_waitpid_status" () int))

  (define (ffi-waitpid-pid pid options)
    (c-ffi-do-waitpid pid options))

  (define (ffi-waitpid-status)
    (c-ffi-get-waitpid-status))

  ;; --- FD Operations ---
  (define ffi-dup (foreign-procedure "dup" (int) int))

  (define (ffi-dup-above fd min-fd)
    ((foreign-procedure "fcntl" (int int int) int) fd 0 min-fd))  ;; F_DUPFD = 0

  (define ffi-dup2 (foreign-procedure "dup2" (int int) int))
  (define ffi-close-fd (foreign-procedure "close" (int) int))
  ;; No-op stubs: Gambit's select_abort pipe fds don't exist in Chez
  (define (ffi-move-gambit-fds min-fd) 0)
  (define (ffi-gambit-scheduler-rfd) -1)
  (define (ffi-gambit-scheduler-wfd) -1)
  (define ffi-open-raw (foreign-procedure "open" (string int int) int))
  (define ffi-mkfifo (foreign-procedure "mkfifo" (string int) int))
  (define ffi-unlink (foreign-procedure "unlink" (string) int))

  ;; F_GETFL = 3, F_SETFL = 4
  (define (ffi-fcntl-getfl fd)
    ((foreign-procedure "fcntl" (int int) int) fd 3))

  (define (ffi-fcntl-setfl fd flags)
    ((foreign-procedure "fcntl" (int int int) int) fd 4 flags))

  (define (ffi-lseek-end fd)
    ;; SEEK_END = 2
    ((foreign-procedure "lseek" (int long int) long) fd 0 2))

  ;; --- Pipe ---
  (define c-ffi-do-pipe (foreign-procedure "ffi_do_pipe" () int))
  (define c-ffi-pipe-read-fd (foreign-procedure "ffi_pipe_read_fd" () int))
  (define c-ffi-pipe-write-fd (foreign-procedure "ffi_pipe_write_fd" () int))

  (define (ffi-pipe-raw)
    (let ((rc (c-ffi-do-pipe)))
      (if (= rc 0)
        (values (c-ffi-pipe-read-fd) (c-ffi-pipe-write-fd))
        (error 'ffi-pipe-raw "pipe failed" rc))))

  ;; --- Process ---
  (define ffi-fork (foreign-procedure "fork" () int))
  (define ffi-exit (foreign-procedure "_exit" (int) void))

  (define ffi-execve
    (foreign-procedure "ffi_do_execve" (string string string) int))

  (define c-ffi-fork-exec
    (foreign-procedure "ffi_fork_exec" (string string string int string string) int))

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

  (define ffi-getpid (foreign-procedure "getpid" () int))
  (define ffi-getppid (foreign-procedure "getppid" () int))
  (define ffi-setpgid (foreign-procedure "setpgid" (int int) int))
  (define ffi-getpgid (foreign-procedure "getpgid" (int) int))
  (define ffi-tcsetpgrp (foreign-procedure "tcsetpgrp" (int int) int))
  (define ffi-tcgetpgrp (foreign-procedure "tcgetpgrp" (int) int))
  (define ffi-setsid (foreign-procedure "setsid" () int))

  ;; --- User/Perms ---
  (define ffi-umask (foreign-procedure "umask" (int) int))
  (define ffi-getuid (foreign-procedure "getuid" () int))
  (define ffi-geteuid (foreign-procedure "geteuid" () int))
  (define ffi-getegid (foreign-procedure "getegid" () int))
  (define ffi-access (foreign-procedure "access" (string int) int))
  (define ffi-isatty (foreign-procedure "isatty" (int) int))

  ;; --- Signal ---
  (define ffi-signal-was-ignored (foreign-procedure "ffi_signal_was_ignored" (int) int))
  (define ffi-signal-set-ignore (foreign-procedure "ffi_signal_set_ignore" (int) int))
  (define ffi-signal-set-default (foreign-procedure "ffi_signal_set_default" (int) int))
  (define ffi-sigpipe-unblock (foreign-procedure "ffi_sigpipe_unblock" () int))
  (define ffi-sigpipe-block (foreign-procedure "ffi_sigpipe_block" () int))
  (define ffi-sigchld-block (foreign-procedure "ffi_sigchld_block" () int))
  (define ffi-sigchld-unblock (foreign-procedure "ffi_sigchld_unblock" () int))
  (define ffi-signal-flag-install (foreign-procedure "ffi_signal_flag_install" (int) int))
  (define ffi-signal-flag-check (foreign-procedure "ffi_signal_flag_check" (int) int))

  ;; --- Environment ---
  (define ffi-unsetenv (foreign-procedure "unsetenv" (string) int))

  ;; --- Read ---
  (define c-ffi-do-read-all (foreign-procedure "ffi_do_read_all" (int) int))
  (define c-ffi-get-read-buf (foreign-procedure "ffi_get_read_buf" () string))

  (define (ffi-read-all-from-fd fd)
    (c-ffi-do-read-all fd)
    (c-ffi-get-read-buf))

  (define ffi-read-byte (foreign-procedure "ffi_read_byte" (int) int))

  (define (ffi-byte-ready? fd)
    (= ((foreign-procedure "ffi_byte_ready" (int) int) fd) 1))

  (define c-ffi-fdread (foreign-procedure "ffi_fdread" (int u8* int) int))
  (define c-ffi-fdwrite (foreign-procedure "ffi_fdwrite" (int u8* int) int))

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
  (define ffi-termios-save (foreign-procedure "ffi_termios_save" (int int) int))
  (define ffi-termios-restore (foreign-procedure "ffi_termios_restore" (int int) int))
  (define ffi-set-raw-mode (foreign-procedure "ffi_set_raw_mode" (int) int))

  (define c-ffi-get-winsize (foreign-procedure "ffi_get_winsize" (int) int))
  (define c-ffi-ws-col (foreign-procedure "ffi_ws_col" () int))
  (define c-ffi-ws-row (foreign-procedure "ffi_ws_row" () int))

  (define (ffi-terminal-columns fd)
    (if (= (c-ffi-get-winsize fd) 0) (c-ffi-ws-col) 80))

  (define (ffi-terminal-rows fd)
    (if (= (c-ffi-get-winsize fd) 0) (c-ffi-ws-row) 24))

  ;; --- Time ---
  (define c-ffi-strftime (foreign-procedure "ffi_do_strftime" (string int) string))
  (define (ffi-strftime fmt epoch)
    (c-ffi-strftime fmt epoch))

  ;; --- Resources ---
  (define ffi-getrlimit-soft (foreign-procedure "ffi_getrlimit_soft" (int) long-long))
  (define ffi-getrlimit-hard (foreign-procedure "ffi_getrlimit_hard" (int) long-long))
  (define c-ffi-setrlimit (foreign-procedure "ffi_setrlimit" (int long-long long-long int int) int))

  (define (ffi-setrlimit resource soft hard only-soft only-hard)
    (c-ffi-setrlimit resource soft hard
      (if only-soft 1 0) (if only-hard 1 0)))

  ;; --- Stat ---
  (define ffi-file-type (foreign-procedure "ffi_file_type" (string int) int))
  (define ffi-file-size (foreign-procedure "ffi_file_size" (string) long-long))
  (define ffi-file-mtime (foreign-procedure "ffi_file_mtime" (string) long-long))
  (define ffi-file-mode (foreign-procedure "ffi_file_mode" (string) int))

  ) ;; end library
