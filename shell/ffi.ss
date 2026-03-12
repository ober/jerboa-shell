;;; ffi.ss — POSIX FFI bindings for gsh
;;; Provides: waitpid, dup2, setpgid, tcsetpgrp, umask, etc.
;;; All functions are standard POSIX libc — no external libraries needed.

(export #t)
(import :std/foreign)

(begin-ffi (ffi-waitpid-pid ffi-waitpid-status
            ffi-dup ffi-dup-above ffi-dup2 ffi-lseek-end ffi-move-gambit-fds ffi-setpgid ffi-getpgid
            ffi-tcsetpgrp ffi-tcgetpgrp ffi-umask ffi-getuid ffi-geteuid
            ffi-getegid ffi-access
            ffi-isatty ffi-setsid ffi-pipe-raw ffi-close-fd
            ffi-open-raw ffi-mkfifo ffi-unlink ffi-getpid
            ffi-fcntl-getfl ffi-fcntl-setfl
            ffi-read-all-from-fd ffi-unsetenv ffi-strftime
            ffi-getrlimit-soft ffi-getrlimit-hard ffi-setrlimit
            ffi-termios-save ffi-termios-restore ffi-set-raw-mode
            ffi-read-byte ffi-byte-ready?
            ffi-terminal-columns ffi-terminal-rows
            ffi-execve ffi-fork-exec
            ffi-gambit-scheduler-rfd ffi-gambit-scheduler-wfd
            ffi-signal-was-ignored ffi-signal-set-ignore ffi-signal-set-default
            ffi-sigpipe-unblock ffi-sigpipe-block
            ffi-sigchld-block ffi-sigchld-unblock
            ffi-signal-flag-install ffi-signal-flag-check
            WNOHANG WUNTRACED WCONTINUED
            WIFEXITED WEXITSTATUS WIFSIGNALED WTERMSIG WIFSTOPPED WSTOPSIG
            ffi-static-binary?
            ffi-find-embedded-ssi
            ffi-extract-embedded-ssi
            ffi-has-embedded-ssi
            ffi-extract-embedded-scm
            ffi-has-embedded-scm
            ffi-extract-embedded-headers
            ffi-has-embedded-headers
            )

  (c-declare #<<END-C
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <signal.h>
#include <string.h>
#include <dirent.h>
#include <dlfcn.h>

/* Record which signals were SIG_IGN at process startup, BEFORE Gambit
   installs its own handlers.  POSIX: non-interactive shells must not
   override inherited SIG_IGN dispositions. */
static unsigned int _initially_ignored_signals = 0;

__attribute__((constructor))
static void _check_initial_signals(void) {
    struct sigaction sa;
    int sigs[] = {SIGHUP, SIGINT, SIGQUIT, SIGTERM, SIGPIPE,
                  SIGTSTP, SIGTTIN, SIGTTOU, 0};
    for (int i = 0; sigs[i]; i++) {
        if (sigaction(sigs[i], NULL, &sa) == 0 && sa.sa_handler == SIG_IGN)
            _initially_ignored_signals |= (1u << sigs[i]);
    }
}

static int ffi_signal_was_ignored(int signum) {
    if (signum > 0 && signum < 32)
        return (_initially_ignored_signals & (1u << signum)) ? 1 : 0;
    return 0;
}

static int ffi_signal_set_ignore(int signum) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = SIG_IGN;
    sigemptyset(&sa.sa_mask);
    return sigaction(signum, &sa, NULL);
}

static int ffi_signal_set_default(int signum) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = SIG_DFL;
    sigemptyset(&sa.sa_mask);
    return sigaction(signum, &sa, NULL);
}

/* Unblock SIGPIPE so children inherit a clean signal mask.
   Returns 0 on success. */
static int ffi_sigpipe_unblock(void) {
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGPIPE);
    return sigprocmask(SIG_UNBLOCK, &set, NULL);
}

/* Re-block SIGPIPE in the parent after fork. */
static int ffi_sigpipe_block(void) {
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGPIPE);
    return sigprocmask(SIG_BLOCK, &set, NULL);
}

/* Block SIGCHLD to prevent Gambit's SIGCHLD handler from reaping
   ffi-fork-exec'd children via waitpid(-1, WNOHANG).
   Must be called before fork; unblock after our waitpid succeeds. */
static int ffi_sigchld_block(void) {
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGCHLD);
    return sigprocmask(SIG_BLOCK, &set, NULL);
}

/* Unblock SIGCHLD after we've reaped our child.
   Any pending SIGCHLD will be delivered to Gambit's handler. */
static int ffi_sigchld_unblock(void) {
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGCHLD);
    return sigprocmask(SIG_UNBLOCK, &set, NULL);
}

/* --- Signal flag mechanism for traps ---
   C-level signal handler sets a flag; Scheme checks it synchronously.
   Avoids the timing issues of Gerbil's async signalfd-based handlers. */
static volatile sig_atomic_t _signal_flags[65] = {0};

static void _signal_flag_handler(int signum) {
    if (signum >= 0 && signum < 65)
        _signal_flags[signum] = 1;
}

/* Install the C-level flag handler for a signal.
   First unblocks the signal (Gerbil may have blocked it for signalfd). */
static int ffi_signal_flag_install(int signum) {
    if (signum < 1 || signum >= 65) return -1;
    /* Unblock the signal so we get real delivery, not signalfd */
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, signum);
    sigprocmask(SIG_UNBLOCK, &set, NULL);
    /* Install our handler */
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = _signal_flag_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART;
    return sigaction(signum, &sa, NULL);
}

/* Check and clear a signal flag. Returns 1 if signal was received. */
static int ffi_signal_flag_check(int signum) {
    if (signum < 1 || signum >= 65) return 0;
    if (_signal_flags[signum]) {
        _signal_flags[signum] = 0;
        return 1;
    }
    return 0;
}

/* We need to store the last waitpid result since we can't easily
   return multiple values from a single C call.
   Thread-local would be ideal but static is fine for a shell. */
static int _waitpid_last_status = 0;
static int _waitpid_last_pid = 0;

static int ffi_do_waitpid(int pid, int options) {
    int status = 0;
    int result = waitpid(pid, &status, options);
    if (result > 0) {
        _waitpid_last_pid = result;
        _waitpid_last_status = status;
    } else {
        _waitpid_last_pid = result;
        _waitpid_last_status = 0;
    }
    return result;
}

static int ffi_get_waitpid_status(void) {
    return _waitpid_last_status;
}

/* Pipe wrapper that stores both fds */
static int _pipe_fds[2] = {-1, -1};

static int ffi_do_pipe(void) {
    return pipe(_pipe_fds);
}

static int ffi_pipe_read_fd(void) { return _pipe_fds[0]; }
static int ffi_pipe_write_fd(void) { return _pipe_fds[1]; }

/* Move Gambit's scheduler select_abort pipe fds to high fds (>= min_fd).
   This modifies the ___pstate_os structure directly so Gambit's scheduler
   continues to work but frees low fds for shell use. */
static int ___gambit_move_select_abort_fds(int min_fd) {
    ___processor_state ___ps = ___PSTATE;
    int old_rfd = ___ps->os.select_abort.reading_fd;
    int old_wfd = ___ps->os.select_abort.writing_fd;
    int new_rfd, new_wfd;

    /* Only move if they are below min_fd */
    if (old_rfd >= min_fd && old_wfd >= min_fd) return 0;

    new_rfd = fcntl(old_rfd, F_DUPFD, min_fd);
    if (new_rfd < 0) return -1;

    new_wfd = fcntl(old_wfd, F_DUPFD, min_fd);
    if (new_wfd < 0) { close(new_rfd); return -1; }

    /* Update Gambit's state */
    ___ps->os.select_abort.reading_fd = new_rfd;
    ___ps->os.select_abort.writing_fd = new_wfd;

    /* Close the old low fds */
    close(old_rfd);
    close(old_wfd);

    return 0;
}

END-C
  )

  ;; Wait option flags
  (define WNOHANG 1)
  (define WUNTRACED 2)
  (define WCONTINUED 8)

  ;; Status decoding macros (matching POSIX W* macros)
  (define (WIFEXITED s) (= (bitwise-and s #x7f) 0))
  (define (WEXITSTATUS s) (arithmetic-shift (bitwise-and s #xff00) -8))
  (define (WIFSIGNALED s)
    (let ((lo (bitwise-and s #x7f)))
      (and (not (= lo 0)) (not (= lo #x7f)))))
  (define (WTERMSIG s) (bitwise-and s #x7f))
  (define (WIFSTOPPED s) (= (bitwise-and s #xff) #x7f))
  (define (WSTOPSIG s) (arithmetic-shift (bitwise-and s #xff00) -8))

  ;; waitpid — call then retrieve pid and status separately
  (define-c-lambda _ffi_do_waitpid (int int) int "ffi_do_waitpid")
  (define-c-lambda _ffi_get_waitpid_status () int "ffi_get_waitpid_status")

  ;; High-level: call waitpid and get (values pid raw-status)
  (define (ffi-waitpid-pid pid options)
    (_ffi_do_waitpid pid options))

  (define (ffi-waitpid-status)
    (_ffi_get_waitpid_status))

  ;; dup — duplicate fd, returns new fd (next available)
  (define-c-lambda ffi-dup (int) int "dup")

  ;; dup-above — duplicate fd to fd >= min_fd using fcntl(F_DUPFD)
  ;; Used by save-fd to avoid conflicting with user-visible fds
  (define-c-lambda ffi-dup-above (int int) int
    "___return(fcntl(___arg1, F_DUPFD, ___arg2));")

  ;; dup2 — duplicate fd onto target fd
  (define-c-lambda ffi-dup2 (int int) int "dup2")

  ;; lseek fd to end of file — returns new offset or -1 on error
  ;; Used to sync fd 1's offset with Gambit port's after flushing
  (define-c-lambda ffi-lseek-end (int) int
    "___return((int)lseek(___arg1, 0, SEEK_END));")

  ;; Move Gambit's internal scheduler pipe fds (select_abort) to high fds.
  ;; This frees fds 3-9 for user shell redirects (exec 3>, etc.)
  ;; Accesses ___PSTATE->os.select_abort.reading_fd and writing_fd
  ;; Returns 0 on success, -1 on error.
  (define-c-lambda ffi-move-gambit-fds (int) int
    "___return(___gambit_move_select_abort_fds(___arg1));")

  ;; close — close a raw file descriptor
  (define-c-lambda ffi-close-fd (int) int "close")

  ;; Process group management
  (define-c-lambda ffi-setpgid (int int) int "setpgid")
  (define-c-lambda ffi-getpgid (int) int "getpgid")

  ;; Terminal foreground group
  (define-c-lambda ffi-tcsetpgrp (int int) int "tcsetpgrp")
  (define-c-lambda ffi-tcgetpgrp (int) int "tcgetpgrp")

  ;; File creation mask
  (define-c-lambda ffi-umask (int) int "umask")

  ;; User IDs
  (define-c-lambda ffi-getuid () int "getuid")
  (define-c-lambda ffi-geteuid () int "geteuid")
  (define-c-lambda ffi-getegid () int "getegid")

  ;; File access check — access(path, mode) returns 0 on success
  ;; mode: R_OK=4, W_OK=2, X_OK=1, F_OK=0
  (define-c-lambda ffi-access (char-string int) int "access")

  ;; Terminal check
  (define-c-lambda ffi-isatty (int) int "isatty")

  ;; Session management
  (define-c-lambda ffi-setsid () int "setsid")

  ;; Raw open — open a file and return raw fd
  ;; flags: O_RDONLY=0, O_WRONLY=1, O_RDWR=2, O_CREAT=64, O_TRUNC=512, O_APPEND=1024, O_NONBLOCK=2048
  (define-c-lambda ffi-open-raw (char-string int int) int "open")

  ;; fcntl — get/set file descriptor flags
  ;; F_GETFL returns current flags, F_SETFL sets flags
  (define-c-lambda ffi-fcntl-getfl (int) int "___return(fcntl(___arg1, F_GETFL));")
  (define-c-lambda ffi-fcntl-setfl (int int) int "___return(fcntl(___arg1, F_SETFL, ___arg2));")

  ;; Raw pipe — returns 0 on success, -1 on error
  ;; After calling, retrieve fds with ffi-pipe-read-fd / ffi-pipe-write-fd
  (define-c-lambda _ffi_do_pipe () int "ffi_do_pipe")
  (define-c-lambda _ffi_pipe_read_fd () int "ffi_pipe_read_fd")
  (define-c-lambda _ffi_pipe_write_fd () int "ffi_pipe_write_fd")

  (define (ffi-pipe-raw)
    (let ((rc (_ffi_do_pipe)))
      (if (= rc 0)
        (values (_ffi_pipe_read_fd) (_ffi_pipe_write_fd))
        (error "pipe failed" rc))))

  ;; mkfifo — create a named pipe (FIFO)
  ;; mode: e.g. #o600
  (define-c-lambda ffi-mkfifo (char-string int) int "mkfifo")

  ;; unlink — remove a file/FIFO
  (define-c-lambda ffi-unlink (char-string) int "unlink")

  ;; getpid — current process ID
  (define-c-lambda ffi-getpid () int "getpid")

  ;; unsetenv — remove variable from OS environment
  (define-c-lambda ffi-unsetenv (char-string) int "unsetenv")

  ;; Read all bytes from a raw fd into a static buffer.
  ;; Returns the number of bytes read. Use ffi-get-read-buf to retrieve the string.
  ;; This avoids open-input-file "/dev/fd/N" which blocks on empty pipes in Gambit.
  (c-declare #<<END-C2
#define FFI_READ_BUF_SIZE (1024*1024)
static char _ffi_read_buf[FFI_READ_BUF_SIZE];
static size_t _ffi_read_buf_len = 0;

static int ffi_do_read_all(int fd) {
    _ffi_read_buf_len = 0;
    char chunk[4096];
    ssize_t n;
    while ((n = read(fd, chunk, sizeof(chunk))) > 0) {
        size_t avail = FFI_READ_BUF_SIZE - 1 - _ffi_read_buf_len;
        size_t copy = (size_t)n < avail ? (size_t)n : avail;
        memcpy(_ffi_read_buf + _ffi_read_buf_len, chunk, copy);
        _ffi_read_buf_len += copy;
        if (copy < (size_t)n) break;
    }
    _ffi_read_buf[_ffi_read_buf_len] = '\0';
    return (int)_ffi_read_buf_len;
}
END-C2
  )

  (define-c-lambda _ffi_read_all (int) int "ffi_do_read_all")
  (define-c-lambda _ffi_get_read_buf () char-string "___return(_ffi_read_buf);")

  (define (ffi-read-all-from-fd fd)
    (_ffi_read_all fd)
    (_ffi_get_read_buf))

  ;; strftime — format a time value
  (c-declare #<<END-STRFTIME
static char _strftime_buf[128];  /* Match bash's 128-byte truncation */
static char* ffi_do_strftime(const char* fmt, int epoch) {
    time_t t = (time_t)epoch;
    struct tm *tm = localtime(&t);
    if (!tm) { _strftime_buf[0] = '\0'; return _strftime_buf; }
    if (strftime(_strftime_buf, sizeof(_strftime_buf), fmt, tm) == 0)
        _strftime_buf[0] = '\0';  /* truncated or empty — return empty string */
    return _strftime_buf;
}
END-STRFTIME
  )
  (define-c-lambda _ffi_strftime (char-string int) char-string "ffi_do_strftime")
  (define (ffi-strftime fmt epoch) (_ffi_strftime fmt epoch))

  ;; getrlimit / setrlimit — resource limits for ulimit builtin
  (c-declare #<<END-RLIMIT
#include <sys/resource.h>

/* Get soft limit. Returns -1 for RLIM_INFINITY, -2 on error */
static long long ffi_do_getrlimit_soft(int resource) {
    struct rlimit rl;
    if (getrlimit(resource, &rl) != 0) return -2;
    if (rl.rlim_cur == RLIM_INFINITY) return -1;
    return (long long)rl.rlim_cur;
}

/* Get hard limit. Returns -1 for RLIM_INFINITY, -2 on error */
static long long ffi_do_getrlimit_hard(int resource) {
    struct rlimit rl;
    if (getrlimit(resource, &rl) != 0) return -2;
    if (rl.rlim_max == RLIM_INFINITY) return -1;
    return (long long)rl.rlim_max;
}

/* Set resource limit. soft/hard of -1 means RLIM_INFINITY.
   If only_soft is 1, only change soft limit (keep hard as-is).
   If only_hard is 1, only change hard limit (keep soft as-is).
   Returns 0 on success, -1 on failure. */
static int ffi_do_setrlimit(int resource, long long soft, long long hard,
                            int only_soft, int only_hard) {
    struct rlimit rl;
    if (getrlimit(resource, &rl) != 0) return -1;
    if (!only_hard) rl.rlim_cur = (soft == -1) ? RLIM_INFINITY : (rlim_t)soft;
    if (!only_soft) rl.rlim_max = (hard == -1) ? RLIM_INFINITY : (rlim_t)hard;
    return setrlimit(resource, &rl);
}

/* Resource constants */
END-RLIMIT
  )
  (define-c-lambda ffi-getrlimit-soft (int) int64 "ffi_do_getrlimit_soft")
  (define-c-lambda ffi-getrlimit-hard (int) int64 "ffi_do_getrlimit_hard")
  (define-c-lambda _ffi_setrlimit (int int64 int64 int int) int "ffi_do_setrlimit")
  (define (ffi-setrlimit resource soft hard only-soft only-hard)
    (_ffi_setrlimit resource soft hard (if only-soft 1 0) (if only-hard 1 0)))

  ;; termios save/restore — two independent slots for nesting
  ;; Slot 0: saved by shell before line-edit (the "sane" cooked state)
  ;; Slot 1: saved by line-edit before going raw
  (c-declare #<<END-TERMIOS
#include <termios.h>
#define TERMIOS_SLOTS 2
static struct termios _saved_termios[TERMIOS_SLOTS];
static int _termios_valid[TERMIOS_SLOTS] = {0, 0};

static int ffi_do_termios_save(int fd, int slot) {
    if (slot < 0 || slot >= TERMIOS_SLOTS) return -1;
    if (tcgetattr(fd, &_saved_termios[slot]) == 0) {
        _termios_valid[slot] = 1;
        return 0;
    }
    return -1;
}

static int ffi_do_termios_restore(int fd, int slot) {
    if (slot < 0 || slot >= TERMIOS_SLOTS) return -1;
    if (_termios_valid[slot]) {
        return tcsetattr(fd, TCSANOW, &_saved_termios[slot]);
    }
    return -1;
}
END-TERMIOS
  )
  (define-c-lambda ffi-termios-save (int int) int "ffi_do_termios_save")
  (define-c-lambda ffi-termios-restore (int int) int "ffi_do_termios_restore")

  ;; Set raw mode on fd: disable echo, canonical, signals; VMIN=1 VTIME=0
  (c-declare #<<END-RAW
static int ffi_do_set_raw(int fd) {
    struct termios raw;
    if (tcgetattr(fd, &raw) != 0) return -1;
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= CS8;
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 1;
    raw.c_cc[VTIME] = 0;
    return tcsetattr(fd, TCSAFLUSH, &raw);
}
END-RAW
  )
  (define-c-lambda ffi-set-raw-mode (int) int "ffi_do_set_raw")

  ;; Read a single byte from fd. Returns byte value (0-255) or -1 on EOF/error.
  (c-declare #<<END-READBYTE
#include <poll.h>
static int ffi_do_read_byte(int fd) {
    unsigned char c;
    ssize_t n;
    do {
        n = read(fd, &c, 1);
    } while (n < 0 && errno == EINTR);
    if (n == 1) return (int)c;
    return -1;
}
static int ffi_do_byte_ready(int fd) {
    struct pollfd pfd;
    pfd.fd = fd;
    pfd.events = POLLIN;
    pfd.revents = 0;
    int ret = poll(&pfd, 1, 0);
    return (ret > 0 && (pfd.revents & POLLIN)) ? 1 : 0;
}
END-READBYTE
  )
  (define-c-lambda ffi-read-byte (int) int "ffi_do_read_byte")
  (define-c-lambda ffi-byte-ready? (int) int "ffi_do_byte_ready")

  ;; Terminal window size via ioctl(TIOCGWINSZ)
  (c-declare #<<END-WINSIZE
#include <sys/ioctl.h>
static int _ws_row = 0, _ws_col = 0;
static int ffi_do_get_winsize(int fd) {
    struct winsize ws;
    if (ioctl(fd, TIOCGWINSZ, &ws) == 0) {
        _ws_row = ws.ws_row;
        _ws_col = ws.ws_col;
        return 0;
    }
    return -1;
}
END-WINSIZE
  )
  (define-c-lambda _ffi_get_winsize (int) int "ffi_do_get_winsize")
  (define-c-lambda _ffi_ws_col () int "___return(_ws_col);")
  (define-c-lambda _ffi_ws_row () int "___return(_ws_row);")
  (define (ffi-terminal-columns fd)
    (if (= (_ffi_get_winsize fd) 0) (_ffi_ws_col) 80))
  (define (ffi-terminal-rows fd)
    (if (= (_ffi_get_winsize fd) 0) (_ffi_ws_row) 24))

  ;; Shared SOH-delimited string unpacker, execve, and fork+exec
  (c-declare #<<END-EXECVE
/* Unpack a SOH-delimited string into a NULL-terminated char** array.
   Returns the array (caller must free both array and copy).
   Sets *out_copy to the strdup'd working copy.
   If packed is empty, returns a 1-element array with just NULL. */
static char** unpack_soh(const char *packed, char **out_copy, int *out_count) {
    if (!packed[0]) {
        *out_copy = NULL;
        *out_count = 0;
        char **arr = (char **)malloc(sizeof(char *));
        if (arr) arr[0] = NULL;
        return arr;
    }
    int count = 1;
    const char *p;
    for (p = packed; *p; p++) if (*p == '\x01') count++;
    char **arr = (char **)malloc((count + 1) * sizeof(char *));
    char *copy = strdup(packed);
    if (!arr || !copy) { free(arr); free(copy); *out_copy = NULL; *out_count = 0; return NULL; }
    arr[0] = copy;
    int i = 1;
    for (char *q = copy; *q; q++) {
        if (*q == '\x01') { *q = '\0'; arr[i++] = q + 1; }
    }
    arr[count] = NULL;
    *out_copy = copy;
    *out_count = count;
    return arr;
}

static int ffi_do_execve(const char *path, const char *packed_argv, const char *packed_env) {
    char *argv_copy, *env_copy;
    int argc, envc;
    char **argv = unpack_soh(packed_argv, &argv_copy, &argc);
    char **envp = unpack_soh(packed_env, &env_copy, &envc);
    if (!argv || !envp) return ENOMEM;

    execve(path, argv, envp);
    /* Only reaches here on failure */
    int err = errno;
    free(argv_copy); free(argv);
    free(env_copy); free(envp);
    return err;
}

/* fork+exec in one C function.  After fork(), the child is in a fragile
   state (Gambit runtime is not fork-safe), so ALL child-side work is
   pure C — no Scheme code can run.

   pgid semantics:
     0  = foreground (no setpgid)
    -1  = background, own process group (setpgid(0,0))
    >0  = join existing group (setpgid(0,pgid))

   gambit_rfd/gambit_wfd = Gambit's scheduler select_abort pipe fds,
   which must be closed in the child so they don't leak.

   Returns child PID on success, -1 on fork failure. */
/* Close all fds > 2 in the child, except those in keep_fds[0..n_keep-1].
   Uses /proc/self/fd for efficiency, falls back to brute-force. */
static void close_fds_except(const int *keep_fds, int n_keep) {
    DIR *d = opendir("/proc/self/fd");
    if (d) {
        struct dirent *entry;
        int dir_fd = dirfd(d);
        while ((entry = readdir(d)) != NULL) {
            int fd = atoi(entry->d_name);
            if (fd <= 2 || fd == dir_fd) continue;
            int keep = 0;
            for (int i = 0; i < n_keep; i++) {
                if (fd == keep_fds[i]) { keep = 1; break; }
            }
            if (!keep) close(fd);
        }
        closedir(d);
    } else {
        for (int fd = 3; fd < 1024; fd++) {
            int keep = 0;
            for (int i = 0; i < n_keep; i++) {
                if (fd == keep_fds[i]) { keep = 1; break; }
            }
            if (!keep) close(fd);
        }
    }
}

static int ffi_do_fork_exec(const char *path, const char *packed_argv,
                            const char *packed_env, int pgid,
                            int gambit_rfd, int gambit_wfd,
                            const char *packed_keep_fds,
                            const char *cwd) {
    /* Block SIGCHLD before fork to prevent Gambit's SIGCHLD handler
       from reaping our child via waitpid(-1, WNOHANG).
       The caller must call ffi_sigchld_unblock() after waiting. */
    ffi_sigchld_block();

    pid_t child = fork();
    if (child < 0) {
        ffi_sigchld_unblock();
        return -1;
    }

    if (child > 0) {
        /* ---- Parent ---- */
        /* Set child's pgid from parent side too (race prevention) */
        if (pgid == -1) {
            setpgid(child, child);
        } else if (pgid > 0) {
            setpgid(child, pgid);
        }
        return (int)child;
    }

    /* ---- Child (pure C, no Scheme) ---- */

    /* 1. Reset signals to SIG_DFL */
    int sigs[] = {SIGINT, SIGQUIT, SIGTERM, SIGHUP, SIGPIPE,
                  SIGTSTP, SIGTTIN, SIGTTOU, SIGCHLD, 0};
    for (int i = 0; sigs[i]; i++) {
        signal(sigs[i], SIG_DFL);
    }

    /* 2. Unblock all signals */
    sigset_t all;
    sigfillset(&all);
    sigprocmask(SIG_UNBLOCK, &all, NULL);

    /* 3. Process group */
    if (pgid == -1) {
        setpgid(0, 0);
    } else if (pgid > 0) {
        setpgid(0, pgid);
    }

    /* 4. Close all fds > 2 except those the child needs.
       packed_keep_fds is a SOH-delimited string of fd numbers to preserve
       (e.g. redirect fds like "8" for 8<<EOF).  Empty string = close all > 2. */
    {
        int keep[64];
        int n_keep = 0;
        if (packed_keep_fds && packed_keep_fds[0]) {
            /* Parse SOH-delimited fd numbers */
            const char *p = packed_keep_fds;
            while (*p && n_keep < 64) {
                keep[n_keep++] = atoi(p);
                while (*p && *p != '\001') p++;
                if (*p == '\001') p++;
            }
        }
        close_fds_except(keep, n_keep);
    }

    /* 5. Set child CWD — Gambit's per-thread current-directory may have
       restored the OS CWD to the wrong path between cd and fork */
    if (cwd && cwd[0]) {
        chdir(cwd);
    }

    /* 6. Unpack argv and envp */
    char *argv_copy, *env_copy;
    int argc, envc;
    char **argv = unpack_soh(packed_argv, &argv_copy, &argc);
    char **envp = unpack_soh(packed_env, &env_copy, &envc);
    if (!argv || !envp) _exit(126);

    /* 7. exec */
    execve(path, argv, envp);

    /* 8. exec failed — try /bin/sh for scripts without shebang (ENOEXEC) */
    if (errno == ENOEXEC) {
        char **new_argv = (char **)alloca((argc + 2) * sizeof(char*));
        new_argv[0] = "/bin/sh";
        for (int i = 0; i <= argc; i++) new_argv[i + 1] = argv[i];
        execve("/bin/sh", new_argv, envp);
    }

    /* 9. exec failed */
    _exit(errno == ENOENT ? 127 : 126);
    return -1; /* unreachable */
}

/* Accessors for Gambit's scheduler select_abort pipe fds.
   These are the fds that Gambit uses internally for its I/O scheduler;
   we need to close them in the child after fork. */
static int ffi_gambit_scheduler_rfd(void) {
    ___processor_state ___ps = ___PSTATE;
    return ___ps->os.select_abort.reading_fd;
}
static int ffi_gambit_scheduler_wfd(void) {
    ___processor_state ___ps = ___PSTATE;
    return ___ps->os.select_abort.writing_fd;
}
END-EXECVE
  )
  (define-c-lambda _ffi_execve (char-string char-string char-string) int "ffi_do_execve")
  (define (ffi-execve path packed-argv packed-env) (_ffi_execve path packed-argv packed-env))

  ;; fork+exec — fork and exec in one C call (child does no Scheme)
  ;; Returns child PID on success, -1 on fork failure
  ;; packed-keep-fds: SOH-delimited string of fd numbers to preserve in child
  ;;                  (for redirect fds like fd 8 from 8<<EOF). "" = close all > 2.
  ;; cwd: working directory for child (Gambit may have restored OS CWD)
  (define-c-lambda _ffi_fork_exec (char-string char-string char-string int int int char-string char-string) int
    "ffi_do_fork_exec")
  (define (ffi-fork-exec path packed-argv packed-env pgid gambit-rfd gambit-wfd
                         #!optional (packed-keep-fds "") (cwd ""))
    (_ffi_fork_exec path packed-argv packed-env pgid gambit-rfd gambit-wfd packed-keep-fds cwd))

  ;; Gambit scheduler fd accessors (for closing in fork-exec child)
  (define-c-lambda ffi-gambit-scheduler-rfd () int "ffi_gambit_scheduler_rfd")
  (define-c-lambda ffi-gambit-scheduler-wfd () int "ffi_gambit_scheduler_wfd")

  ;; Check if a signal was SIG_IGN when the process started (before Gambit).
  ;; Returns 1 if ignored, 0 otherwise.
  (define-c-lambda ffi-signal-was-ignored (int) int "ffi_signal_was_ignored")

  ;; Set a signal's disposition to SIG_IGN (restore inherited ignore).
  ;; Returns 0 on success, -1 on error.
  (define-c-lambda ffi-signal-set-ignore (int) int "ffi_signal_set_ignore")

  ;; Set a signal's disposition to SIG_DFL (reset to default).
  ;; Returns 0 on success, -1 on error.
  (define-c-lambda ffi-signal-set-default (int) int "ffi_signal_set_default")

  ;; Unblock SIGPIPE before open-process so children inherit a clean mask.
  (define-c-lambda ffi-sigpipe-unblock () int "ffi_sigpipe_unblock")
  ;; Re-block SIGPIPE after open-process returns.
  (define-c-lambda ffi-sigpipe-block () int "ffi_sigpipe_block")

  ;; Block/unblock SIGCHLD — prevent Gambit's handler from reaping
  ;; ffi-fork-exec'd children.  ffi_do_fork_exec blocks automatically;
  ;; callers must unblock after their waitpid succeeds.
  (define-c-lambda ffi-sigchld-block () int "ffi_sigchld_block")
  (define-c-lambda ffi-sigchld-unblock () int "ffi_sigchld_unblock")

  ;; Signal flag mechanism — C-level handler sets flags synchronously.
  ;; Used for user traps instead of Gerbil's async signalfd handlers.
  (define-c-lambda ffi-signal-flag-install (int) int "ffi_signal_flag_install")
  (define-c-lambda ffi-signal-flag-check (int) int "ffi_signal_flag_check")

  ;; Detect statically-linked binary by checking for embedded .ssi data.
  ;; The gsh_has_embedded_ssi() weak symbol returns 0 in dynamic builds
  ;; (no embedded_ssi.o linked) and >0 in static builds.
  (define-c-lambda ffi-static-binary? () scheme-object
    "___result = gsh_has_embedded_ssi() > 0 ? ___TRU : ___FAL;")

  ;; Embedded .ssi file support — data comes from embedded_ssi.c (linked in).
  ;; Weak symbols: return defaults when embedded_ssi.o is not linked.
  (c-declare #<<END-SSI
extern const char* gsh_find_embedded_ssi(const char *path, unsigned int *size);
extern int gsh_extract_embedded_ssi(const char *base_dir);
extern int gsh_has_embedded_ssi(void);

const char* __attribute__((weak)) gsh_find_embedded_ssi(const char *path, unsigned int *size) {
    (void)path; (void)size; return NULL;
}
int __attribute__((weak)) gsh_extract_embedded_ssi(const char *base_dir) {
    (void)base_dir; return 0;
}
int __attribute__((weak)) gsh_has_embedded_ssi(void) { return 0; }
END-SSI
  )
  ;; Lookup a single .ssi by relative path (e.g. "std/sugar.ssi")
  (define-c-lambda _ffi_find_embedded_ssi (char-string) char-string
    "___return((char*)gsh_find_embedded_ssi(___arg1, NULL));")
  (define (ffi-find-embedded-ssi path) (_ffi_find_embedded_ssi path))
  ;; Extract all embedded .ssi files to a directory. Returns count or -1.
  (define-c-lambda ffi-extract-embedded-ssi (char-string) int
    "gsh_extract_embedded_ssi")
  ;; Check if embedded .ssi data is available
  (define-c-lambda ffi-has-embedded-ssi () int "gsh_has_embedded_ssi")

  ;; --- Embedded .scm archive (compressed runtime modules) ---
  ;; Weak symbols: default to no-op when embedded_scm.o is not linked (dynamic builds)
  (c-declare #<<END-SCM
int __attribute__((weak)) gsh_extract_embedded_scm(const char *base_dir) {
    (void)base_dir; return 0;
}
int __attribute__((weak)) gsh_has_embedded_scm(void) { return 0; }
END-SCM
  )
  ;; Extract compressed .scm archive to a directory. Returns 0 on success.
  (define-c-lambda ffi-extract-embedded-scm (char-string) int
    "gsh_extract_embedded_scm")
  ;; Check if embedded .scm archive is available
  (define-c-lambda ffi-has-embedded-scm () int "gsh_has_embedded_scm")

  ;; --- Embedded gambit.h headers (for compile-file support) ---
  ;; Weak symbols: default to no-op when embedded_gambit_h.o is not linked
  (c-declare #<<END-HEADERS
int __attribute__((weak)) gsh_extract_embedded_headers(const char *include_dir) {
    (void)include_dir; return -1;
}
int __attribute__((weak)) gsh_has_embedded_headers(void) { return 0; }
END-HEADERS
  )
  ;; Extract embedded gambit.h to a directory. Returns 0 on success.
  (define-c-lambda ffi-extract-embedded-headers (char-string) int
    "gsh_extract_embedded_headers")
  ;; Check if embedded header data is available
  (define-c-lambda ffi-has-embedded-headers () int "gsh_has_embedded_headers")

)
