/* ffi-shim.c -- POSIX FFI bindings for gherkin-shell
 * Compile: gcc -shared -fPIC -o libgsh-ffi.so src/gsh/ffi-shim.c -Wall -O2
 *
 * All functions are standard POSIX libc — no external libraries needed.
 * Functions prefixed with ffi_ to avoid symbol conflicts.
 */

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <dirent.h>
#include <termios.h>
#include <poll.h>

/* ========== Signal Initialization ========== */

/* Record which signals were SIG_IGN at process startup.
   POSIX: non-interactive shells must not override inherited SIG_IGN. */
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

int ffi_signal_was_ignored(int signum) {
    if (signum > 0 && signum < 32)
        return (_initially_ignored_signals & (1u << signum)) ? 1 : 0;
    return 0;
}

int ffi_signal_set_ignore(int signum) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = SIG_IGN;
    sigemptyset(&sa.sa_mask);
    return sigaction(signum, &sa, NULL);
}

int ffi_signal_set_default(int signum) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = SIG_DFL;
    sigemptyset(&sa.sa_mask);
    return sigaction(signum, &sa, NULL);
}

/* ========== Signal Mask Management ========== */

int ffi_sigpipe_unblock(void) {
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGPIPE);
    return sigprocmask(SIG_UNBLOCK, &set, NULL);
}

int ffi_sigpipe_block(void) {
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGPIPE);
    return sigprocmask(SIG_BLOCK, &set, NULL);
}

int ffi_sigchld_block(void) {
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGCHLD);
    return sigprocmask(SIG_BLOCK, &set, NULL);
}

int ffi_sigchld_unblock(void) {
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGCHLD);
    return sigprocmask(SIG_UNBLOCK, &set, NULL);
}

/* ========== Signal Flag Mechanism ========== */

static volatile sig_atomic_t _signal_flags[65] = {0};

static void _signal_flag_handler(int signum) {
    if (signum >= 0 && signum < 65)
        _signal_flags[signum] = 1;
}

int ffi_signal_flag_install(int signum) {
    if (signum < 1 || signum >= 65) return -1;
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, signum);
    sigprocmask(SIG_UNBLOCK, &set, NULL);
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = _signal_flag_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_RESTART;
    return sigaction(signum, &sa, NULL);
}

int ffi_signal_flag_check(int signum) {
    if (signum < 1 || signum >= 65) return 0;
    if (_signal_flags[signum]) {
        _signal_flags[signum] = 0;
        return 1;
    }
    return 0;
}

/* ========== Waitpid ========== */

static int _waitpid_last_status = 0;

int ffi_do_waitpid(int pid, int options) {
    int status = 0;
    int result = waitpid(pid, &status, options);
    if (result > 0) {
        _waitpid_last_status = status;
    } else {
        _waitpid_last_status = 0;
    }
    return result;
}

int ffi_get_waitpid_status(void) {
    return _waitpid_last_status;
}

/* ========== Pipe ========== */

static int _pipe_fds[2] = {-1, -1};

int ffi_do_pipe(void) {
    return pipe(_pipe_fds);
}

int ffi_pipe_read_fd(void) { return _pipe_fds[0]; }
int ffi_pipe_write_fd(void) { return _pipe_fds[1]; }

/* ========== Read All From FD ========== */

#define FFI_READ_BUF_SIZE (1024*1024)
static char _ffi_read_buf[FFI_READ_BUF_SIZE];
static size_t _ffi_read_buf_len = 0;

int ffi_do_read_all(int fd) {
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

const char* ffi_get_read_buf(void) {
    return _ffi_read_buf;
}

/* ========== Strftime ========== */

static char _strftime_buf[128];

const char* ffi_do_strftime(const char* fmt, int epoch) {
    time_t t = (time_t)epoch;
    struct tm *tm = localtime(&t);
    if (!tm) { _strftime_buf[0] = '\0'; return _strftime_buf; }
    if (strftime(_strftime_buf, sizeof(_strftime_buf), fmt, tm) == 0)
        _strftime_buf[0] = '\0';
    return _strftime_buf;
}

/* ========== Resource Limits ========== */

long long ffi_getrlimit_soft(int resource) {
    struct rlimit rl;
    if (getrlimit(resource, &rl) != 0) return -2;
    if (rl.rlim_cur == RLIM_INFINITY) return -1;
    return (long long)rl.rlim_cur;
}

long long ffi_getrlimit_hard(int resource) {
    struct rlimit rl;
    if (getrlimit(resource, &rl) != 0) return -2;
    if (rl.rlim_max == RLIM_INFINITY) return -1;
    return (long long)rl.rlim_max;
}

int ffi_setrlimit(int resource, long long soft, long long hard,
                   int only_soft, int only_hard) {
    struct rlimit rl;
    if (getrlimit(resource, &rl) != 0) return -1;
    if (!only_hard) rl.rlim_cur = (soft == -1) ? RLIM_INFINITY : (rlim_t)soft;
    if (!only_soft) rl.rlim_max = (hard == -1) ? RLIM_INFINITY : (rlim_t)hard;
    return setrlimit(resource, &rl);
}

/* ========== Termios ========== */

#define TERMIOS_SLOTS 2
static struct termios _saved_termios[TERMIOS_SLOTS];
static int _termios_valid[TERMIOS_SLOTS] = {0, 0};

int ffi_termios_save(int fd, int slot) {
    if (slot < 0 || slot >= TERMIOS_SLOTS) return -1;
    if (tcgetattr(fd, &_saved_termios[slot]) == 0) {
        _termios_valid[slot] = 1;
        return 0;
    }
    return -1;
}

int ffi_termios_restore(int fd, int slot) {
    if (slot < 0 || slot >= TERMIOS_SLOTS) return -1;
    if (_termios_valid[slot])
        return tcsetattr(fd, TCSANOW, &_saved_termios[slot]);
    return -1;
}

int ffi_set_raw_mode(int fd) {
    struct termios raw;
    if (tcgetattr(fd, &raw) != 0) return -1;
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    /* Keep OPOST so \n → \r\n translation works for child processes */
    raw.c_cflag |= CS8;
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 1;
    raw.c_cc[VTIME] = 0;
    return tcsetattr(fd, TCSAFLUSH, &raw);
}

/* ========== Read Byte / Byte Ready ========== */

int ffi_read_byte(int fd) {
    unsigned char c;
    ssize_t n;
    do {
        n = read(fd, &c, 1);
    } while (n < 0 && errno == EINTR);
    if (n == 1) return (int)c;
    return -1;
}

int ffi_byte_ready(int fd) {
    struct pollfd pfd;
    pfd.fd = fd;
    pfd.events = POLLIN;
    pfd.revents = 0;
    int ret = poll(&pfd, 1, 0);
    return (ret > 0 && (pfd.revents & POLLIN)) ? 1 : 0;
}

/* ========== Terminal Size ========== */

static int _ws_row = 0, _ws_col = 0;

int ffi_get_winsize(int fd) {
    struct winsize ws;
    if (ioctl(fd, TIOCGWINSZ, &ws) == 0) {
        _ws_row = ws.ws_row;
        _ws_col = ws.ws_col;
        return 0;
    }
    return -1;
}

int ffi_ws_col(void) { return _ws_col; }
int ffi_ws_row(void) { return _ws_row; }

/* ========== SOH Unpacking for execve ========== */

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

/* ========== Fork+Exec ========== */

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

int ffi_fork_exec(const char *path, const char *packed_argv,
                   const char *packed_env, int pgid,
                   const char *packed_keep_fds, const char *cwd) {
    ffi_sigchld_block();

    pid_t child = fork();
    if (child < 0) {
        ffi_sigchld_unblock();
        return -1;
    }

    if (child > 0) {
        /* Parent */
        if (pgid == -1) setpgid(child, child);
        else if (pgid > 0) setpgid(child, pgid);
        return (int)child;
    }

    /* Child (pure C, no Scheme) */

    /* 1. Reset signals */
    int sigs[] = {SIGINT, SIGQUIT, SIGTERM, SIGHUP, SIGPIPE,
                  SIGTSTP, SIGTTIN, SIGTTOU, SIGCHLD, 0};
    for (int i = 0; sigs[i]; i++)
        signal(sigs[i], SIG_DFL);

    /* 2. Unblock all signals */
    sigset_t all;
    sigfillset(&all);
    sigprocmask(SIG_UNBLOCK, &all, NULL);

    /* 3. Process group */
    if (pgid == -1) setpgid(0, 0);
    else if (pgid > 0) setpgid(0, pgid);

    /* 4. Close excess fds */
    {
        int keep[64];
        int n_keep = 0;
        if (packed_keep_fds && packed_keep_fds[0]) {
            const char *p = packed_keep_fds;
            while (*p && n_keep < 64) {
                keep[n_keep++] = atoi(p);
                while (*p && *p != '\001') p++;
                if (*p == '\001') p++;
            }
        }
        close_fds_except(keep, n_keep);
    }

    /* 5. Set CWD */
    if (cwd && cwd[0]) chdir(cwd);

    /* 6. Unpack and exec */
    char *argv_copy, *env_copy;
    int argc, envc;
    char **argv = unpack_soh(packed_argv, &argv_copy, &argc);
    char **envp = unpack_soh(packed_env, &env_copy, &envc);
    if (!argv || !envp) _exit(126);

    execve(path, argv, envp);

    if (errno == ENOEXEC) {
        char **new_argv = (char **)alloca((argc + 2) * sizeof(char*));
        new_argv[0] = "/bin/sh";
        for (int i = 0; i <= argc; i++) new_argv[i + 1] = argv[i];
        execve("/bin/sh", new_argv, envp);
    }

    _exit(errno == ENOENT ? 127 : 126);
    return -1;
}

/* ========== Execve (replace current process) ========== */

int ffi_do_execve(const char *path, const char *packed_argv, const char *packed_env) {
    char *argv_copy = NULL, *envp_copy = NULL;
    int argc, envc;
    char **argv = unpack_soh(packed_argv, &argv_copy, &argc);
    char **envp = unpack_soh(packed_env, &envp_copy, &envc);
    if (!argv || !envp) { free(argv); free(envp); free(argv_copy); free(envp_copy); return -1; }
    execve(path, argv, envp);
    /* If execve returns, it failed */
    int err = errno;
    free(argv); free(envp); free(argv_copy); free(envp_copy);
    return -err;  /* negative errno */
}

/* ========== FD Read/Write ========== */

int ffi_fdread(int fd, char *buf, int count) {
    ssize_t n;
    do { n = read(fd, buf, count); } while (n < 0 && errno == EINTR);
    return (int)n;
}

int ffi_fdwrite(int fd, const char *buf, int count) {
    ssize_t n;
    do { n = write(fd, buf, count); } while (n < 0 && errno == EINTR);
    return (int)n;
}

/* ========== Stat ========== */

/* Returns file type as int:
   0=regular, 1=directory, 2=symlink, 3=fifo,
   4=block-special, 5=character-special, 6=socket, 7=other, -1=error */
int ffi_file_type(const char *path, int follow_links) {
    struct stat st;
    int rc = follow_links ? stat(path, &st) : lstat(path, &st);
    if (rc != 0) return -1;
    if (S_ISREG(st.st_mode)) return 0;
    if (S_ISDIR(st.st_mode)) return 1;
    if (S_ISLNK(st.st_mode)) return 2;
    if (S_ISFIFO(st.st_mode)) return 3;
    if (S_ISBLK(st.st_mode)) return 4;
    if (S_ISCHR(st.st_mode)) return 5;
    if (S_ISSOCK(st.st_mode)) return 6;
    return 7;
}

/* Full stat info: returns 0 on success, -1 on error.
   Results returned via out array: [mode, uid, gid, dev, ino, nlink, atime] */
int ffi_stat(const char *path, int follow_links,
             long long *out_size, long long *out_mtime,
             int *out_type, int *out_mode,
             int *out_uid, int *out_gid,
             long long *out_dev, long long *out_ino, long long *out_nlink) {
    struct stat st;
    int rc = follow_links ? stat(path, &st) : lstat(path, &st);
    if (rc != 0) return -1;

    *out_size = (long long)st.st_size;
    *out_mtime = (long long)st.st_mtime;
    *out_mode = (int)(st.st_mode & 07777);
    *out_uid = (int)st.st_uid;
    *out_gid = (int)st.st_gid;
    *out_dev = (long long)st.st_dev;
    *out_ino = (long long)st.st_ino;
    *out_nlink = (long long)st.st_nlink;

    if (S_ISREG(st.st_mode)) *out_type = 0;
    else if (S_ISDIR(st.st_mode)) *out_type = 1;
    else if (S_ISLNK(st.st_mode)) *out_type = 2;
    else if (S_ISFIFO(st.st_mode)) *out_type = 3;
    else if (S_ISBLK(st.st_mode)) *out_type = 4;
    else if (S_ISCHR(st.st_mode)) *out_type = 5;
    else if (S_ISSOCK(st.st_mode)) *out_type = 6;
    else *out_type = 7;

    return 0;
}

long long ffi_file_size(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    return (long long)st.st_size;
}

long long ffi_file_mtime(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    return (long long)st.st_mtime;
}

int ffi_file_mode(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    return (int)(st.st_mode & 07777);
}

int ffi_file_uid(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    return (int)st.st_uid;
}

int ffi_file_gid(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    return (int)st.st_gid;
}

long long ffi_file_dev(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    return (long long)st.st_dev;
}

long long ffi_file_ino(const char *path) {
    struct stat st;
    if (stat(path, &st) != 0) return -1;
    return (long long)st.st_ino;
}

long long ffi_file_ino_lstat(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) return -1;
    return (long long)st.st_ino;
}

long long ffi_file_dev_lstat(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) return -1;
    return (long long)st.st_dev;
}
