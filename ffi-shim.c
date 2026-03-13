#define _GNU_SOURCE
/* ffi-shim.c -- POSIX FFI bindings for gherkin-shell
 * Compile: gcc -shared -fPIC -o libjsh-ffi.so src/jsh/ffi-shim.c -Wall -O2
 *
 * All functions are standard POSIX libc — no external libraries needed.
 * Functions prefixed with ffi_ to avoid symbol conflicts.
 */

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/resource.h>
#include <sys/ioctl.h>
#include <sys/prctl.h>
#include <sys/syscall.h>
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
#include <stdint.h>

/* ========== Landlock Definitions ========== */
/* Defined inline — neither glibc nor musl provides wrappers. */

#ifndef __NR_landlock_create_ruleset
#define __NR_landlock_create_ruleset 444
#endif
#ifndef __NR_landlock_add_rule
#define __NR_landlock_add_rule 445
#endif
#ifndef __NR_landlock_restrict_self
#define __NR_landlock_restrict_self 446
#endif

#define LANDLOCK_CREATE_RULESET_VERSION (1U << 0)

#define LANDLOCK_ACCESS_FS_EXECUTE      (1ULL << 0)
#define LANDLOCK_ACCESS_FS_WRITE_FILE   (1ULL << 1)
#define LANDLOCK_ACCESS_FS_READ_FILE    (1ULL << 2)
#define LANDLOCK_ACCESS_FS_READ_DIR     (1ULL << 3)
#define LANDLOCK_ACCESS_FS_REMOVE_DIR   (1ULL << 4)
#define LANDLOCK_ACCESS_FS_REMOVE_FILE  (1ULL << 5)
#define LANDLOCK_ACCESS_FS_MAKE_CHAR    (1ULL << 6)
#define LANDLOCK_ACCESS_FS_MAKE_DIR     (1ULL << 7)
#define LANDLOCK_ACCESS_FS_MAKE_REG     (1ULL << 8)
#define LANDLOCK_ACCESS_FS_MAKE_SOCK    (1ULL << 9)
#define LANDLOCK_ACCESS_FS_MAKE_FIFO    (1ULL << 10)
#define LANDLOCK_ACCESS_FS_MAKE_BLOCK   (1ULL << 11)
#define LANDLOCK_ACCESS_FS_MAKE_SYM     (1ULL << 12)
#define LANDLOCK_ACCESS_FS_REFER        (1ULL << 13)
#define LANDLOCK_ACCESS_FS_TRUNCATE     (1ULL << 14)
#define LANDLOCK_ACCESS_FS_IOCTL_DEV    (1ULL << 15)

#define LANDLOCK_RULE_PATH_BENEATH 1

struct landlock_ruleset_attr {
    uint64_t handled_access_fs;
    uint64_t handled_access_net;
};

struct landlock_path_beneath_attr {
    uint64_t allowed_access;
    int32_t  parent_fd;
} __attribute__((packed));

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

/* Copy read buffer into a pre-allocated byte array (u8* in Chez FFI).
   Returns the number of bytes actually copied. */
int ffi_copy_read_buf(unsigned char *out, int maxlen) {
    int n = (int)_ffi_read_buf_len;
    if (n > maxlen) n = maxlen;
    memcpy(out, _ffi_read_buf, n);
    return n;
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

/* Write a single raw byte to a file descriptor, bypassing Chez text port encoding */
int ffi_write_byte(int fd, int byte) {
    unsigned char b = (unsigned char)byte;
    ssize_t n;
    do { n = write(fd, &b, 1); } while (n < 0 && errno == EINTR);
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

/* ========== Landlock Sandbox ========== */

#define ACCESS_FS_READ ( \
    LANDLOCK_ACCESS_FS_EXECUTE   | \
    LANDLOCK_ACCESS_FS_READ_FILE | \
    LANDLOCK_ACCESS_FS_READ_DIR)

#define ACCESS_FS_WRITE ( \
    LANDLOCK_ACCESS_FS_WRITE_FILE  | \
    LANDLOCK_ACCESS_FS_REMOVE_DIR  | \
    LANDLOCK_ACCESS_FS_REMOVE_FILE | \
    LANDLOCK_ACCESS_FS_MAKE_CHAR   | \
    LANDLOCK_ACCESS_FS_MAKE_DIR    | \
    LANDLOCK_ACCESS_FS_MAKE_REG    | \
    LANDLOCK_ACCESS_FS_MAKE_SOCK   | \
    LANDLOCK_ACCESS_FS_MAKE_FIFO   | \
    LANDLOCK_ACCESS_FS_MAKE_BLOCK  | \
    LANDLOCK_ACCESS_FS_MAKE_SYM)

/* Query Landlock ABI version. Returns version (>=1) or -1 if unsupported. */
int ffi_landlock_abi_version(void) {
    int v = syscall(__NR_landlock_create_ruleset, NULL, 0,
                    LANDLOCK_CREATE_RULESET_VERSION);
    if (v < 0) return -1;
    return v;
}

/* Get the full set of handled_access_fs flags for a given ABI version. */
static uint64_t landlock_handled_fs(int abi) {
    uint64_t a = ACCESS_FS_READ | ACCESS_FS_WRITE;
    if (abi >= 2) a |= LANDLOCK_ACCESS_FS_REFER;
    if (abi >= 3) a |= LANDLOCK_ACCESS_FS_TRUNCATE;
    if (abi >= 5) a |= LANDLOCK_ACCESS_FS_IOCTL_DEV;
    return a;
}

/*
 * ffi_landlock_sandbox — Apply Landlock restrictions to the current process.
 *
 * packed_read:  SOH-separated paths for read-only access (or empty/NULL)
 * packed_write: SOH-separated paths for read+write access (or empty/NULL)
 * packed_exec:  SOH-separated paths for execute access (or empty/NULL)
 *
 * Returns: 0 on success, 1 if Landlock unsupported, -1 on error.
 *
 * Once applied, restrictions are PERMANENT and IRREVERSIBLE for this process
 * and all children. This is the point — it's real enforcement.
 */
int ffi_landlock_sandbox(const char *packed_read,
                         const char *packed_write,
                         const char *packed_exec) {
    /* 1. Check ABI version */
    int abi = syscall(__NR_landlock_create_ruleset, NULL, 0,
                      LANDLOCK_CREATE_RULESET_VERSION);
    if (abi < 0) {
        if (errno == ENOSYS || errno == EOPNOTSUPP)
            return 1;  /* unsupported — graceful degradation */
        return -1;
    }

    /* 2. Create ruleset handling all known FS access types */
    uint64_t handled = landlock_handled_fs(abi);
    struct landlock_ruleset_attr attr;
    memset(&attr, 0, sizeof(attr));
    attr.handled_access_fs = handled;

    int ruleset_fd = syscall(__NR_landlock_create_ruleset,
                             &attr, sizeof(attr), 0);
    if (ruleset_fd < 0) return -1;

    /* Helper: add one path rule */
    #define ADD_RULE(path, access) do { \
        int fd = open((path), O_PATH | O_CLOEXEC); \
        if (fd >= 0) { \
            struct landlock_path_beneath_attr pb; \
            pb.allowed_access = (access) & handled; \
            pb.parent_fd = fd; \
            syscall(__NR_landlock_add_rule, ruleset_fd, \
                    LANDLOCK_RULE_PATH_BENEATH, &pb, 0); \
            close(fd); \
        } \
    } while(0)

    /* 3. Parse packed paths and add rules */

    /* Always allow read access to essential system paths for exec to work */
    ADD_RULE("/usr", ACCESS_FS_READ);
    ADD_RULE("/lib", ACCESS_FS_READ);
    ADD_RULE("/lib64", ACCESS_FS_READ);
    ADD_RULE("/bin", ACCESS_FS_READ);
    ADD_RULE("/sbin", ACCESS_FS_READ);
    ADD_RULE("/etc", ACCESS_FS_READ);
    /* /proc and /dev needed for basic operation */
    ADD_RULE("/proc", ACCESS_FS_READ);
    ADD_RULE("/dev", ACCESS_FS_READ | LANDLOCK_ACCESS_FS_WRITE_FILE);

    /* Read-only paths from user */
    if (packed_read && packed_read[0]) {
        const char *p = packed_read;
        while (*p) {
            const char *end = p;
            while (*end && *end != '\001') end++;
            char path[4096];
            int len = end - p;
            if (len > 0 && len < (int)sizeof(path)) {
                memcpy(path, p, len);
                path[len] = '\0';
                ADD_RULE(path, ACCESS_FS_READ);
            }
            p = *end ? end + 1 : end;
        }
    }

    /* Read+write paths from user */
    if (packed_write && packed_write[0]) {
        const char *p = packed_write;
        while (*p) {
            const char *end = p;
            while (*end && *end != '\001') end++;
            char path[4096];
            int len = end - p;
            if (len > 0 && len < (int)sizeof(path)) {
                memcpy(path, p, len);
                path[len] = '\0';
                ADD_RULE(path, ACCESS_FS_READ | ACCESS_FS_WRITE);
            }
            p = *end ? end + 1 : end;
        }
    }

    /* Execute paths from user (read + execute) */
    if (packed_exec && packed_exec[0]) {
        const char *p = packed_exec;
        while (*p) {
            const char *end = p;
            while (*end && *end != '\001') end++;
            char path[4096];
            int len = end - p;
            if (len > 0 && len < (int)sizeof(path)) {
                memcpy(path, p, len);
                path[len] = '\0';
                ADD_RULE(path, ACCESS_FS_READ | LANDLOCK_ACCESS_FS_EXECUTE);
            }
            p = *end ? end + 1 : end;
        }
    }

    #undef ADD_RULE

    /* 4. Set no_new_privs (mandatory before landlock_restrict_self) */
    if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0)) {
        close(ruleset_fd);
        return -1;
    }

    /* 5. Enforce — PERMANENT and IRREVERSIBLE */
    if (syscall(__NR_landlock_restrict_self, ruleset_fd, 0)) {
        close(ruleset_fd);
        return -1;
    }

    close(ruleset_fd);
    return 0;
}

/*
 * ffi_sandbox_fork_exec — Fork, apply Landlock in child, then exec.
 *
 * Like ffi_fork_exec but the child process is sandboxed before exec.
 * The parent process is NOT affected.
 *
 * packed_read/write/exec: SOH-separated allowed paths (same as ffi_landlock_sandbox)
 * path, packed_argv, packed_env, cwd: same as ffi_fork_exec
 *
 * Returns: child pid on success, -1 on error.
 */
int ffi_sandbox_fork_exec(const char *path, const char *packed_argv,
                          const char *packed_env, const char *cwd,
                          const char *packed_read, const char *packed_write,
                          const char *packed_exec) {
    ffi_sigchld_block();

    pid_t child = fork();
    if (child < 0) {
        ffi_sigchld_unblock();
        return -1;
    }

    if (child > 0) {
        /* Parent — unaffected by sandbox */
        return (int)child;
    }

    /* Child — apply sandbox then exec */

    /* Reset signals */
    int sigs[] = {SIGINT, SIGQUIT, SIGTERM, SIGHUP, SIGPIPE,
                  SIGTSTP, SIGTTIN, SIGTTOU, SIGCHLD, 0};
    for (int i = 0; sigs[i]; i++)
        signal(sigs[i], SIG_DFL);
    sigset_t all;
    sigfillset(&all);
    sigprocmask(SIG_UNBLOCK, &all, NULL);

    /* Set CWD before sandbox (sandbox may restrict access to cwd) */
    if (cwd && cwd[0]) chdir(cwd);

    /* Apply Landlock sandbox — BEFORE exec */
    int sb_ret = ffi_landlock_sandbox(packed_read, packed_write, packed_exec);
    if (sb_ret < 0) {
        fprintf(stderr, "sandbox: Landlock enforcement failed\n");
        _exit(126);
    }
    /* sb_ret == 1 means unsupported — continue without sandbox (logged) */
    if (sb_ret == 1) {
        fprintf(stderr, "sandbox: Landlock not supported by kernel, "
                        "running without enforcement\n");
    }

    /* Close excess fds */
    {
        int keep[3] = {0, 1, 2};
        close_fds_except(keep, 3);
    }

    /* Unpack and exec */
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
