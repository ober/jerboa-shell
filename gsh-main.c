/*
 * gsh-main.c — Custom entry point for gsh.
 *
 * Chez Scheme's default main() steals flags like -c (interprets as --compact).
 * This custom main bypasses Chez's arg parsing: it saves all user args in
 * positional env vars (GSH_ARGC, GSH_ARG0, GSH_ARG1, ...), then calls the
 * Chez runtime with no user args. The Scheme entry point reads these env vars.
 *
 * Boot files (petite.boot, scheme.boot, gsh.boot) are embedded as C byte
 * arrays and registered via Sregister_boot_file_bytes — no external files needed.
 *
 * Threading workaround: Programs embedded in boot files (via make-boot-file)
 * cannot create threads — fork-thread creates OS threads that block forever
 * on an internal GC futex. To fix this, we load only libraries via the boot
 * file and run the program separately via Sscheme_script, which preserves
 * full threading support.
 *
 * The program .so is embedded in the binary as a C byte array (gsh_program.h)
 * and extracted to a memfd at runtime.
 */

#define _GNU_SOURCE
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include "scheme.h"
#include "gsh_program.h"      /* generated: gsh_program_data[], gsh_program_size */
#include "gsh_petite_boot.h"  /* generated: petite_boot_data[], petite_boot_size */
#include "gsh_scheme_boot.h"  /* generated: scheme_boot_data[], scheme_boot_size */
#include "gsh_gsh_boot.h"     /* generated: gsh_boot_data[], gsh_boot_size */

int main(int argc, char *argv[]) {
    /* Save args in positional env vars: GSH_ARGC, GSH_ARG0, GSH_ARG1, ... */
    char countbuf[32];
    snprintf(countbuf, sizeof(countbuf), "%d", argc - 1);
    setenv("GSH_ARGC", countbuf, 1);

    for (int i = 1; i < argc; i++) {
        char name[32];
        snprintf(name, sizeof(name), "GSH_ARG%d", i - 1);
        setenv(name, argv[i], 1);
    }

    /* Resolve real executable path for $SHELL (argv[0] may be relative,
     * and command-line in Scheme returns the memfd /proc/self/fd/N path) */
    {
        char exe_buf[4096];
        ssize_t len = readlink("/proc/self/exe", exe_buf, sizeof(exe_buf) - 1);
        if (len > 0) {
            exe_buf[len] = '\0';
            setenv("GSH_EXE", exe_buf, 1);
        }
    }

    /* Create memfd for embedded program .so */
    int fd = memfd_create("gsh-program", MFD_CLOEXEC);
    if (fd < 0) {
        perror("memfd_create");
        return 1;
    }
    if (write(fd, gsh_program_data, gsh_program_size) != (ssize_t)gsh_program_size) {
        perror("write memfd");
        close(fd);
        return 1;
    }
    char prog_path[64];
    snprintf(prog_path, sizeof(prog_path), "/proc/self/fd/%d", fd);

    /* Initialize Chez Scheme */
    Sscheme_init(NULL);

    /* Register embedded boot files (no external files needed) */
    Sregister_boot_file_bytes("petite", (void*)petite_boot_data, petite_boot_size);
    Sregister_boot_file_bytes("scheme", (void*)scheme_boot_data, scheme_boot_size);
    Sregister_boot_file_bytes("gsh",    (void*)gsh_boot_data,    gsh_boot_size);

    /* Build heap from registered boot files (libraries only — no program) */
    Sbuild_heap(NULL, NULL);

    /* Run the program via Sscheme_script (NOT Sscheme_start).
     * This avoids the Chez bug where programs in boot files cannot
     * create threads (fork-thread threads block on internal GC futex). */
    const char *script_args[] = { argv[0] };
    int status = Sscheme_script(prog_path, 1, script_args);

    close(fd);
    Sscheme_deinit();
    return status;
}
