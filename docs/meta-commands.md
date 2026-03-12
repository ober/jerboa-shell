# jsh Meta-Commands

Meta-commands are invoked by prefixing a line with `,` at the jsh prompt (or via
`-c` in scripts). They extend the shell with Scheme-level introspection, sandboxing,
profiling, and tracing.

---

## `,sb` — Sandboxed command/script execution

Runs a shell command or script inside a capability sandbox that restricts filesystem
access, network access, and execution, with an optional timeout.

```
,sb [options] [-c cmd | script.sh]

Options:
  -r path     allow reading  path (can repeat)
  -w path     allow writing  path (can repeat)
  -x cmd      allow executing cmd (can repeat)
  --net       allow network access
  --no-net    deny  network access (default)
  -t ms       timeout in milliseconds
  -c cmd      run inline shell command
  --help      show this message
```

```bash
# Read-only access to /tmp
,sb -r /tmp -c "cat /tmp/notes.txt"

# Build script: deny network, 30-second timeout
,sb -r /home/user/project -w /tmp -x /usr/bin/make --no-net -t 30000 build.sh

# Allow git but nothing else
,sb -r . -x /usr/bin/git -x /bin/sh -c "git status"
```

---

## `,(with-sandbox ...)` — Scheme-level sandbox expression

For use inside the `,` Gerbil eval environment. Uses postfix-colon keyword syntax
(`keyword:` not `#:keyword`).

```
,(with-sandbox kw: val ... body)

Keywords:
  allow-read:   list of paths to allow reading
  allow-write:  list of paths to allow writing
  allow-exec:   list of executables to allow
  deny-net:     #t to block network (default)
  allow-net:    #t to allow network
  timeout-ms:   integer timeout in milliseconds
```

The `body` is the last argument and may call `run-cmd` or `run-script`, which
dispatch through the live jsh session.

```scheme
# Run a sandboxed shell command
,(with-sandbox allow-read: '("/tmp") (run-cmd "cat /tmp/hello.txt"))

# Run a script, deny network, 60-second timeout
,(with-sandbox
    allow-read:  '("/home/user/proj" "/usr/lib")
    allow-write: '("/tmp")
    allow-exec:  '("/usr/bin/make")
    deny-net:    #t
    timeout-ms:  60000
    (run-script "build.sh"))
```

`run-cmd` and `run-script` are also callable directly in `,` eval without a sandbox:

```scheme
,(run-cmd "echo hello from jsh")
,(run-script "myscript.sh" "arg1" "arg2")
```

---

## `,trace` — Pipeline structure visualization with timing

Parses and displays the pipeline structure of a command before running it, then
shows wall-clock execution time.

```
,trace [-c cmd | script.sh]
```

```bash
,trace -c "ls | grep .ss | wc -l"
```

```
=== Trace: ls | grep .ss | wc -l ===
Pipeline: 3 stages
  [1] ls              → stdout→pipe1
  [2] grep .ss  ← pipe1 → stdout→pipe2
  [3] wc -l     ← pipe2 → stdout

Status: 0
Wall time: 0.012s
```

```bash
# Trace a script file
,trace build.sh
```

---

## `,profile` — Per-command timing

Runs a script (or inline command) with instrumentation, then prints each command's
elapsed time plus a summary with the slowest command highlighted.

```
,profile [-c cmd | script.sh]
```

```bash
,profile deploy.sh
```

```
=== jsh profile: deploy.sh ===
  0.002s  echo Starting deploy
  1.847s  make all
  0.341s  rsync -av dist/ server:/var/www/
  0.001s  echo Done

--- Summary ---
  Total:    2.191s
  Commands: 4
  Slowest:  make all (1.847s, 84%)
```

```bash
# Profile an inline command
,profile -c "find /usr -name '*.so' | wc -l"
```

---

## `,room` — Heap/GC introspection

Prints a Chez Scheme memory report: heap size, GC stats, per-generation breakdown,
and top object types.

```bash
,room        # standard report (top 15 object types)
,room #t     # verbose: all object types
```

---

## `,use file.ss` — Load a Gerbil source file

Reads a `.ss` file through the Gerbil reader → Gherkin compiler → Chez `eval`.
`(export ...)` and `(import ...)` forms are skipped; the Gerbil runtime is
pre-loaded automatically on first use.

```bash
,use ~/scripts/my-lib.ss
,use ./helpers.ss
```

---

## Quick reference

| Command              | What it does                                      |
|----------------------|---------------------------------------------------|
| `,sb -c "cmd"`       | Run command in capability sandbox                 |
| `,sb script.sh`      | Run script in capability sandbox                  |
| `,trace -c "cmd"`    | Show pipeline structure + timing for command      |
| `,trace script.sh`   | Show timing for each stage of a script            |
| `,profile -c "cmd"`  | Per-command timing breakdown for inline command   |
| `,profile script.sh` | Per-command timing breakdown for script           |
| `,room`              | Chez Scheme heap/GC report                        |
| `,room #t`           | Verbose heap report (all object types)            |
| `,use file.ss`       | Compile and load a Gerbil source file             |
| `,(expr)`            | Evaluate a Gerbil expression in the shell session |
