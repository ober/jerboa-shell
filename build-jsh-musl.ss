#!chezscheme
(import (chezscheme)
        (jerboa build)
        (jerboa build musl))

;; Validate musl setup
(let ([result (validate-musl-setup)])
  (unless (eq? (car result) 'ok)
    (printf "Error: ~a~n" (cdr result))
    (printf "~nTo build Chez with musl:~n")
    (printf "  cd ~/mine/jerboa && sudo support/musl-chez-build.sh~n")
    (exit 1)))

(printf "musl Chez Scheme found: ~a~n" (musl-chez-prefix))
(printf "Building static jsh binary...~n~n")

;; Build with musl
(build-musl-binary "jsh.ss" "jsh-musl"
  'optimize-level: 3
  'extra-c-files: '("ffi-shim.c")
  'extra-cflags: "-I."
  'static-libs: '()
  'verbose: #t)

(printf "~nSuccess! Static binary created: jsh-musl~n")
(printf "Test with: ./jsh-musl -c 'echo Hello from static jsh'~n")
