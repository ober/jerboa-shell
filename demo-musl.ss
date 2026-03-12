#!/usr/bin/env scheme-script
#!chezscheme

;;; Demo: jerboa-shell with musl static build support
;;;
;;; Run with: scheme --libdirs ~/mine/jerboa/lib --script demo-musl.ss

(import (chezscheme)
        (jerboa build)
        (jerboa build musl))

(display "====================================\n")
(display "jerboa-shell musl Build Demo\n")
(display "====================================\n\n")

;; Step 1: Check musl-gcc
(display "1. Checking musl toolchain...\n")
(if (musl-available?)
    (begin
      (display "   ✓ musl-gcc found: ")
      (display (musl-gcc-path))
      (newline))
    (begin
      (display "   ✗ musl-gcc not found\n")
      (display "   Install: sudo apt install musl-tools\n")
      (exit 1)))

;; Step 2: Validate setup
(display "\n2. Validating musl setup...\n")
(let ([result (validate-musl-setup)])
  (display "   Status: ")
  (display (car result))
  (newline)
  (display "   ")
  (display (cdr result))
  (newline)
  
  (unless (eq? (car result) 'ok)
    (display "\n   Note: Full musl build requires musl-built Chez Scheme\n")
    (display "   See: ~/mine/jerboa/support/musl-chez-build.sh\n")))

;; Step 3: Show configuration
(display "\n3. Configuration:\n")
(display "   musl Chez prefix: ")
(display (musl-chez-prefix))
(newline)
(display "   musl sysroot: ")
(display (musl-sysroot))
(newline)

;; Step 4: Show build command
(display "\n4. To build static jsh:\n\n")
(display "   make musl-jsh\n\n")
(display "   This creates: jsh-musl (zero-dependency static binary)\n\n")

(display "====================================\n")
(display "Setup validated!\n")
(display "====================================\n")
