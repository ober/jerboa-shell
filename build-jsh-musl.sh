#!/bin/bash
# build-jsh-musl.sh — Build jsh as a fully static binary using musl libc
#
# Prerequisites:
#   - musl-gcc installed (apt install musl-tools)
#   - Chez Scheme built with: ./configure --threads --static CC=musl-gcc
#     and installed to ~/chez-musl (or set JERBOA_MUSL_CHEZ_PREFIX)
#   - Jerboa and Gherkin libraries compiled
#
# The build uses stock scheme (glibc) for the Scheme compilation steps,
# then musl-gcc for the C compilation and linking steps.
set -euo pipefail

JERBOA_DIR="${JERBOA_DIR:-$HOME/mine/jerboa}"
JERBOA_LIB="${JERBOA_DIR}/lib"
GHERKIN_DIR="${GHERKIN_DIR:-$HOME/mine/gherkin/src}"

echo "==================================="
echo "Building jsh with musl libc (static)"
echo "==================================="
echo ""
echo "Jerboa:  $JERBOA_LIB"
echo "Gherkin: $GHERKIN_DIR"
echo ""

# Check musl availability
if ! command -v musl-gcc &>/dev/null; then
	echo "ERROR: musl-gcc not found"
	echo "Install: sudo apt install musl-tools"
	exit 1
fi

# Use jerboa's musl module to validate and build
echo "[1/2] Validating musl toolchain via jerboa..."
scheme -q --libdirs "src:${JERBOA_LIB}:${GHERKIN_DIR}" <<'VALIDATE'
(import (chezscheme) (jerboa build musl))
(let ([result (validate-musl-setup)])
  (printf "  ~a: ~a~n" (car result) (cdr result))
  (unless (eq? (car result) 'ok)
    (exit 1)))
VALIDATE

echo ""
echo "[2/2] Running musl build..."
LD_LIBRARY_PATH=. scheme -q --libdirs "src:${JERBOA_LIB}:${GHERKIN_DIR}" \
	<build-jsh-musl.ss

# Verify
if [ -f "jsh-musl" ]; then
	echo ""
	echo "==================================="
	echo "jsh-musl built successfully!"
	echo "==================================="
	ls -lh jsh-musl
	echo ""
	file jsh-musl
	echo ""
	ldd jsh-musl 2>&1 || echo "  (Fully static - no dependencies)"
	echo ""
	echo "Test: ./jsh-musl -c 'echo Hello from static jsh'"
else
	echo "ERROR: jsh-musl not created"
	exit 1
fi
