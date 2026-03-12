#!/bin/bash
# build-jsh-musl.sh — Build jsh with musl for zero-dependency static binary
set -euo pipefail

JERBOA_DIR="${JERBOA_DIR:-$HOME/mine/jerboa}"
JERBOA_LIB="${JERBOA_DIR}/lib"
GHERKIN_DIR="${GHERKIN_DIR:-$HOME/mine/gherkin/src}"

echo "==================================="
echo "Building jsh with musl libc"
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

# Check if musl Chez is available
if [ ! -d "/opt/chez-musl" ] && [ -z "${JERBOA_MUSL_CHEZ_PREFIX:-}" ]; then
    echo "WARNING: musl-built Chez Scheme not found"
    echo ""
    echo "To build fully static jsh with musl:"
    echo "  1. Build Chez Scheme with musl:"
    echo "     cd $JERBOA_DIR"
    echo "     sudo support/musl-chez-build.sh"
    echo ""
    echo "  2. Or set JERBOA_MUSL_CHEZ_PREFIX if installed elsewhere"
    echo ""
    echo "For now, using scheme to generate build artifacts..."
    echo ""
fi

# Step 1: Build FFI shim
echo "[1/5] Building FFI shim..."
musl-gcc -shared -fPIC -static -o libjsh-ffi-musl.so ffi-shim.c || {
    echo "WARNING: FFI shim build failed, trying without -static"
    musl-gcc -shared -fPIC -o libjsh-ffi-musl.so ffi-shim.c
}

# Step 2: Compile jsh modules
echo "[2/5] Compiling jsh modules..."
scheme -q --libdirs "src:${JERBOA_LIB}:${GHERKIN_DIR}" \
    --compile-imported-libraries < build-jsh.ss

# Step 3: Use jerboa musl module to build static binary
echo "[3/5] Generating musl build script..."
cat > build-jsh-musl.ss << 'EOSCRIPT'
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
EOSCRIPT

# Step 4: Run musl build
echo "[4/5] Building with musl..."
LD_LIBRARY_PATH=. scheme -q --libdirs "src:${JERBOA_LIB}:${GHERKIN_DIR}" \
    < build-jsh-musl.ss || {
    echo ""
    echo "NOTE: musl build requires musl-built Chez Scheme"
    echo "      See: $JERBOA_DIR/support/musl-chez-build.sh"
    echo ""
    echo "      For now, falling back to regular static build with musl-gcc..."
    echo ""
    
    # Fallback: use regular build but with musl-gcc
    make jsh-binary CC=musl-gcc CFLAGS="-static"
    exit 0
}

# Step 5: Verify
echo "[5/5] Verifying binary..."
if [ -f "jsh-musl" ]; then
    echo ""
    echo "==================================="
    echo "✓ jsh-musl built successfully!"
    echo "==================================="
    ls -lh jsh-musl
    echo ""
    file jsh-musl
    echo ""
    ldd jsh-musl 2>&1 || echo "  (Fully static - no dependencies)"
    echo ""
    echo "Test: ./jsh-musl -c 'echo Hello'"
    echo ""
else
    echo "ERROR: jsh-musl not created"
    exit 1
fi
