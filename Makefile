SCHEME = scheme
JERBOA ?= $(HOME)/mine/jerboa/lib
GHERKIN ?= $(HOME)/mine/gherkin/src
# Compiler needs gherkin's runtime; compiled output needs jerboa's stdlib
LIBDIRS = src:$(JERBOA):$(GHERKIN)
LIBDIRS_JSH = src:$(JERBOA):$(GHERKIN)

.PHONY: ffi jerboa compile run clean test binary all jsh jsh-compile jsh-run jsh-binary compat-test musl musl-jsh

all: ffi jerboa compile

# ─── Legacy jsh targets (submodule-based) ────────────────────────────────────

ffi:
	@echo "=== Building FFI shim ==="
	gcc -shared -fPIC -o libjsh-ffi.so ffi-shim.c

jerboa:
	@echo "=== Translating .ss → .sls via Gherkin compiler (with Jerboa imports) ==="
	$(SCHEME) -q --libdirs $(LIBDIRS) --compile-imported-libraries < build-jerboa.ss

compile:
	@echo "=== Compiling .sls → .so ==="
	$(SCHEME) -q --libdirs $(LIBDIRS) --compile-imported-libraries < build-all.ss

run:
	LD_LIBRARY_PATH=. $(SCHEME) --libdirs $(LIBDIRS) --program jsh.ss

binary: all
	@echo "=== Building standalone jsh binary ==="
	LD_LIBRARY_PATH=. $(SCHEME) -q --libdirs $(LIBDIRS) < build-binary.ss

# ─── jsh targets (inline source, no submodule) ───────────────────────────────

jsh-compile:
	@echo "=== Compiling src/jsh/*.sls → .so ==="
	$(SCHEME) -q --libdirs $(LIBDIRS_JSH) --compile-imported-libraries < build-jsh.ss

jsh-binary: jsh-compile
	@echo "=== Building standalone jsh binary ==="
	LD_LIBRARY_PATH=. $(SCHEME) -q --libdirs $(LIBDIRS_JSH) < build-binary-jsh.ss

jsh-run:
	LD_LIBRARY_PATH=. $(SCHEME) --libdirs $(LIBDIRS_JSH) --program jsh.ss

jsh: ffi jsh-binary
	@echo "=== jsh binary ready ==="
	@ls -lh jsh

# ─── musl Static Binary ──────────────────────────────────────────────────────

musl-jsh:
	@echo "=== Building static jsh with musl ==="
	./build-jsh-musl.sh

musl:
	@echo "=== Testing musl module ==="
	@$(SCHEME) --libdirs $(JERBOA) -q << 'EOFTEST' || true; \
	(import (jerboa build musl)); \
	(display "musl-gcc: "); \
	(display (if (musl-available?) "available" "not found")); \
	(newline); \
	(when (musl-available?) \
	  (let ([status (validate-musl-setup)]) \
	    (display "Status: ") \
	    (display (car status)) \
	    (display " - ") \
	    (display (cdr status)) \
	    (newline)))
	EOFTEST

# ─── Testing ─────────────────────────────────────────────────────────────────

test:
	@echo "=== Running unit tests ==="
	LD_LIBRARY_PATH=. $(SCHEME) --libdirs src:$(JERBOA) --script test/test-jsh.ss

compat-test:
	@echo "=== Running Oils compat tests vs jsh ==="
	python3 gerbil-shell/test/gen_compat_report.py \
	  /home/jafourni/mine/gerbil-shell/_vendor/oils \
	  ./jsh \
	  gerbil-shell/.gerbil/bin/jsh 2>/dev/null || \
	python3 gerbil-shell/test/run_spec.py \
	  /home/jafourni/mine/gerbil-shell/_vendor/oils/spec/builtin-echo.test.sh \
	  ./jsh

# ─── Cleanup ─────────────────────────────────────────────────────────────────

clean:
	find src -name "*.so" -delete 2>/dev/null || true
	find src -name "*.wpo" -delete 2>/dev/null || true
	rm -f libjsh-ffi.so libjsh-ffi-musl.so jsh-all.so jsh.wpo jsh.boot jsh jsh-musl build-jsh-musl.ss
	rm -f jsh_program.h jsh_petite_boot.h jsh_scheme_boot.h jsh_jsh_boot.h
	# Remove auto-generated jsh .sls files (keep handwritten ones)
	@for f in ast registry macros util environment lexer arithmetic glob \
	          fuzzy history parser functions signals expander redirect \
	          control jobs builtins pipeline executor completion prompt \
	          lineedit fzf script startup main; do \
		rm -f src/jsh/$$f.sls; \
	done
