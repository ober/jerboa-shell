SCHEME = scheme
JERBOA ?= $(HOME)/mine/jerboa/lib
GHERKIN ?= $(HOME)/mine/gherkin/src
# Compiler needs gherkin's runtime; compiled output needs jerboa's stdlib
LIBDIRS = src:$(JERBOA):$(GHERKIN)

.PHONY: ffi jerboa compile run clean test binary all

all: ffi jerboa compile

ffi:
	@echo "=== Building FFI shim ==="
	gcc -shared -fPIC -o libgsh-ffi.so ffi-shim.c

jerboa:
	@echo "=== Translating .ss → .sls via Gherkin compiler (with Jerboa imports) ==="
	$(SCHEME) -q --libdirs $(LIBDIRS) --compile-imported-libraries < build-jerboa.ss

compile:
	@echo "=== Compiling .sls → .so ==="
	$(SCHEME) -q --libdirs $(LIBDIRS) --compile-imported-libraries < build-all.ss

run:
	LD_LIBRARY_PATH=. $(SCHEME) --libdirs $(LIBDIRS) --program gsh.ss

test:
	@echo "=== Running tests ==="
	LD_LIBRARY_PATH=. $(SCHEME) --libdirs src:$(JERBOA) --script test/test-basic.ss

binary: all
	@echo "=== Building standalone binary ==="
	LD_LIBRARY_PATH=. $(SCHEME) -q --libdirs $(LIBDIRS) < build-binary.ss

clean:
	find src -name "*.so" -delete 2>/dev/null || true
	find src -name "*.wpo" -delete 2>/dev/null || true
	rm -f libgsh-ffi.so
	# Remove auto-generated .sls files (keep handwritten ones)
	@for f in ast registry macros util environment lexer arithmetic glob \
	          fuzzy history parser functions signals expander redirect \
	          control jobs builtins pipeline executor completion prompt \
	          lineedit fzf script startup main; do \
		rm -f src/gsh/$$f.sls; \
	done
