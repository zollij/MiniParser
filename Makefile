# Makefile that wraps cabal commands / targets.
#
# v0.5.0.0 and later are GHC-only. The MHS (MicroHs) targets that existed
# in the v0.4.x line have been removed; pin to v0.4.2.0 if you need MHS.

.PHONY: build test clean performance \
        ghc-build ghc-test ghc-clean ghc-performance

# perf-test accepts an optional SCALE argument controlling input sizes.
# Default 100 produces ~100KB inputs, completes in <1s.
GHC_PERF_SCALE = 100

build: ghc-build
test: ghc-test
clean: ghc-clean
performance: ghc-performance

# ── GHC targets ──────────────────────────────────────────────────────

ghc-build:
	cabal build all

# Run each test binary individually via 'cabal run' so we can pass the
# SCALE argument to perf-test. `@echo` between runs adds a blank line
# after each suite's "All N tests passed" line, before the next
# "cabal run X" output.
ghc-test: ghc-build
	cabal run MiniParser-test
	@echo
	cabal run expr-parser-test
	@echo
	cabal run comments-c-test
	@echo
	cabal run comments-haskell-test
	@echo
	cabal run comments-jack-test
	@echo
	cabal run comments-java-test
	@echo
	cabal run comments-kotlin-test
	@echo
	cabal run float-test
	@echo
	cabal run perf-test -- $(GHC_PERF_SCALE)

ghc-performance: ghc-build
	cabal run perf-test -- $(GHC_PERF_SCALE)

ghc-clean:
	cabal clean
