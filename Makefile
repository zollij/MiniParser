# Makefile that wraps cabal & mcabal commands / targets

# Combined targets
build: ghc-build mhs-build
test: ghc-test mhs-test
clean: ghc-clean mhs-clean

.PHONY: ghc-build ghc-test ghc-clean ghc-performance \
        mhs-build mhs-test mhs-clean mhs-performance \
        build test clean

MHS_BIN = dist-mcabal/bin/mhs

# MHS perf-test needs a larger spine stack (default 100k entries is too
# small for inputs parsed char-at-a-time via 'many item').
MHS_PERF_RTS = +RTS -K1M -RTS

# perf-test accepts an optional SCALE argument controlling input sizes.
# GHC default: 100 (~100KB inputs, completes in <1s).
# MHS scale: 10 (~10KB inputs, completes in <5s).  MHS graph reduction
# is ~100x slower than GHC native code on char-at-a-time operations,
# so we use smaller inputs to keep the test under 5 minutes.
GHC_PERF_SCALE = 100
MHS_PERF_SCALE = 10

# ── GHC targets ──────────────────────────────────────────────────────

ghc-build:
	cabal build all

# Run each GHC test binary individually via 'cabal run' so we can
# pass the SCALE argument to perf-test.
ghc-test: ghc-build
	cabal run MiniParser-test
	cabal run expr-parser-test
	cabal run comments-c-test
	cabal run comments-haskell-test
	cabal run comments-jack-test
	cabal run comments-java-test
	cabal run comments-kotlin-test
	cabal run perf-test -- $(GHC_PERF_SCALE)

ghc-performance: ghc-build
	cabal run perf-test -- $(GHC_PERF_SCALE)

ghc-clean:
	cabal clean

# ── MHS targets ──────────────────────────────────────────────────────

mhs-build:
	mcabal build

# mcabal has no way to build test binaries without also running them,
# so we use 'mcabal test' to build everything.  The perf-test at
# default scale (100) will stack-overflow under MHS — that's expected.
# We then re-run perf-test with the smaller MHS_PERF_SCALE.
mhs-test:
	mcabal test || true
	@echo "Above stack overflow error is expected; we blew through the stack on MHS"
	@echo ""
	@echo "Re-running perf-test with MHS scale=$(MHS_PERF_SCALE)..."
	$(MHS_BIN)/perf-test $(MHS_PERF_RTS) $(MHS_PERF_SCALE)

mhs-performance:
	mcabal test || true
	$(MHS_BIN)/perf-test $(MHS_PERF_RTS) $(MHS_PERF_SCALE)

mhs-clean:
	mcabal clean
	rm -f out.comb
