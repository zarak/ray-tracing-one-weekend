test:
	cabal test --test-show-details=direct

clean:
	rm -rf dist

run:
	cabal run

build:
	cabal build --ghc-options="-O2"

prof:
	cabal run ray-tracing-one-weekend --ghc-options="-O2" --enable-profiling -- +RTS -P

heap:
	cabal run ray-tracing-one-weekend --ghc-options="-O2" --enable-profiling -- +RTS -hc
