test:
	cabal test --test-show-details=direct

clean:
	rm -rf dist

run:
	cabal run ray-tracing-one-weekend --ghc-options="-threaded -eventlog -O2" -- +RTS -N -s -RTS

build:
	cabal build --ghc-options="-O2 -threaded -rtsopts -eventlog"

prof:
	cabal run ray-tracing-one-weekend --ghc-options="-O2 -fprof-auto" --enable-profiling -- +RTS -P

heap:
	cabal run ray-tracing-one-weekend --ghc-options="-O2" --enable-profiling -- +RTS -hc
