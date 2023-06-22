test: build
	cabal test --test-show-details=direct

clean:
	rm -rf dist

run:
	cabal run

build:
	cabal build

prof:
	cabal run ray-tracing-one-weekend --enable-profiling -- +RTS -P
