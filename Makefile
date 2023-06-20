test: build
	cabal test --test-show-details=direct

clean:
	rm -rf dist

run:
	cabal run

build:
	cabal build
