build:
	@cabal build --ghc-options="-Wall -O -rtsopts -fno-ignore-asserts"

prof:
	@cabal configure --enable-library-profiling --enable-executable-profiling
	@cabal build --ghc-options="-Wall -rtsopts -prof -fprof-auto"

release:
	@cabal clean
	@cabal configure
	@cabal build --ghc-options="-Wall -O2 -fllvm -fignore-asserts"

test:
	@cabal configure --enable-tests
	@cabal build --ghc-options="-Wall -O -fno-ignore-asserts"
	./dist/build/tests/tests

clean:
	@cabal clean

lint:
	@hlint src tests
