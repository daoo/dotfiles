build:
	@cabal build --ghc-options="-H64m -rtsopts"

prof:
	@cabal build --ghc-options="-rtsopts -prof -fprof-auto -H64m"

release:
	@cabal build --ghc-options="-fllvm -H64m -O2"

init:
	@cabal sandbox init
	@cabal install --only-dependencies
	@cabal configure

test:
	@cabal test

clean:
	@cabal clean --save-configure

tags:
	@cabal repl :ctags

lint:
	@hlint src
