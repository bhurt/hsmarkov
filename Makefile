
all: .cabal-sandbox dist
	cabal build -j

.PHONY: cabal-init
cabal-init: .cabal-sandbox dist

dist:
	cabal configure

.cabal-sandbox: ChatBoxServer.cabal
	cabal sandbox init
	cabal install -j --enable-documentation --haddock-html --haddock-hoogle --only-dependencies

.PHONY: cabal-clean
cabal-clean: clean
	rm -rf .cabal-sandbox cabal.sandbox.config Setup.hs

.PHONY: clean
clean:
	rm -rf dist

