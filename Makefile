bootstrap: .cabal-sandbox

.cabal-sandbox: bioboxes-signature-parser.cabal
	cabal sandbox init
	cabal install --only-dependencies
