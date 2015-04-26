exec = dist/build/bioboxes-signature-parser/bioboxes-signature-parser

build: $(exec)

$(exec): $(shell find src -type f) bioboxes-signature-parser.cabal
	cabal build

bootstrap: .cabal-sandbox

.cabal-sandbox: bioboxes-signature-parser.cabal
	cabal sandbox init
	cabal install --only-dependencies
