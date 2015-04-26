exec = dist/build/bioboxes-signature-parser/bioboxes-signature-parser

test:
	.cabal-sandbox/bin/doctest $(shell find src -type f)

build: $(exec)

$(exec): $(shell find src -type f) bioboxes-signature-parser.cabal
	cabal build

bootstrap: .cabal-sandbox

.cabal-sandbox: bioboxes-signature-parser.cabal
	cabal sandbox init
	cabal install --only-dependencies --enable-tests
