image = haskell-builder

exec = dist/build/bioboxes-signature-parser/bioboxes-signature-parser

test:
	.cabal-sandbox/bin/doctest $(shell find src -type f)

build: $(exec)

$(exec): $(shell find src -type f) bioboxes-signature-parser.cabal
	cabal build

bootstrap: .image

.image: bioboxes-signature-parser.cabal Dockerfile
	docker build --tag $(image) .
	touch $@
