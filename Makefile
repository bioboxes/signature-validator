image = haskell-builder
docker = docker run \
		--volume "$(shell pwd)/src:/opt/signature-validator/src:ro" \
		--volume "$(shell pwd)/dist:/opt/signature-validator/dist:rw"

exec = dist/build/bioboxes-signature-parser/bioboxes-signature-parser

test:
	$(docker) $(image) doctest $(shell find src -type f)

build: $(exec)

$(exec): $(shell find src -type f) bioboxes-signature-parser.cabal
	$(docker) $(image) cabal build

bootstrap: .image

.image: bioboxes-signature-parser.cabal Dockerfile
	docker build --tag $(image) .
	touch $@
