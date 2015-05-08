builder = builder
docker = docker run \
	        --rm \
		--volume "$(shell pwd)/src:/opt/signature-validator/src:ro" \
		--volume "$(shell pwd)/dist:/opt/signature-validator/dist:rw"

exec = dist/build/bioboxes-signature-parser/bioboxes-signature-parser

ssh:
	$(docker) --interactive --tty $(image) /bin/bash

test:
	$(docker) $(image) doctest $(shell find src -type f)

build: $(exec)

$(exec): $(shell find src -type f) bioboxes-signature-parser.cabal
	$(docker) $(image) cabal build

bootstrap: .image-builder

.image-%: images/%/Dockerfile bioboxes-signature-parser.cabal
	cp bioboxes-signature-parser.cabal $(dir $<)
	docker build --tag $* $(dir $<)
	touch $@
