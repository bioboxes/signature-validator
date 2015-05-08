builder = builder
tester  = tester

docker = docker run \
	        --rm \
		--volume "$(shell pwd)/src:/opt/signature-validator/src:ro" \
		--volume "$(shell pwd)/dist:/opt/signature-validator/dist:rw" \
		--volume "$(shell pwd)/features:/features:ro"

exec = dist/build/bioboxes-signature-parser/bioboxes-signature-parser

ssh:
	$(docker) --interactive --tty $(builder) /bin/bash

feature: $(exec)
	$(docker) $(tester) cucumber /features

test:
	$(docker) $(builder) doctest $(shell find src -type f)

build: $(exec)

$(exec): $(shell find src -type f) bioboxes-signature-parser.cabal
	$(docker) $(builder) cabal build

bootstrap: .image-builder .image-tester

.image-%: images/%/Dockerfile bioboxes-signature-parser.cabal
	cp bioboxes-signature-parser.cabal $(dir $<)
	docker build --tag $* $(dir $<)
	touch $@
