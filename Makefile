builder = builder
tester  = tester

docker = docker run \
	        --tty \
	        --rm \
		--env BINARY=/opt/signature-validator/$(exec) \
		--volume "$(shell pwd)/src:/opt/signature-validator/src:ro" \
		--volume "$(shell pwd)/dist:/opt/signature-validator/dist:rw" \
		--volume "$(shell pwd)/features:/features:ro"

exec = dist/build/bioboxes-signature-parser/bioboxes-signature-parser


try: $(exec)
	$(docker) --interactive $(builder) \
		$(exec) --signature="$(SIG)" --schema=input

ssh: $(exec)
	$(docker) --interactive --tty $(tester) /bin/bash

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

.PHONY: test feature bootstrap build
