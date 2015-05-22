FROM haskell:7.10
MAINTAINER Michael Barton, mail@michaelbarton.me.uk

RUN cabal update

ENV BUILD /opt/signature-validator
RUN mkdir ${BUILD}
ADD ./bioboxes-signature-parser.cabal ${BUILD}/
WORKDIR ${BUILD}
RUN cabal install --only-dependencies --enable-tests --jobs=$(nproc)
