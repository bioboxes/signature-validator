FROM haskell:7.10
MAINTAINER Michael Barton, mail@michaelbarton.me.uk

RUN cabal update && \
    cabal install \
    'base       >= 4.8  && < 4.9'\
    'parsec     >= 3.1  && < 3.2'\
    'yaml       >= 0.8  && < 0.9'\
    'bytestring >= 0.10 && < 0.11'\
    'doctest    >= 0.8'

ENV BUILD /opt/signature-validator
RUN mkdir ${BUILD}
ADD ./bioboxes-signature-parser.cabal ${BUILD}/
WORKDIR ${BUILD}
RUN cabal install --only-dependencies --enable-tests --jobs=$(nproc)
