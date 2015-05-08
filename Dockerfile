FROM haskell:7.10
MAINTAINER Michael Barton, mail@michaelbarton.me.uk

ENV BUILD /opt/signature-validator
RUN mkdir ${BUILD}

ADD ./bioboxes-signature-parser.cabal ${BUILD}/
RUN cd ${BUILD} && \
       cabal update && \
       cabal install --only-dependencies --enable-tests --jobs=$(nproc)

WORKDIR ${BUILD}
