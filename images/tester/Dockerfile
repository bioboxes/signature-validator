FROM debian:jessie
MAINTAINER Michael Barton, mail@michaelbarton.me.uk

RUN echo "deb http://debian.bioboxes.org stable main" >> /etc/apt/sources.list
ENV PACKAGES ruby ruby-dev build-essential
RUN apt-get update && apt-get install --yes ${PACKAGES}

RUN apt-get install \
	    --yes \
	    --no-install-recommends \
	    --allow-unauthenticated \
	    validate-biobox-file

RUN gem install cucumber aruba
