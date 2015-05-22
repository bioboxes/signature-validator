FROM ruby:2.2-wheezy
MAINTAINER Michael Barton, mail@michaelbarton.me.uk

RUN gem install cucumber aruba

RUN echo "deb http://debian.bioboxes.org stable main" >> /etc/apt/sources.list
RUN apt-get update && apt-get install \
            --yes \
	    --no-install-recommends \
	    --allow-unauthenticated \
	    validate-biobox-file
