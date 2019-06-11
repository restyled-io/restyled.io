# Build stage
FROM fpco/stack-build:lts-13.3 as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

ENV LANG en_US.UTF-8
ENV PATH /root/.local/bin:$PATH

RUN mkdir -p /src
WORKDIR /src

COPY stack.yaml /src/
RUN stack setup

COPY package.yaml /src/
RUN stack install --dependencies-only

COPY app /src/app
COPY src /src/src
COPY templates /src/templates

COPY config /src/config
COPY static /src/static
COPY .git/refs/heads/master /src/config/revision

COPY LICENSE /src/

RUN stack install --ghc-options -DDOCKERIZED

# Runtime
FROM fpco/stack-run:lts
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

ENV LANG en_US.UTF-8

RUN mkdir -p /app
WORKDIR /app

COPY --from=builder /root/.local/bin/restyled.io /app/restyled.io
COPY --from=builder /src/config /app/config
COPY --from=builder /src/static /app/static

# Install docker
ENV DOCKER_ARCHIVE docker-17.03.1-ce.tgz
ENV DOCKER_SRC_URL https://get.docker.com/builds/Linux/x86_64/$DOCKER_ARCHIVE
RUN \
  curl -fsSLO "$DOCKER_SRC_URL" && \
  tar --strip-components=1 -xvzf "$DOCKER_ARCHIVE" -C /usr/local/bin

RUN useradd app

# Make /app/config owned by app. The webapp creates client_session_key.aes. We
# should move to ENV-based secret; it's better for this, and avoids everyone
# getting logged-out on deploys.
RUN chown -R app /app/config

# Make static/tmp and change its ownership. Static assets can lazily generated
# so app needs to be able to write here.
RUN mkdir -p /app/static/tmp
RUN chown -R app /app/static/tmp

USER app

COPY docker/files /
ENTRYPOINT ["/entrypoint"]
CMD ["/app/restyled.io", "--help"]
