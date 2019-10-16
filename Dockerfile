FROM fpco/stack-build-small:lts-14.6 AS builder
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    gcc \
    libpq-dev \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
RUN mkdir -p /src
WORKDIR /src

# Dependencies
COPY stack.yaml package.yaml /src/
RUN stack install --dependencies-only

# App
COPY app /src/app
COPY src /src/src
COPY templates /src/templates
COPY config /src/config
ARG REVISION=unknown
RUN echo $REVISION > /src/config/revision
RUN stack install --ghc-options -DDOCKERIZED

# Docker client
ENV DOCKER_ARCHIVE docker-17.03.1-ce.tgz
ENV DOCKER_SRC_URL https://get.docker.com/builds/Linux/x86_64/$DOCKER_ARCHIVE
RUN \
  curl -fsSLO "$DOCKER_SRC_URL" && \
  tar --strip-components=1 -xvzf "$DOCKER_ARCHIVE" -C /usr/local/bin

FROM ubuntu:18.04
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"
ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8
RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    gcc \
    libpq-dev \
    locales \
    netbase && \
  locale-gen en_US.UTF-8 && \
  rm -rf /var/lib/apt/lists/*
RUN mkdir -p /app
WORKDIR /app

# Build stage files
COPY --from=builder /src/config /app/config
COPY --from=builder /root/.local/bin/restyled.io /app/restyled.io
COPY --from=builder /usr/local/bin/docker /usr/local/bin/docker

# Static
COPY static /app/static
RUN mkdir -p /app/static/tmp
