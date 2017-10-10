# Build stage
FROM fpco/stack-build:lts-9.5 as builder
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
COPY backend /src/backend
COPY restyler /src/restyler

COPY config /src/config
COPY static /src/static

COPY LICENSE /src/

RUN stack install

# Runtime
FROM fpco/stack-run:lts
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

ENV LANG en_US.UTF-8

# Configuration that's static for all deployed containers
ENV GIT_AUTHOR_EMAIL commits@restyled.io
ENV GIT_AUTHOR_NAME Restyled.io
ENV GIT_COMMITTER_EMAIL commits@restyled.io
ENV GIT_COMMITTER_NAME Restyled.io
ENV RESTYLER_EXECUTABLE /app/restyler

RUN mkdir -p /app
WORKDIR /app

COPY --from=builder /root/.local/bin/restyled.io /app/restyled.io
COPY --from=builder /root/.local/bin/restyled.io-backend /app/restyled.io-backend
COPY --from=builder /root/.local/bin/restyler /app/restyler
COPY --from=builder /src/config /app/config
COPY --from=builder /src/static /app/static

# Install docker
ENV DOCKER_ARCHIVE docker-17.03.1-ce.tgz
ENV DOCKER_SRC_URL https://get.docker.com/builds/Linux/x86_64/$DOCKER_ARCHIVE
RUN \
  curl -fsSLO "$DOCKER_SRC_URL" && \
  tar --strip-components=1 -xvzf "$DOCKER_ARCHIVE" -C /usr/local/bin

RUN useradd app
USER app

COPY docker/files /
ENTRYPOINT ["/entrypoint"]
CMD ["/app/restyled.io"]
