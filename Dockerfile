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
COPY restyler /src/restyler

COPY config /src/config
COPY static /src/static

COPY LICENSE /src/

RUN stack install

# Install actual re-formatting tools
RUN stack install stylish-haskell

# Runtime
FROM fpco/stack-run:lts
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

ENV LANG en_US.UTF-8

RUN mkdir -p /app
WORKDIR /app

COPY --from=builder /root/.local/bin/restyled.io /app/restyled.io
COPY --from=builder /src/config /app/config
COPY --from=builder /src/static /app/static

# Copy any built re-formaters
COPY --from=builder /root/.local/bin/stylish-haskell /usr/bin/stylish-haskell

RUN useradd app
USER app

ENTRYPOINT []
CMD ["/app/restyled.io"]
