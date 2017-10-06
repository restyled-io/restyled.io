FROM fpco/stack-build:lts as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
ENV PATH /root/.local/bin:$PATH

ENV BRITTANY_VERSION 0.8.0.3
RUN \
  git clone https://github.com/lspitzner/brittany.git && cd brittany && \
  git reset --hard "$BRITTANY_VERSION" && \
  stack setup && \
  stack install

FROM fpco/stack-run:lts
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
COPY --from=builder /root/.local/bin/brittany /usr/bin/brittany
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/settings \
  /root/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/settings
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/platformConstants \
  /root/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/platformConstants
COPY --from=builder \
  /root/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/package.conf.d \
  /root/.stack/programs/x86_64-linux/ghc-8.0.2/lib/ghc-8.0.2/package.conf.d
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
COPY files /
CMD ["brittany", "--help"]
