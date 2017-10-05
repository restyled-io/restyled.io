FROM fpco/stack-build:lts-9.5 as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
ENV PATH /root/.local/bin:$PATH
RUN stack --resolver lts-9.5 setup
RUN stack --resolver lts-9.5 install stylish-haskell

FROM fpco/stack-run:lts
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
COPY --from=builder /root/.local/bin/stylish-haskell /usr/bin/stylish-haskell
COPY --from=builder \
  /root/.stack/snapshots/x86_64-linux/lts-9.5/8.0.2/share/x86_64-linux-ghc-8.0.2/stylish-haskell-0.8.1.0/data/stylish-haskell.yaml \
  /root/.stack/snapshots/x86_64-linux/lts-9.5/8.0.2/share/x86_64-linux-ghc-8.0.2/stylish-haskell-0.8.1.0/data/stylish-haskell.yaml
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
CMD ["stylish-haskell", "--help"]
