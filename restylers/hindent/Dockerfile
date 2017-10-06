FROM fpco/stack-build:lts as builder
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
ENV PATH /root/.local/bin:$PATH
RUN stack --resolver lts setup
RUN stack --resolver lts install hindent

FROM fpco/stack-run:lts
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
ENV LANG en_US.UTF-8
COPY --from=builder /root/.local/bin/hindent /usr/bin/hindent
RUN mkdir -p /code
WORKDIR /code
ENTRYPOINT []
COPY files /
CMD ["hindent", "--help"]
