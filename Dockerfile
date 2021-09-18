FROM alpine:3.14

ARG ENV_DIR="${ENV_DIR:-home}"
ARG COBC_VERSION="${COBC_VERSION:-3.1.2}"

COPY ./home/gnucobol-${COBC_VERSION}/ /opt/gnucobol-${COBC_VERSION}/

WORKDIR /opt/gnucobol-${COBC_VERSION}

RUN apk add --update g++ make gmp-dev db-dev ncurses cjson-dev libxml2-dev npm \
    && ./configure \
    && make \
    && make install \
    && npm install -g cobolget

WORKDIR /root

VOLUME [ "/root" ]

ENTRYPOINT [ "/bin/sh" ]