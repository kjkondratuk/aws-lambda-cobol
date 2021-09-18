FROM alpine:3.14

ARG COBC_VERSION="${COBC_VERSION}"

COPY ./gnucobol-${COBC_VERSION}/ /opt/gnucobol-${COBC_VERSION}/

WORKDIR /opt/gnucobol-${COBC_VERSION}

RUN apk add g++ make gmp-dev db-dev ncurses cjson-dev libxml2-dev && ./configure && make && make install

WORKDIR /root

ENTRYPOINT [ "/bin/sh" ]