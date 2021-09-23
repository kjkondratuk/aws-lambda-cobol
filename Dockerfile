FROM amazonlinux:2

ARG COBC_VERSION="${COBC_VERSION:-3.1.2}"

ENV BIN_NAME="application"
ENV SOURCES="application.cob"

WORKDIR /opt

COPY ${SOURCES} /opt/sources/

RUN yum update -y && yum install -y \
    wget \
    zip \
    tar \
    xz \
    glibc-devel \
    gcc \
    patch \
    g++ \
    make \
    gmp-devel \
    libdb-devel \
    json-c \
    json-c-devel \
    libfastjson \
    json-glib-devel \
    json-glib \
    libxml2-devel \
    # should parameterize this URL
    && wget -O gnucobol.tar.xz "https://downloads.sourceforge.net/project/gnucobol/gnucobol/3.1/gnucobol-3.1.2.tar.xz?ts=gAAAAABhS6WRqAFpU7mDhXOt9IIMMtZuhz_ufmMEMVCcPRd3qKXPPYZi_Yt8E3-k4_yhDZfkYypg7Z6ctaMGmff294XqBo9MeQ%3D%3D&r=https%3A%2F%2Fsourceforge.net%2Fprojects%2Fgnucobol%2Ffiles%2Fgnucobol%2F3.1%2Fgnucobol-3.1.2.tar.xz%2Fdownload" \
    && tar -xvf gnucobol.tar.xz \
    && rm gnucobol.tar.xz \
    # build gnucobol
    && cd gnucobol-${COBC_VERSION} \
    && ./configure \
    && make \
    && make install \
    && rm -rf gnucobol-${COBC_VERSION}

VOLUME [ "/opt/bin" ]

CMD cp -r /usr/local/lib/* bin/ && \
    LD_LIBRARY_PATH=/usr/local/lib cobc -fixed -x -o "/opt/bin/$BIN_NAME" "sources/$SOURCES"