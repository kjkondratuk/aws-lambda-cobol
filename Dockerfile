FROM amazonlinux:2

ARG COBC_VERSION="${COBC_VERSION:-3.1.2}"

ENV BIN_NAME=application
ENV SOURCES=application.cob

WORKDIR /opt

RUN echo "Sources: $SOURCES"

# COPY ${PWD}/*.cob /opt/sources/
VOLUME [ "/opt/sources" ]

# Install necessary dependencies
RUN yum update -y && yum install -y \
    touch \
    git \
    wget \
    zip \
    tar \
    xz \
    glibc-devel \
    gcc \
    patch \
    g++ \
    make \
    cmake \
    gmp-devel \
    libdb-devel \
    libxml2-devel

# build cJSON
RUN  git clone https://github.com/DaveGamble/cJSON.git \
    && cd cJSON \
    && git checkout tags/v1.7.15 \
    && mkdir build \
    && cd build \
    && cmake .. -DENABLE_CJSON_UTILS=On -DENABLE_CJSON_TEST=Off -DCMAKE_INSTALL_PREFIX=/usr \
    && make \
    && make DESTDIR=$pkgdir install

# build gnucobol
# TODO: should parameterize this URL
RUN wget -O gnucobol.tar.xz "https://downloads.sourceforge.net/project/gnucobol/gnucobol/3.1/gnucobol-3.1.2.tar.xz?ts=gAAAAABhS6WRqAFpU7mDhXOt9IIMMtZuhz_ufmMEMVCcPRd3qKXPPYZi_Yt8E3-k4_yhDZfkYypg7Z6ctaMGmff294XqBo9MeQ%3D%3D&r=https%3A%2F%2Fsourceforge.net%2Fprojects%2Fgnucobol%2Ffiles%2Fgnucobol%2F3.1%2Fgnucobol-3.1.2.tar.xz%2Fdownload" \
    && tar -xvf gnucobol.tar.xz \
    && rm gnucobol.tar.xz \
    && cd gnucobol-${COBC_VERSION} \
    && ./configure \
    && make \
    && make install \
    && rm -rf gnucobol-${COBC_VERSION} \
    # Setup LD_LIBRARY_PATH for dynamic libraries we need access to
    && echo '/usr/local/lib' >> /etc/ld.so.conf.d/gnucobol.conf \
    && ldconfig

VOLUME [ "/opt/bin" ]

CMD [ "cobc" ]