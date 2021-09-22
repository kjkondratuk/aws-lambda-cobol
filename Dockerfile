FROM amazonlinux:2

ARG ENV_DIR="${ENV_DIR:-home}"
ARG COBC_VERSION="${COBC_VERSION:-3.1.2}"

WORKDIR /opt

RUN yum update -y && yum install -y \
    which \
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
    db4-devel \
    ncurses \
    cjson-devel \
    libxml2-devel \
    && wget -O gnucobol.tar.xz "https://downloads.sourceforge.net/project/gnucobol/gnucobol/3.1/gnucobol-3.1.2.tar.xz?ts=gAAAAABhS6WRqAFpU7mDhXOt9IIMMtZuhz_ufmMEMVCcPRd3qKXPPYZi_Yt8E3-k4_yhDZfkYypg7Z6ctaMGmff294XqBo9MeQ%3D%3D&r=https%3A%2F%2Fsourceforge.net%2Fprojects%2Fgnucobol%2Ffiles%2Fgnucobol%2F3.1%2Fgnucobol-3.1.2.tar.xz%2Fdownload" \
    && tar -xvf gnucobol.tar.xz \
    && rm gnucobol.tar.xz \
    && pwd \
    && ls -al gnucobol-3.1.2 \
    && cd gnucobol-3.1.2 \
    #     \
    && ./configure \
    && make \
    && make install

VOLUME [ "/install" ]

ENTRYPOINT [ "cp", "-rL", "/usr/bin/", "/lib64/libidn.so.11", "/install/" ]