ARG UBUNTU_VERSION=20.04
FROM ubuntu:${UBUNTU_VERSION}

ARG DEBIAN_FRONTEND=noninteractive
# set up python virtual env
# for Ubuntu 24, cannot pip install system wide
ENV VIRTUAL_ENV=/opt/venv
ENV PATH="$VIRTUAL_ENV/bin:$PATH"

RUN apt-get update && apt-get upgrade -y

RUN apt-get install -y -q \
    git \
    opam \
    autoconf \
    automake \
    debianutils \
    m4 \
    gcc \
    g++ \
    gcc-multilib \
    g++-multilib \
    build-essential \
    wget \
    python3 \
    python3-pip \
    python3-venv \
    cmake \
    libtool \
    curl \
    pkg-config \
    zip \
    vim \
    autopoint \
    bison \
    gettext \
    gperf \
    texinfo \
    gfortran \
    libgmp-dev

# allow use of git inside the container
RUN git config --global --add safe.directory /usr/src
# create virtual env
RUN python3 -m venv $VIRTUAL_ENV

RUN opam init -a --disable-sandboxing
RUN opam switch create 5.3.0
RUN opam install -y ppx_deriving.6.0.3 parmap.1.2.5 batteries.3.9.0
