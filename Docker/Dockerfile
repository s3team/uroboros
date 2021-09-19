FROM ubuntu:20.04

ARG DEBIAN_FRONTEND=noninteractive

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
    gfortran

# build spec cpu 2006
ENV SPECIMG=/data/SPEC_CPU2006v1.1.iso
ENV SRCDIR=/data/spec_src
ENV BINDIR=/data/spec_bin
ENV REBUILD_SPEC=

ADD ./tool/spec_cpu_on_ubuntu/script /script
ADD ./tool/spec_cpu_on_ubuntu/patch /patch
ADD ./tool/build_spec.sh /script/build_spec.sh
ADD ./tool/config.cfg /script/config.cfg

CMD /script/build_spec.sh && \
    bash
