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
    python \
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

RUN opam init -a --disable-sandboxing
RUN opam switch create 4.01.0
RUN opam install -y deriving.0.7 ocamlfind.1.5.5 parmap.1.0-rc6 batteries.2.3.1

RUN git clone https://github.com/s3team/uroboros.git /root/uroboros
WORKDIR "/root/uroboros/src"
RUN eval $(opam env) && ./build
