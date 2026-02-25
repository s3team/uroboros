#!/bin/sh
sudo apt-get upgrade -y
sudo apt-get update
sudo apt-get install -y -q \
    git \
    opam \
    autoconf \
    automake \
    debianutils \
    m4 \
    gcc \
    g++ \
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
    libssl-dev \
    libselinux1-dev \
    qemu-user \
    gdb-multiarch \
    gcc-arm-linux-gnueabihf \
    g++-arm-linux-gnueabihf \
    gcc-aarch64-linux-gnu \
    g++-aarch64-linux-gnu \
    binutils-arm-linux-gnueabihf \
    libgmp-dev
