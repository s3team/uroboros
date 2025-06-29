#!/bin/sh
sudo apt-get upgrade -y
sudo apt-get update
sudo apt-get install -y -q \
     git \
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
     libgmp-dev \
     libssl-dev \
     libselinux-dev \
     libselinux1 \
     gcc-arm-none-eabi \
     gcc-arm-linux-gnueabihf \
     gcc-aarch64-linux-gnu \
     gcc-multilib \
     libc6-dev-i386
