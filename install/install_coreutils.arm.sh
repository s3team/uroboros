#!/bin/bash

tar -xf test/coreutils-9.6.tar.xz
cp -r coreutils-9.6 $(pwd)/coreutils-arm-32bit/
cp -r coreutils-9.6 $(pwd)/coreutils-arm-thumb/
cp -r coreutils-9.6 $(pwd)/coreutils-arm-64bit/

export CC="arm-linux-gnueabihf-gcc"
export CFLAGS="-mthumb -no-pie"
pushd $(pwd)/coreutils-arm-thumb
./configure --host=arm
make -j 4

# Uroboros currently only handles stripped binaries
find src -maxdepth 1 -type f -executable -exec arm-linux-gnueabihf-strip {} + 2>/dev/null || true
popd

export CC="aarch64-linux-gnu-gcc"
export CFLAGS="-no-pie"
pushd $(pwd)/coreutils-arm-64bit
./configure --host=arm64
make -j 4
popd
