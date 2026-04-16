#!/bin/bash

tar -xf test/coreutils-9.6.tar.xz
cp -r coreutils-9.6 $(pwd)/coreutils-32bit
cp -r coreutils-9.6 $(pwd)/coreutils-64bit
cp -r coreutils-9.6 $(pwd)/coreutils-arm-32bit/
cp -r coreutils-9.6 $(pwd)/coreutils-arm-thumb/
cp -r coreutils-9.6 $(pwd)/coreutils-arm-64bit/

export CC="arm-linux-gnueabihf-gcc"
export CFLAGS="-mthumb -no-pie"
pushd $(pwd)/coreutils-arm-thumb
./configure --host=arm
make -j 4
popd

export CC="aarch64-linux-gnu-gcc"
export CFLAGS="-no-pie"
pushd $(pwd)/coreutils-arm-64bit
./configure --host=arm64
make -j 4
popd
