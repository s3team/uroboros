wget ftp.gnu.org/gnu/coreutils/coreutils-9.6.tar.xz
tar -xf coreutils-9.6.tar.xz
cp -r coreutils-9.6 $(pwd)/coreutils-32bit
cp -r coreutils-9.6 $(pwd)/coreutils-64bit

export CFLAGS="-m32 -no-pie"
export CXXFLAGS="-m32 -no-pie"

pushd $(pwd)/coreutils-32bit
./configure
make -j 4
popd

export CFLAGS="-m64 -no-pie"
export CXXFLAGS="-m64 -no-pie"
pushd $(pwd)/coreutils-64bit
./configure
make -j 4
popd

