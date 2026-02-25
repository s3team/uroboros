#!/bin/bash

read -r -d '' expected_test00 <<'EOF'
passed int: 10
^passed: %d
 10^
10
passed string: hello world
hello world
EOF

read -r -d '' expected_test05 <<'EOF'
before call to print_info
called 1 times
name: Jinquan Zhang
age: 26
gender: m
called 2 times
EOF

read -r -d '' expected_test07 <<'EOF'
int arg: 10
int arg: 3628800
char* arg: %d

3628800
EOF

read -r -d '' expected_simul_io <<'EOF'
Cycle: 0
analogWrite called.
Cycle: 1
analogWrite called.
Cycle: 2
analogWrite called.
Cycle: 3
analogWrite called.
Cycle: 4
analogWrite called.
EOF

read -r -d '' expected_rware_64 <<'EOF'
passed int: 24
passed int: 16
passed int: 16
passed int: 16
passed int: 16
passed int: 16
passed int: 16
EOF

read -r -d '' expected_unrware_64 <<'EOF'
called 1 times
called 2 times
called 3 times
EOF

read -r -d '' expected_genecdh_64 <<'EOF'
passed int: 24
EOF
has_failed="false"

##########
# test00 #
##########
echo ">>> arm64 test00"

pushd $(pwd)/src
cp point_examples/points.test00.arm.64.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test00/test00.arm.64.nopie.dynamic.sym --arch arm &> /dev/null
qemu-aarch64 -L /usr/aarch64-linux-gnu  $(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test00" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test00.arm.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
popd

##########
# test05 #
##########
echo ">>> arm64 test05"

pushd $(pwd)/src
cp point_examples/points.test05.arm.64.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test05/test05.arm.64.nopie.dynamic.sym --arch arm &> /dev/null
qemu-aarch64 -L /usr/aarch64-linux-gnu $(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test05" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test05.arm.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
popd

##########
# test07 #
##########
echo ">>> arm64 test07"

pushd $(pwd)/src
cp point_examples/points.test07.arm.64.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test07/test07.arm.64.nopie.dynamic.sym --arch arm &> /dev/null
qemu-aarch64 -L /usr/aarch64-linux-gnu $(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test07" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test07.arm.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
popd

############
# simul_io #
############
pushd $(pwd)/test/simul_io
# compile simul_io binaries
make arm.64
popd

echo ">>> arm64 simul_io"

pushd $(pwd)/src
cp ../test/simul_io/points.float_capture.arm.64.ins points/
cp ../test/simul_io/capture.c capture.c
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/simul_io/simul_io.arm.64.nopie.dynamic.sym --arch arm &> /dev/null
qemu-aarch64 -L /usr/aarch64-linux-gnu $(pwd)/a.out &> "$tmpfile"

if [[ "$expected_simul_io" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for simul_io.arm.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
rm capture.c
popd

pushd $(pwd)/test/simul_io
# delete simul_io binaries
make clean
popd

##########
## rware #
##########
rware_dir=$(pwd)/test/rware
pushd ${rware_dir}
make -f Makefile.arm.64
popd

echo ">>> arm64 rware"

pushd $(pwd)/src
cp ${rware_dir}/points.rware.arm.64.ins $(pwd)/points/
tmpfile=$(mktemp)
python3 uroboros.py ${rware_dir}/build/rware_sym --arch arm &> /dev/null
qemu-aarch64 -L /usr/aarch64-linux-gnu $(pwd)/a.out ${rware_dir}/hello &> "$tmpfile"

if [[ "$expected_rware_64" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for rware.arm.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
popd

pushd ${rware_dir}/hello
if find . -maxdepth 1 -type f ! -name '*.CRYPT' -print -quit | grep -q .; then
  # Not all files end with .CRYPT, so error
  echo "##### expected encrypted output for rware.arm.64 not matching #####"
  has_failed="true"
fi
popd
#pushd ${rware_dir}
#make clean -f Makefile.arm.64
#popd

############
## unrware #
############
echo ">>> arm64 unrware"

pushd $(pwd)/src
cp ${rware_dir}/points.unrware.arm.64.ins $(pwd)/points/
tmpfile=$(mktemp)
python3 uroboros.py ${rware_dir}/build/unrware_sym --arch arm &> /dev/null
qemu-aarch64 -L /usr/aarch64-linux-gnu $(pwd)/a.out info.enc ${rware_dir}/keys &> "$tmpfile"

if [[ "$expected_unrware_64" != $(cat "$tmpfile" | grep "called") ]]; then
  echo "##### expected output for unrware.arm.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
popd

pushd ${rware_dir}/hello
if find . -type f -name '*.CRYPT' -print -quit | read; then
  # At least one file ends with .CRYPT, so error
  echo "##### expected decrypted output for unrware.arm.64 not matching #####"
  has_failed="true"
fi
popd
pushd ${rware_dir}
make clean -f Makefile.arm.64
popd

############
## genecdh #
############
rware_dir=$(pwd)/test/rware
pushd ${rware_dir}
make -f Makefile.arm.64
popd

echo ">>> arm64 genecdh"

pushd $(pwd)/src
cp ${rware_dir}/points.genecdh.arm.64.ins points/
tmpfile=$(mktemp)
python3 uroboros.py ${rware_dir}/build/genecdh_sym --arch arm &> /dev/null
qemu-aarch64 -L /usr/aarch64-linux-gnu $(pwd)/a.out genkey &> "$tmpfile"

if [[ "$expected_genecdh_64" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for genecdh.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +

if [ ! -f public_key.h ]; then
  echo "##### expected resulting file public_key.h for genecdh.arm.64 not matching #####"
  has_failed="true"
fi
if [ ! -f private.key ]; then
  echo "##### expected resulting file private.key for genecdh.arm.64 not matching #####"
  has_failed="true"
fi
rm public_key.h
rm private.key
popd

pushd ${rware_dir}
make clean -f Makefile.arm.64
popd

###########
# closing #
###########
if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

echo "testing success."
