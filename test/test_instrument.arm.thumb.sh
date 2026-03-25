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

has_failed="false"

##########
# test00 #
##########
echo ">>> arm thumb test00"

pushd $(pwd)/src
cp point_examples/points.test00.arm.thumb.ins points/
tmpfile=$(mktemp)
# tmp fix for arm file ordering reading issue
python3 uroboros.py $(pwd)/../test/test00/test00.arm.thumb.nopie.dynamic.sym --arch thumb &> /dev/null
qemu-arm -L /usr/arm-linux-gnueabihf $(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test00" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test00.thumb not matching #####"
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
echo ">>> arm thumb test05"

pushd $(pwd)/src
cp point_examples/points.test05.arm.thumb.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test05/test05.arm.thumb.nopie.dynamic.sym --arch thumb &> /dev/null
qemu-arm -L /usr/arm-linux-gnueabihf $(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test05" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test05.thumb not matching #####"
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
echo ">>> arm thumb test07"

pushd $(pwd)/src
cp point_examples/points.test07.arm.thumb.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test07/test07.arm.thumb.nopie.dynamic.sym --arch thumb &> /dev/null
qemu-arm -L /usr/arm-linux-gnueabihf $(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test07" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test07.thumb not matching #####"
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
make arm.thumb
popd

echo ">>> arm thumb simul_io"

pushd $(pwd)/src
cp ../test/simul_io/points.float_capture.arm.thumb.ins points/
cp ../test/simul_io/capture.c capture.c
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/simul_io/simul_io.arm.thumb.nopie.dynamic.sym --arch thumb &> /dev/null
qemu-arm -L /usr/arm-linux-gnueabihf $(pwd)/a.out &> "$tmpfile"

if [[ "$expected_simul_io" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for simul_io.thumb not matching #####"
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

###########
# closing #
###########
if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

echo "testing success."
