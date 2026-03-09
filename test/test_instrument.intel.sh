#!/bin/bash

read -r -d '' expected_test00 <<'EOF'
passed int: 10
^passed: %d
 10^
10
passed string: hello world
hello world
EOF

read -r -d '' expected_test01 <<'EOF'
10
ABC
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

read -r -d '' expected_rware <<'EOF'
passed int: 24
passed int: 16
passed int: 16
passed int: 16
passed int: 16
passed int: 16
passed int: 16
EOF

read -r -d '' expected_unrware <<'EOF'
called 1 times
called 2 times
called 3 times
EOF

read -r -d '' expected_genecdh <<'EOF'
passed int: 24
EOF

has_failed="false"

##########
# test00 #
##########
echo ">>> 32-bit test00"

pushd $(pwd)/src
cp point_examples/points.test00.intel.32.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test00/test00.intel.32.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test00" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test00.32 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
popd

echo ">>> 64-bit test00"

pushd $(pwd)/src
cp point_examples/points.test00.intel.64.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test00/test00.intel.64.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test00" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test00.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
popd

##########
# test01 #
##########
# test old instrumentation approach
#echo ">>> test01"
#
#pushd $(pwd)/src
#cp point_examples/points.test01.intel.32.ins points/
#echo "0x804919d" > instrument_locs.ins
#tmpfile=$(mktemp)
#python3 uroboros.py $(pwd)/../test/test01/test01.intel.32.nopie.dynamic.sym &> /dev/null
#$(pwd)/a.out &> "$tmpfile"
#
#if [[ "$expected_test01" != $(cat "$tmpfile") ]]; then
#  echo "##### expected output for test01.32 not matching #####"
#  echo "~ actual:"
#  cat "$tmpfile"
#  has_failed="true"
#fi
#rm "$tmpfile"
#find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
#popd
#
#pushd $(pwd)/src
#cp point_examples/points.test01.intel.64.ins points/
#echo "0x401162" > instrument_locs.ins
#tmpfile=$(mktemp)
#python3 uroboros.py $(pwd)/../test/test01/test01.intel.64.nopie.dynamic.sym &> /dev/null
#$(pwd)/a.out &> "$tmpfile"
#
#if [[ "$expected_test01" != $(cat "$tmpfile") ]]; then
#  echo "##### expected output for test01.64 not matching #####"
#  echo "~ actual:"
#  cat "$tmpfile"
#  has_failed="true"
#fi
#rm "$tmpfile"
#find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
#popd

##########
# test05 #
##########
echo ">>> 32-bit test05"

pushd $(pwd)/src
cp point_examples/points.test05.intel.32.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test05/test05.intel.32.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test05" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test05.32 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
popd

echo ">>> 64-bit test05"

pushd $(pwd)/src
cp point_examples/points.test05.intel.64.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test05/test05.intel.64.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test05" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test05.64 not matching #####"
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
echo ">>> 32-bit test07"

pushd $(pwd)/src
cp point_examples/points.test07.intel.32.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test07/test07.intel.32.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test07" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test07.32 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
popd

echo ">>> 64-bit test07"

pushd $(pwd)/src
cp point_examples/points.test07.intel.64.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test07/test07.intel.64.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test07" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test07.64 not matching #####"
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
make clean
make intel
popd

echo ">>> 32-bit simul_io"

pushd $(pwd)/src
cp ../test/simul_io/points.float_capture.intel.32.ins points/
cp ../test/simul_io/capture.c capture.c
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/simul_io/simul_io.intel.32.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_simul_io" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for simul_io.32 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
rm capture.c
popd

pushd $(pwd)/test/simul_io
make clean
make intel
popd

echo ">>> 64-bit simul_io"

pushd $(pwd)/src
cp ../test/simul_io/points.float_capture.intel.64.ins points/
cp ../test/simul_io/capture.c capture.c
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/simul_io/simul_io.intel.64.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_simul_io" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for simul_io.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
rm capture.c
popd

pushd $(pwd)/test/simul_io
make clean
popd

#################
## rware 32-bit #
#################
rware_dir=$(pwd)/test/rware
pushd ${rware_dir}
make clean -f Makefile.intel.32
make -f Makefile.intel.32
popd

echo ">>> 32-bit rware"

pushd $(pwd)/src
cp ${rware_dir}/points.rware.intel.32.ins points/
tmpfile=$(mktemp)
python3 uroboros.py ${rware_dir}/build/rware_sym &> /dev/null
$(pwd)/a.out ${rware_dir}/hello &> "$tmpfile"

if [[ "$expected_rware" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for rware.32 not matching #####"
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

###################
## unrware 32-bit #
###################
echo ">>> 32-bit unrware"

pushd $(pwd)/src
cp ${rware_dir}/points.unrware.intel.32.ins $(pwd)/points/
tmpfile=$(mktemp)
python3 uroboros.py ${rware_dir}/build/unrware_sym &> /dev/null
$(pwd)/a.out info.enc ${rware_dir}/keys &> "$tmpfile"

if [[ "$expected_unrware" != $(cat "$tmpfile" | grep "called") ]]; then
  echo "##### expected output for unrware.32 not matching #####"
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
  echo "##### expected decrypted output for unrware.arm.32 not matching #####"
  has_failed="true"
fi
popd
pushd ${rware_dir}
make clean -f Makefile.intel.32
popd

#################
## rware 64-bit #
#################
rware_dir=$(pwd)/test/rware
pushd ${rware_dir}
make -f Makefile.intel.64
popd

echo ">>> 64-bit rware"

pushd $(pwd)/src
cp ${rware_dir}/points.rware.intel.64.ins $(pwd)/points/
tmpfile=$(mktemp)
python3 uroboros.py ${rware_dir}/build/rware_sym &> /dev/null
$(pwd)/a.out ${rware_dir}/hello &> "$tmpfile"

if [[ "$expected_rware" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for rware.64 not matching #####"
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

###################
## unrware 64-bit #
###################
echo ">>> 64-bit unrware"

pushd $(pwd)/src
cp ${rware_dir}/points.unrware.intel.64.ins $(pwd)/points/
tmpfile=$(mktemp)
python3 uroboros.py ${rware_dir}/build/unrware_sym &> /dev/null
$(pwd)/a.out info.enc ${rware_dir}/keys &> "$tmpfile"

if [[ "$expected_unrware" != $(cat "$tmpfile" | grep "called") ]]; then
  echo "##### expected output for unrware.64 not matching #####"
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
make clean -f Makefile.intel.64
popd

############
## genecdh #
############
rware_dir=$(pwd)/test/rware
pushd ${rware_dir}
make -f Makefile.intel.32
popd

echo ">>> 32-bit genecdh"

pushd $(pwd)/src
cp ${rware_dir}/points.genecdh.intel.32.ins points/
tmpfile=$(mktemp)
python3 uroboros.py ${rware_dir}/build/genecdh_sym &> /dev/null
$(pwd)/a.out genkey &> "$tmpfile"

if [[ "$expected_genecdh" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for genecdh.32 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +

if [ ! -f public_key.h ]; then
  echo "##### expected resulting file public_key.h for genecdh.intel.32 not matching #####"
  has_failed="true"
fi
if [ ! -f private.key ]; then
  echo "##### expected resulting file private.key for genecdh.intel.32 not matching #####"
  has_failed="true"
fi
rm public_key.h
rm private.key
popd

pushd ${rware_dir}
make clean -f Makefile.intel.32
make -f Makefile.intel.64
popd

echo ">>> 64-bit genecdh"

pushd $(pwd)/src
cp ${rware_dir}/points.genecdh.intel.64.ins points/
tmpfile=$(mktemp)
python3 uroboros.py ${rware_dir}/build/genecdh_sym &> /dev/null
$(pwd)/a.out genkey &> "$tmpfile"

if [[ "$expected_genecdh" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for genecdh.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +

if [ ! -f public_key.h ]; then
  echo "##### expected resulting file public_key.h for genecdh.intel.64 not matching #####"
  has_failed="true"
fi
if [ ! -f private.key ]; then
  echo "##### expected resulting file private.key for genecdh.intel.64 not matching #####"
  has_failed="true"
fi
rm public_key.h
rm private.key
popd

pushd ${rware_dir}
make clean -f Makefile.intel.64
popd

###########
# closing #
###########
if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

echo "testing success."
