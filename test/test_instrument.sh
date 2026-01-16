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

read -r -d '' expected_simul_io_32 <<'EOF'
Cycle: 0
analogWrite called. pin=0 val=0.000000
Cycle: 1
analogWrite called. pin=0 val=0.000000
Cycle: 2
analogWrite called. pin=0 val=0.000000
Cycle: 3
analogWrite called. pin=0 val=0.000000
Cycle: 4
analogWrite called. pin=0 val=0.000000
EOF

read -r -d '' expected_simul_io_64 <<'EOF'
Cycle: 0
analogWrite called. pin=0 val=0.000000
Cycle: 1
analogWrite called. pin=1 val=0.003906
Cycle: 2
analogWrite called. pin=2 val=0.007812
Cycle: 3
analogWrite called. pin=3 val=0.011719
Cycle: 4
analogWrite called. pin=0 val=0.015625
EOF

read -r -d '' expected_rware_64 <<'EOF'
called 1 times
called 2 times
called 3 times
called 4 times
called 5 times
called 6 times
called 7 times
EOF

read -r -d '' expected_genecdh_64 <<'EOF'
called 1 times
EOF

has_failed="false"

##########
# test00 #
##########
echo ">>> test00"

pushd $(pwd)/src
cp point_examples/points.test00.32.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test00/test00.32.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test00" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test00.32 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
rm -rf points
mkdir points
popd

pushd $(pwd)/src
cp point_examples/points.test00.64.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test00/test00.64.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test00" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test00.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
rm -rf points
mkdir points
popd

##########
# test01 #
##########
# old instrumentation
#echo ">>> test01"
#
#pushd $(pwd)/src
#cp point_examples/points.test01.32.ins points/
#echo "0x804919d" > instrument_locs.ins
#tmpfile=$(mktemp)
#python3 uroboros.py $(pwd)/../test/test01/test01.32.nopie.dynamic.sym &> /dev/null
#$(pwd)/a.out &> "$tmpfile"
#
#if [[ "$expected_test01" != $(cat "$tmpfile") ]]; then
#  echo "##### expected output for test01.32 not matching #####"
#  echo "~ actual:"
#  cat "$tmpfile"
#  has_failed="true"
#fi
#rm "$tmpfile"
#rm -rf points
#mkdir points
#popd
#
#pushd $(pwd)/src
#cp point_examples/points.test01.64.ins points/
#echo "0x401162" > instrument_locs.ins
#tmpfile=$(mktemp)
#python3 uroboros.py $(pwd)/../test/test01/test01.64.nopie.dynamic.sym &> /dev/null
#$(pwd)/a.out &> "$tmpfile"
#
#if [[ "$expected_test01" != $(cat "$tmpfile") ]]; then
#  echo "##### expected output for test01.64 not matching #####"
#  echo "~ actual:"
#  cat "$tmpfile"
#  has_failed="true"
#fi
#rm "$tmpfile"
#rm -rf points
#mkdir points
#popd

##########
# test05 #
##########
echo ">>> test05"

pushd $(pwd)/src
cp point_examples/points.test05.32.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test05/test05.32.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test05" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test05.32 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
rm -rf points
mkdir points
popd

pushd $(pwd)/src
cp point_examples/points.test05.64.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test05/test05.64.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test05" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test05.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
rm -rf points
mkdir points
popd

##########
# test07 #
##########
echo ">>> test07"

pushd $(pwd)/src
cp point_examples/points.test07.32.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test07/test07.32.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test07" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test07.32 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
rm -rf points
mkdir points
popd

pushd $(pwd)/src
cp point_examples/points.test07.64.ins points/
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test07/test07.64.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test07" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test07.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
rm -rf points
mkdir points
popd

if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

############
# simul_io #
############
#echo ">>> 32-bit simul_io"
#pushd $(pwd)/src
#cp ../test/simul_io/points.float_capture.32.ins points/
#cp ../test/simul_io/capture.c capture.c
#tmpfile=$(mktemp)
#python3 uroboros.py $(pwd)/../test/simul_io/simul_io.32.nopie.dynamic.sym &> /dev/null
#$(pwd)/a.out &> "$tmpfile"
#
#if [[ "$expected_simul_io_32" != $(cat "$tmpfile") ]]; then
#  echo "##### expected output for simul_io.32 not matching #####"
#  echo "~ actual:"
#  cat "$tmpfile"
#  has_failed="true"
#fi
#rm "$tmpfile"
#rm capture.c
#rm -rf points
#mkdir points
#popd
#
#echo ">>> 64-bit simul_io"
#pushd $(pwd)/src
#cp ../test/simul_io/points.float_capture.64.ins points/
#cp ../test/simul_io/capture.c capture.c
#tmpfile=$(mktemp)
#python3 uroboros.py $(pwd)/../test/simul_io/simul_io.64.nopie.dynamic.sym &> /dev/null
#$(pwd)/a.out &> "$tmpfile"
#
#if [[ "$expected_simul_io_64" != $(cat "$tmpfile") ]]; then
#  echo "##### expected output for simul_io.64 not matching #####"
#  echo "~ actual:"
#  cat "$tmpfile"
#  has_failed="true"
#fi
#rm "$tmpfile"
#rm capture.c
#rm -rf points
#mkdir points
#popd

###################
## rware (32-bit then 64-bit) #
###################
#echo ">>> 32-bit rware"
#
#rware_dir=$(pwd)/test/rware
#pushd ${rware_dir}
#make
#popd
#pushd $(pwd)/src
#cp ${rware_dir}/points.rware.32.ins points/
#tmpfile=$(mktemp)
#python3 uroboros.py ${rware_dir}/build/rware &> /dev/null
#$(pwd)/a.out ${rware_dir}/hello &> "$tmpfile"
#
#if [[ "$expected_rware_64" != $(cat "$tmpfile") ]]; then
#  echo "##### expected output for rware.32 not matching #####"
#  echo "~ actual:"
#  cat "$tmpfile"
#  has_failed="true"
#fi
#rm "$tmpfile"
#rm -rf ${rware_dir}/hello
#cp -R ${rware_dir}/hello-bk ${rware_dir}/hello
#popd
#pushd ${rware_dir}
#make clean
#rm -rf points
#mkdir points
#popd
#
#echo ">>> 64-bit rware"
#
#rware_dir=$(pwd)/test/rware
#pushd ${rware_dir}
#make -f Makefile64
#popd
#pushd $(pwd)/src
#cp ${rware_dir}/points.rware.64.ins points/
#tmpfile=$(mktemp)
#python3 uroboros.py ${rware_dir}/build/rware &> /dev/null
#$(pwd)/a.out ${rware_dir}/hello &> "$tmpfile"
#
#if [[ "$expected_rware_64" != $(cat "$tmpfile") ]]; then
#  echo "##### expected output for rware.64 not matching #####"
#  echo "~ actual:"
#  cat "$tmpfile"
#  has_failed="true"
#fi
#rm "$tmpfile"
#rm -rf ${rware_dir}/hello
#cp -R ${rware_dir}/hello-bk ${rware_dir}/hello
#popd
#pushd ${rware_dir}
#make clean
#rm -rf points
#mkdir points
#popd


#####################
## genecdh (32-bit then 64-bit) #
#####################
#echo ">>> 32-bit genecdh"
#
#rware_dir=$(pwd)/test/rware
#pushd ${rware_dir}
#make
#popd
#pushd $(pwd)/src
#cp ${rware_dir}/points.genecdh.32.ins points/
#tmpfile=$(mktemp)
#python3 uroboros.py ${rware_dir}/build/genecdh &> /dev/null
#$(pwd)/a.out genkey &> "$tmpfile"
#
#if [[ "$expected_genecdh_64" != $(cat "$tmpfile") ]]; then
#  echo "##### expected output for genecdh.32 not matching #####"
#  echo "~ actual:"
#  cat "$tmpfile"
#  has_failed="true"
#fi
#rm "$tmpfile"
#rm -rf points
#mkdir points
#popd
#pushd ${rware_dir}
#make clean
#popd
#
#echo ">>> 64-bit genecdh"
#
#rware_dir=$(pwd)/test/rware
#pushd ${rware_dir}
#make -f Makefile64
#popd
#pushd $(pwd)/src
#cp ${rware_dir}/points.genecdh.64.ins points/
#tmpfile=$(mktemp)
#python3 uroboros.py ${rware_dir}/build/genecdh &> /dev/null
#$(pwd)/a.out genkey &> "$tmpfile"
#
#if [[ "$expected_genecdh_64" != $(cat "$tmpfile") ]]; then
#  echo "##### expected output for genecdh.64 not matching #####"
#  echo "~ actual:"
#  cat "$tmpfile"
#  has_failed="true"
#fi
#rm "$tmpfile"
#rm -rf points
#mkdir points
#popd
#pushd ${rware_dir}
#make clean
#popd


###########
# closing #
###########
if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

echo "testing success."
