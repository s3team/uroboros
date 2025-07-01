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
called 1 times
before call to print_info
age: 30
called 2 times
called 3 times
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


has_failed="false"

##########
# test00 #
##########
echo ">>> test00"

pushd $(pwd)/src
cp points.test00.32.ins points.ins
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
popd

pushd $(pwd)/src
cp points.test00.64.ins points.ins
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
popd

##########
# test01 #
##########
echo ">>> test01"

pushd $(pwd)/src
cp points.test01.32.ins points.ins
echo "0x804919d" > instrument_locs.ins
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test01/test01.32.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test01" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test01.32 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
popd

pushd $(pwd)/src
cp points.test01.64.ins points.ins
echo "0x401162" > instrument_locs.ins
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/test01/test01.64.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_test01" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for test01.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
popd

##########
# test05 #
##########
echo ">>> test05"

pushd $(pwd)/src
cp points.test05.32.ins points.ins
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
popd

pushd $(pwd)/src
cp points.test05.64.ins points.ins
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
popd

##########
# test07 #
##########
echo ">>> test07"

pushd $(pwd)/src
cp points.test07.32.ins points.ins
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
popd

pushd $(pwd)/src
cp points.test07.64.ins points.ins
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
popd

if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

############
# simul_io #
############
echo ">>> simul_io"

pushd $(pwd)/src
cp ../test/simul_io/points.float_capture.32.ins points.ins
cp ../test/simul_io/capture.c capture.c
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/simul_io/simul_io.32.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_simul_io_32" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for simul_io.32 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
rm capture.c
popd

pushd $(pwd)/src
cp ../test/simul_io/points.float_capture.64.ins points.ins
cp ../test/simul_io/capture.c capture.c
tmpfile=$(mktemp)
python3 uroboros.py $(pwd)/../test/simul_io/simul_io.64.nopie.dynamic.sym &> /dev/null
$(pwd)/a.out &> "$tmpfile"

if [[ "$expected_simul_io_64" != $(cat "$tmpfile") ]]; then
  echo "##### expected output for simul_io.64 not matching #####"
  echo "~ actual:"
  cat "$tmpfile"
  has_failed="true"
fi
rm "$tmpfile"
rm capture.c
popd

if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

echo "testing success."
