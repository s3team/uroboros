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

has_failed="false"

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
