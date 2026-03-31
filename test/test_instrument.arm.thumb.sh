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
src_dir=$(pwd)/src
QEMU="qemu-arm -L /usr/arm-linux-gnueabihf"

#####################
# helper functions  #
#####################

run_test() {
  local label="$1" point_file="$2" binary="$3" expected="$4"
  shift 4
  echo ">>> $label"
  pushd "$src_dir"
  cp "$point_file" points/
  tmpfile=$(mktemp)
  echo "run: python3 uroboros.py $binary --arch thumb"
  python3 uroboros.py "$binary" --arch thumb &> /dev/null
  echo "run: script -qec $QEMU $(pwd)/a.out $* /dev/null"
  script -qec "$QEMU $(pwd)/a.out $*" /dev/null 2>&1 | tr -d '\r' > "$tmpfile"
  local actual
  if [[ -n "$TEST_GREP" ]]; then
    actual=$(grep "$TEST_GREP" "$tmpfile")
  else
    actual=$(cat "$tmpfile")
  fi
  if [[ "$expected" != "$actual" ]]; then
    echo "##### expected output for $label not matching #####"
    echo "~ actual:"
    cat "$tmpfile"
    has_failed="true"
  fi
  rm "$tmpfile"
  cleanup_points
  popd
}

cleanup_points() {
  find points -maxdepth 1 ! -name '.gitignore' -type f -exec rm -v {} +
}

in_dir() {
  local dir="$1"; shift
  pushd "$dir"
  echo "run: $@"
  "$@"
  popd
}

##########
# test00 #
##########
run_test "arm thumb test00" point_examples/points.test00.arm.thumb.ins \
  "$(pwd)/test/test00/test00.arm.thumb.nopie.dynamic.sym" "$expected_test00"

##########
# test05 #
##########
run_test "arm thumb test05" point_examples/points.test05.arm.thumb.ins \
  "$(pwd)/test/test05/test05.arm.thumb.nopie.dynamic.sym" "$expected_test05"

##########
# test07 #
##########
run_test "arm thumb test07" point_examples/points.test07.arm.thumb.ins \
  "$(pwd)/test/test07/test07.arm.thumb.nopie.dynamic.sym" "$expected_test07"

############
# simul_io #
############
simul_io_dir=$(pwd)/test/simul_io
in_dir "$simul_io_dir" make arm.thumb

cp "$simul_io_dir"/capture.c "$src_dir"/capture.c
run_test "arm thumb simul_io" \
  ../test/simul_io/points.float_capture.arm.thumb.ins \
  "$simul_io_dir/simul_io.arm.thumb.nopie.dynamic.sym" "$expected_simul_io"
rm "$src_dir"/capture.c

in_dir "$simul_io_dir" make clean

###########
# closing #
###########
if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

echo "testing success."
