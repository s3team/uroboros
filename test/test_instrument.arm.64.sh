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

read -r -d '' expected_genecdh_base <<'EOF'
Pass command: genkey|shared
EOF

read -r -d '' expected_rware_base <<'EOF'
Provide path!
Usage: ./rware <path>
EOF

read -r -d '' expected_unrware_base <<'EOF'
Provide info file and unlock key!
Usage: ./unrware <info-file> <unlock-key>
EOF
has_failed="false"
src_dir=$(pwd)/src
rware_dir=$(pwd)/test/rware
QEMU="qemu-aarch64 -L /usr/aarch64-linux-gnu"

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
  echo "run: python3 uroboros.py $binary --arch arm"
  python3 uroboros.py "$binary" --arch arm &> /dev/null
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

check_all_encrypted() {
  local label="$1"
  pushd ${rware_dir}/hello
  if find . -maxdepth 1 -type f ! -name '*.CRYPT' -print -quit | grep -q .; then
    echo "##### expected encrypted output for $label not matching #####"
    has_failed="true"
  fi
  popd
}

check_all_decrypted() {
  local label="$1"
  pushd ${rware_dir}/hello
  if find . -type f -name '*.CRYPT' -print -quit | read; then
    echo "##### expected decrypted output for $label not matching #####"
    has_failed="true"
  fi
  popd
}

check_genecdh_files() {
  local label="$1"
  pushd "$src_dir"
  if [ ! -f public_key.h ]; then
    echo "##### expected resulting file public_key.h for $label not matching #####"
    has_failed="true"
  fi
  if [ ! -f private.key ]; then
    echo "##### expected resulting file private.key for $label not matching #####"
    has_failed="true"
  fi
  rm -f public_key.h private.key
  popd
}

check_base_usage() {
  label="$1"
  expected="$2"
  pushd "$src_dir"
  tmpfile=$(mktemp)
  script -qec "$QEMU $(pwd)/a.out" /dev/null 2>&1 | tr -d '\r' > "$tmpfile"
  actual=$(cat "$tmpfile")
  if [[ "$expected" != "$actual" ]]; then
    echo "##### expected usage output for $label not matching #####"
    echo "~ actual:"
    cat "$tmpfile"
    has_failed="true"
  fi
  rm "$tmpfile"
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
run_test "arm64 test00" point_examples/points.test00.arm.64.ins \
  "$(pwd)/test/test00/test00.arm.64.nopie.dynamic.sym" "$expected_test00"

##########
# test05 #
##########
run_test "arm64 test05" point_examples/points.test05.arm.64.ins \
  "$(pwd)/test/test05/test05.arm.64.nopie.dynamic.sym" "$expected_test05"

##########
# test07 #
##########
run_test "arm64 test07" point_examples/points.test07.arm.64.ins \
  "$(pwd)/test/test07/test07.arm.64.nopie.dynamic.sym" "$expected_test07"

############
# simul_io #
############

# dynamically-linked tests

simul_io_dir=$(pwd)/test/simul_io
in_dir "$simul_io_dir" make clean
in_dir "$simul_io_dir" make arm.64

cp "$simul_io_dir"/capture.c "$src_dir"/capture.c
run_test "arm64 simul_io" \
  ../test/simul_io/points.float_capture.arm.64.ins \
  "$simul_io_dir/simul_io.arm.64.nopie.dynamic.sym" "$expected_simul_io"
rm "$src_dir"/capture.c

# statically-linked tests

simul_io_dir=$(pwd)/test/simul_io
in_dir "$simul_io_dir" make clean
in_dir "$simul_io_dir" make arm.64

cp "$simul_io_dir"/capture.c "$src_dir"/capture.c
run_test "arm64 simul_io (statically-linked)" \
  ../test/simul_io/points.float_capture.arm.64.ins \
  "$simul_io_dir/simul_io.arm.64.nopie.static.sym" "$expected_simul_io"
rm "$src_dir"/capture.c

in_dir "$simul_io_dir" make clean

##########################################
## rware tests (genecdh, rware, unrware) #
##########################################

#############################
## dynamically-linked tests #
#############################

## rware
in_dir "$rware_dir" make -f Makefile.arm.64

run_test "arm64 rware" \
  ${rware_dir}/points.rware.arm.64.ins \
  "${rware_dir}/build/rware_sym" "$expected_rware_64" \
  ${rware_dir}/hello
check_all_encrypted "rware.arm.64"
check_base_usage "rware.arm.64" "$expected_rware_base"

## unrware
TEST_GREP="called" run_test "arm64 unrware" \
  ${rware_dir}/points.unrware.arm.64.ins \
  "${rware_dir}/build/unrware_sym" "$expected_unrware_64" \
  info.enc ${rware_dir}/keys
pushd "$src_dir"; rm -f info.enc; popd
check_all_decrypted "unrware.arm.64"
check_base_usage "unrware.arm.64" "$expected_unrware_base"

in_dir "$rware_dir" make clean

## genecdh
in_dir "$rware_dir" make -f Makefile.arm.64

run_test "arm64 genecdh" \
  ${rware_dir}/points.genecdh.arm.64.ins \
  "${rware_dir}/build/genecdh_sym" "$expected_genecdh_64" \
  genkey
check_genecdh_files "genecdh.arm.64"
check_base_usage "genecdh.arm.64" "$expected_genecdh_base"

in_dir "$rware_dir" make clean

############################
## statically-linked tests #
############################

## rware (static)
in_dir "$rware_dir" make -f Makefile.arm.static.64

echo ">>> arm64 rware (statically-linked)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/rware_sym" --arch arm &> /dev/null
$QEMU $(pwd)/a.out ${rware_dir}/hello &> /dev/null
popd
check_all_encrypted "rware.arm.static.64"
check_base_usage "rware.arm.static.64" "$expected_rware_base"

## unrware (static)
echo ">>> arm64 unrware (statically-linked)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/unrware_sym" --arch arm &> /dev/null
$QEMU $(pwd)/a.out info.enc ${rware_dir}/keys &> /dev/null
rm info.enc
popd
check_all_decrypted "unrware.arm.static.64"
check_base_usage "unrware.arm.static.64" "$expected_unrware_base"

in_dir "$rware_dir" make clean

## genecdh (static)
in_dir "$rware_dir" make -f Makefile.arm.static.64

echo ">>> arm64 genecdh (statically-linked)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/genecdh_sym" --arch arm &> /dev/null
$QEMU $(pwd)/a.out genkey &> /dev/null
popd
check_genecdh_files "genecdh.arm.static.64"
check_base_usage "genecdh.arm.static.64" "$expected_genecdh_base"

in_dir "$rware_dir" make clean

###########
# closing #
###########
if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

echo "testing success."
