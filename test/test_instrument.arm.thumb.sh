#!/bin/bash

read -r -d '' expected_test00 <<'EOF'
filename: a.out
passed int: 10
^passed: %d
 10^
10
passed string: hello world
hello world
EOF

read -r -d '' expected_test05 <<'EOF'
filename: a.out
before call to print_info
called 1 times
name: Jinquan Zhang
age: 26
gender: m
called 2 times
EOF

read -r -d '' expected_test07 <<'EOF'
filename: a.out
int arg: 10
int arg: 3628800
char* arg: %d

3628800
EOF

read -r -d '' expected_simul_io <<'EOF'
filename: a.out
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

read -r -d '' expected_rware_thumb <<'EOF'
filename: a.out
passed int: 24
passed int: 16
passed int: 16
passed int: 16
passed int: 16
passed int: 16
passed int: 16
EOF

read -r -d '' expected_unrware_thumb <<'EOF'
filename: a.out
called 1 times
called 2 times
called 3 times
EOF

read -r -d '' expected_genecdh_thumb <<'EOF'
filename: a.out
passed int: 24
EOF

read -r -d '' expected_genecdh_base <<'EOF'
filename: a.out
Pass command: genkey|shared
EOF

read -r -d '' expected_rware_base <<'EOF'
filename: a.out
Provide path!
Usage: ./rware <path>
EOF

read -r -d '' expected_unrware_base <<'EOF'
filename: a.out
Provide info file and unlock key!
Usage: ./unrware <info-file> <unlock-key>
EOF

read -r -d '' expected_genecdh_base_static <<'EOF'
Pass command: genkey|shared
EOF

read -r -d '' expected_rware_base_static <<'EOF'
Provide path!
Usage: ./rware <path>
EOF

read -r -d '' expected_unrware_base_static <<'EOF'
Provide info file and unlock key!
Usage: ./unrware <info-file> <unlock-key>
EOF

has_failed="false"
src_dir=$(pwd)/src
rware_dir=$(pwd)/test/rware
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

# dynamically-linked tests

simul_io_dir=$(pwd)/test/simul_io
in_dir "$simul_io_dir" make clean
in_dir "$simul_io_dir" make arm.thumb

cp "$simul_io_dir"/capture.c "$src_dir"/capture.c
run_test "arm thumb simul_io" \
  ../test/simul_io/points.float_capture.arm.thumb.ins \
  "$simul_io_dir/simul_io.arm.thumb.nopie.dynamic.sym" "$expected_simul_io"
rm "$src_dir"/capture.c

in_dir "$simul_io_dir" make clean

# statically-linked tests

simul_io_dir=$(pwd)/test/simul_io
in_dir "$simul_io_dir" make clean
in_dir "$simul_io_dir" make arm.thumb

cp "$simul_io_dir"/capture.c "$src_dir"/capture.c
run_test "arm thumb simul_io (statically-linked)" \
  ../test/simul_io/points.float_capture.arm.thumb.ins \
  "$simul_io_dir/simul_io.arm.thumb.nopie.static.sym" "$expected_simul_io"
rm "$src_dir"/capture.c

in_dir "$simul_io_dir" make clean

##########################################
## rware tests (genecdh, rware, unrware) #
##########################################

#############################
## dynamically-linked tests #
#############################

## rware
in_dir "$rware_dir" make -f Makefile.arm.thumb

run_test "arm thumb rware" \
  ${rware_dir}/points.rware.arm.thumb.ins \
  "${rware_dir}/build/rware_sym" "$expected_rware_thumb" \
  ${rware_dir}/hello
check_all_encrypted "rware.arm.thumb"
check_base_usage "rware.arm.thumb" "$expected_rware_base"

## unrware
TEST_GREP="called\|filename" run_test "arm thumb unrware" \
  ${rware_dir}/points.unrware.arm.thumb.ins \
  "${rware_dir}/build/unrware_sym" "$expected_unrware_thumb" \
  info.enc ${rware_dir}/keys
pushd "$src_dir"; rm -f info.enc; popd
check_all_decrypted "unrware.arm.thumb"
check_base_usage "unrware.arm.thumb" "$expected_unrware_base"

in_dir "$rware_dir" make clean

## genecdh
in_dir "$rware_dir" make -f Makefile.arm.thumb

run_test "arm thumb genecdh" \
  ${rware_dir}/points.genecdh.arm.thumb.ins \
  "${rware_dir}/build/genecdh_sym" "$expected_genecdh_thumb" \
  genkey
check_genecdh_files "genecdh.arm.thumb"
check_base_usage "genecdh.arm.thumb" "$expected_genecdh_base"

in_dir "$rware_dir" make clean

############################
## statically-linked tests #
############################

## rware (static)
in_dir "$rware_dir" make -f Makefile.arm.static.thumb

echo ">>> arm thumb rware (statically-linked)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/rware_sym" --arch thumb &> /dev/null
$QEMU $(pwd)/a.out ${rware_dir}/hello &> /dev/null
popd
check_all_encrypted "rware.arm.static.thumb"
check_base_usage "rware.arm.static.thumb" "$expected_rware_base_static"

## unrware (static)
echo ">>> arm thumb unrware (statically-linked)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/unrware_sym" --arch thumb &> /dev/null
$QEMU $(pwd)/a.out info.enc ${rware_dir}/keys &> /dev/null
rm info.enc
popd
check_all_decrypted "unrware.arm.static.thumb"
check_base_usage "unrware.arm.static.thumb" "$expected_unrware_base_static"

in_dir "$rware_dir" make clean

## genecdh (static)
in_dir "$rware_dir" make -f Makefile.arm.static.thumb

echo ">>> arm thumb genecdh (statically-linked)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/genecdh_sym" --arch thumb &> /dev/null
$QEMU $(pwd)/a.out genkey &> /dev/null
popd
check_genecdh_files "genecdh.arm.static.thumb"
check_base_usage "genecdh.arm.static.thumb" "$expected_genecdh_base_static"

in_dir "$rware_dir" make clean


###########
# closing #
###########
if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

echo "testing success."
