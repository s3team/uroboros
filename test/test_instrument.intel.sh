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

read -r -d '' expected_test01 <<'EOF'
filename: a.out
10
ABC
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

read -r -d '' expected_rware <<'EOF'
filename: a.out
passed int: 24
passed int: 16
passed int: 16
passed int: 16
passed int: 16
passed int: 16
passed int: 16
EOF

read -r -d '' expected_unrware <<'EOF'
filename: a.out
called 1 times
called 2 times
called 3 times
EOF

read -r -d '' expected_genecdh <<'EOF'
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
  echo "run: python3 uroboros.py $binary"
  python3 uroboros.py "$binary" &> /dev/null
  echo "run: script -qec $(pwd)/a.out $* /dev/null"
  script -qec "$(pwd)/a.out $*" /dev/null 2>&1 | tr -d '\r' > "$tmpfile"
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
  script -qec "$(pwd)/a.out" /dev/null 2>&1 | tr -d '\r' > "$tmpfile"
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
run_test "32-bit test00" point_examples/points.test00.intel.32.ins \
  "$(pwd)/test/test00/test00.intel.32.nopie.dynamic.sym" "$expected_test00"
run_test "64-bit test00" point_examples/points.test00.intel.64.ins \
  "$(pwd)/test/test00/test00.intel.64.nopie.dynamic.sym" "$expected_test00"

##########
# test01 #
##########
# test old instrumentation approach (disabled)
#run_test "32-bit test01" point_examples/points.test01.intel.32.ins \
#  "$(pwd)/test/test01/test01.intel.32.nopie.dynamic.sym" "$expected_test01"
#run_test "64-bit test01" point_examples/points.test01.intel.64.ins \
#  "$(pwd)/test/test01/test01.intel.64.nopie.dynamic.sym" "$expected_test01"

##########
# test05 #
##########
run_test "32-bit test05" point_examples/points.test05.intel.32.ins \
  "$(pwd)/test/test05/test05.intel.32.nopie.dynamic.sym" "$expected_test05"
run_test "64-bit test05" point_examples/points.test05.intel.64.ins \
  "$(pwd)/test/test05/test05.intel.64.nopie.dynamic.sym" "$expected_test05"

##########
# test07 #
##########
run_test "32-bit test07" point_examples/points.test07.intel.32.ins \
  "$(pwd)/test/test07/test07.intel.32.nopie.dynamic.sym" "$expected_test07"
run_test "64-bit test07" point_examples/points.test07.intel.64.ins \
  "$(pwd)/test/test07/test07.intel.64.nopie.dynamic.sym" "$expected_test07"

############
# simul_io #
############

# dynamically-linked tests

simul_io_dir=$(pwd)/test/simul_io
in_dir "$simul_io_dir" make clean
in_dir "$simul_io_dir" make intel

cp "$simul_io_dir"/capture.c "$src_dir"/capture.c
run_test "32-bit simul_io" \
  ../test/simul_io/points.float_capture.intel.32.ins \
  "$simul_io_dir/simul_io.intel.32.nopie.dynamic.sym" "$expected_simul_io"
rm "$src_dir"/capture.c

in_dir "$simul_io_dir" make clean
in_dir "$simul_io_dir" make intel

cp "$simul_io_dir"/capture.c "$src_dir"/capture.c
run_test "64-bit simul_io" \
  ../test/simul_io/points.float_capture.intel.64.ins \
  "$simul_io_dir/simul_io.intel.64.nopie.dynamic.sym" "$expected_simul_io"
rm "$src_dir"/capture.c

# statically-linked tests

in_dir "$simul_io_dir" make clean
in_dir "$simul_io_dir" make intel

cp "$simul_io_dir"/capture.c "$src_dir"/capture.c
run_test "32-bit simul_io (statically-linked)" \
  ../test/simul_io/points.float_capture.intel.32.ins \
  "$simul_io_dir/simul_io.intel.32.nopie.static.sym" "$expected_simul_io"
rm "$src_dir"/capture.c

in_dir "$simul_io_dir" make clean
in_dir "$simul_io_dir" make intel

cp "$simul_io_dir"/capture.c "$src_dir"/capture.c
run_test "64-bit simul_io (statically-linked)" \
  ../test/simul_io/points.float_capture.intel.64.ins \
  "$simul_io_dir/simul_io.intel.64.nopie.static.sym" "$expected_simul_io"
rm "$src_dir"/capture.c

# statically-linked tests (PIE)

in_dir "$simul_io_dir" make clean
in_dir "$simul_io_dir" make intel

cp "$simul_io_dir"/capture.c "$src_dir"/capture.c
run_test "32-bit simul_io (statically-linked, PIE)" \
  ../test/simul_io/points.float_capture.intel.32.ins \
  "$simul_io_dir/simul_io.intel.32.pie.static.sym" "$expected_simul_io"
rm "$src_dir"/capture.c

in_dir "$simul_io_dir" make clean
in_dir "$simul_io_dir" make intel

cp "$simul_io_dir"/capture.c "$src_dir"/capture.c
run_test "64-bit simul_io (statically-linked, PIE)" \
  ../test/simul_io/points.float_capture.intel.64.ins \
  "$simul_io_dir/simul_io.intel.64.pie.static.sym" "$expected_simul_io"
rm "$src_dir"/capture.c

in_dir "$simul_io_dir" make clean

#########################################
# rware tests (genecdh, rware, unrware) #
#########################################

############################
# dynamically-linked tests #
############################

# rware/unrware 32-bit
in_dir "$rware_dir" make clean
in_dir "$rware_dir" make -f Makefile.intel.32

run_test "32-bit rware (dynamically-linked)" \
  ${rware_dir}/points.rware.intel.32.ins \
  "${rware_dir}/build/rware_sym" "$expected_rware" \
  ${rware_dir}/hello
check_all_encrypted "rware.intel.32"
check_base_usage "rware.intel.32" "$expected_rware_base"

TEST_GREP="called\|filename" run_test "32-bit unrware (dynamically-linked)" \
  ${rware_dir}/points.unrware.intel.32.ins \
  "${rware_dir}/build/unrware_sym" "$expected_unrware" \
  info.enc ${rware_dir}/keys
pushd "$src_dir"; rm -f info.enc; popd
check_all_decrypted "unrware.intel.32"
check_base_usage "unrware.intel.32" "$expected_unrware_base"

in_dir "$rware_dir" make clean

# rware/unrware 64-bit
in_dir "$rware_dir" make -f Makefile.intel.64

run_test "64-bit rware (dynamically-linked)" \
  ${rware_dir}/points.rware.intel.64.ins \
  "${rware_dir}/build/rware_sym" "$expected_rware" \
  ${rware_dir}/hello
check_all_encrypted "rware.intel.64"
check_base_usage "rware.intel.64" "$expected_rware_base"

TEST_GREP="called\|filename" run_test "64-bit unrware (dynamically-linked)" \
  ${rware_dir}/points.unrware.intel.64.ins \
  "${rware_dir}/build/unrware_sym" "$expected_unrware" \
  info.enc ${rware_dir}/keys
pushd "$src_dir"; rm -f info.enc; popd
check_all_decrypted "unrware.intel.64"
check_base_usage "unrware.intel.64" "$expected_unrware_base"

in_dir "$rware_dir" make clean

# genecdh 32-bit (dynamically-linked)
in_dir "$rware_dir" make -f Makefile.intel.32

run_test "32-bit genecdh (dynamically-linked)" \
  ${rware_dir}/points.genecdh.intel.32.ins \
  "${rware_dir}/build/genecdh_sym" "$expected_genecdh" \
  genkey
check_genecdh_files "genecdh.intel.32"
check_base_usage "genecdh.intel.32" "$expected_genecdh_base"

in_dir "$rware_dir" make clean

# genecdh 64-bit (dynamically-linked)
in_dir "$rware_dir" make -f Makefile.intel.64

run_test "64-bit genecdh (dynamically-linked)" \
  ${rware_dir}/points.genecdh.intel.64.ins \
  "${rware_dir}/build/genecdh_sym" "$expected_genecdh" \
  genkey
check_genecdh_files "genecdh.intel.64"
check_base_usage "genecdh.intel.64" "$expected_genecdh_base"

in_dir "$rware_dir" make clean

###########################
# statically-linked tests #
###########################

# rware/unrware 32-bit (static)
in_dir "$rware_dir" make clean
in_dir "$rware_dir" make -f Makefile.intel.static.32

echo ">>> 32-bit rware (statically-linked)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/rware_sym" &> /dev/null
$(pwd)/a.out ${rware_dir}/hello &> /dev/null
popd
check_all_encrypted "rware.intel.static.32"
check_base_usage "rware.intel.static.32" "$expected_rware_base_static"

echo ">>> 32-bit unrware (statically-linked)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/unrware_sym" &> /dev/null
$(pwd)/a.out info.enc ${rware_dir}/keys &> /dev/null
rm info.enc
popd
check_all_decrypted "unrware.intel.static.32"
check_base_usage "unrware.intel.static.32" "$expected_unrware_base_static"

in_dir "$rware_dir" make clean

# rware/unrware 64-bit (static)
in_dir "$rware_dir" make -f Makefile.intel.static.64

echo ">>> 64-bit rware (statically-linked)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/rware_sym" &> /dev/null
$(pwd)/a.out ${rware_dir}/hello &> /dev/null
popd
check_all_encrypted "rware.intel.static.64"
check_base_usage "rware.intel.static.64" "$expected_rware_base_static"

echo ">>> 64-bit unrware (statically-linked)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/unrware_sym" &> /dev/null
$(pwd)/a.out info.enc ${rware_dir}/keys &> /dev/null
rm info.enc
popd
check_all_decrypted "unrware.intel.static.64"
check_base_usage "unrware.intel.static.64" "$expected_unrware_base_static"

in_dir "$rware_dir" make clean

# rware/unrware 32-bit (static, PIE)
in_dir "$rware_dir" make clean
in_dir "$rware_dir" make -f Makefile.intel.static.pie.32

echo ">>> 32-bit rware (statically-linked, PIE)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/rware_sym" &> /dev/null
$(pwd)/a.out ${rware_dir}/hello &> /dev/null
popd
check_all_encrypted "rware.intel.static.pie.32"
check_base_usage "rware.intel.static.pie.32" "$expected_rware_base_static"

echo ">>> 32-bit unrware (statically-linked, PIE)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/unrware_sym" &> /dev/null
$(pwd)/a.out info.enc ${rware_dir}/keys &> /dev/null
rm info.enc
popd
check_all_decrypted "unrware.intel.static.pie.32"
check_base_usage "unrware.intel.static.pie.32" "$expected_unrware_base_static"

in_dir "$rware_dir" make clean

# rware/unrware 64-bit (static, PIE)
in_dir "$rware_dir" make -f Makefile.intel.static.pie.64

echo ">>> 64-bit rware (statically-linked, PIE)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/rware_sym" &> /dev/null
$(pwd)/a.out ${rware_dir}/hello &> /dev/null
popd
check_all_encrypted "rware.intel.static.pie.64"
check_base_usage "rware.intel.static.pie.64" "$expected_rware_base_static"

echo ">>> 64-bit unrware (statically-linked, PIE)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/unrware_sym" &> /dev/null
$(pwd)/a.out info.enc ${rware_dir}/keys &> /dev/null
rm info.enc
popd
check_all_decrypted "unrware.intel.static.pie.64"
check_base_usage "unrware.intel.static.pie.64" "$expected_unrware_base_static"

in_dir "$rware_dir" make clean

# genecdh 32-bit (static)
in_dir "$rware_dir" make -f Makefile.intel.static.32

echo ">>> 32-bit genecdh (statically-linked)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/genecdh_sym" &> /dev/null
$(pwd)/a.out genkey &> /dev/null
popd
check_genecdh_files "genecdh.intel.static.32"
check_base_usage "genecdh.intel.static.32" "$expected_genecdh_base_static"

in_dir "$rware_dir" make clean

# genecdh 64-bit (static)
in_dir "$rware_dir" make -f Makefile.intel.static.64

echo ">>> 64-bit genecdh (statically-linked)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/genecdh_sym" &> /dev/null
$(pwd)/a.out genkey &> /dev/null
popd
check_genecdh_files "genecdh.intel.static.64"
check_base_usage "genecdh.intel.static.64" "$expected_genecdh_base_static"

in_dir "$rware_dir" make clean

# genecdh 32-bit (static, PIE)
in_dir "$rware_dir" make -f Makefile.intel.static.pie.32

echo ">>> 32-bit genecdh (statically-linked, PIE)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/genecdh_sym" &> /dev/null
$(pwd)/a.out genkey &> /dev/null
popd
check_genecdh_files "genecdh.intel.static.pie.32"
check_base_usage "genecdh.intel.static.pie.32" "$expected_genecdh_base_static"

in_dir "$rware_dir" make clean

# genecdh 64-bit (static, PIE)
in_dir "$rware_dir" make -f Makefile.intel.static.pie.64

echo ">>> 64-bit genecdh (statically-linked, PIE)"
pushd "$src_dir"
python3 uroboros.py "${rware_dir}/build/genecdh_sym" &> /dev/null
$(pwd)/a.out genkey &> /dev/null
popd
check_genecdh_files "genecdh.intel.static.pie.64"
check_base_usage "genecdh.intel.static.pie.64" "$expected_genecdh_base_static"

in_dir "$rware_dir" make clean

###########
# closing #
###########
if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

echo "testing success."
