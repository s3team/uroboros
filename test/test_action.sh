#!/bin/bash

expected_fail="{test_uroboros - ERROR} Uroboros failed on test01.32.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test04.intel.32.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test06.intel.32.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test03.intel.32.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test05.intel.32.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test02.intel.32.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test07.intel.32.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test01.intel.64.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test04.intel.64.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test06.intel.64.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test03.intel.64.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test05.intel.64.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test02.intel.64.pie.dynamic: recompile failed
{test_uroboros - ERROR} Uroboros failed on test07.intel.64.pie.dynamic: recompile failed"
expected_fail=$(echo "$expected_fail" | sort)

expected_mismatch="{test_uroboros - ERROR} Output mismatch for test04.intel.32.pie.static
{test_uroboros - ERROR} Output mismatch for test04.intel.32.nopie.static
{test_uroboros - ERROR} Output mismatch for test04.intel.32.nopie.dynamic"
expected_mismatch=$(echo "$expected_mismatch" | sort)

tmpfile=$(mktemp)
python3 $(pwd)/test/test_all.py -a &> "$tmpfile"
failed=$(cat "$tmpfile" | grep "Uroboros failed on")
mismatched=$(cat "$tmpfile" | grep "Output mismatch for")
failed=$(echo "$failed" | sort)
mismatched=$(echo "$mismatched" | sort)

echo "##### raw outputs #####"
cat "$tmpfile"

has_failed="false"
if [[ -n "$failed" ]]; then
  echo "##### expected failures not matching #####"
  echo "~ expected:"
  echo "$expected_fail"
  echo "~ actual:"
  echo "$failed"
  has_failed="true"
fi

if [[ -n "$mismatched" ]]; then
  echo "##### expected runtime failures not matching #####"
  echo "~ expected:"
  echo "$expected_mismatch"
  echo "~ actual:"
  echo "$mismatched"
  has_failed="true"
fi

if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

echo "testing success."
rm "$tmpfile"
