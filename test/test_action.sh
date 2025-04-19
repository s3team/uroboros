#!/bin/bash

expected_mismatch="{test_uroboros - ERROR} Output mismatch for test04.32.pie.static
{test_uroboros - ERROR} Output mismatch for test04.32.nopie.static
{test_uroboros - ERROR} Output mismatch for test04.32.nopie.dynamic"
expected_mismatch=$(echo "$expected_mismatch" | sort)

tmpfile=$(mktemp)
python3 $(pwd)/test/test_all.py -a &> "$tmpfile"
failed=$(cat "$tmpfile" | grep "Uroboros failed on" | grep "nopie")
mismatched=$(cat "$tmpfile" | grep "Output mismatch for")
failed=$(echo "$failed" | sort)
mismatched=$(echo "$mismatched" | sort)

has_failed="false"
if [[ -n "$failed" ]]; then
  echo "##### expected failures not matching #####"
  echo "~ actual:"
  echo "$failed"
  has_failed="true"
fi  

if [[ "$mismatched" != "$expected_mismatch" ]]; then
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

echo "testing success"
rm "$tmpfile"
