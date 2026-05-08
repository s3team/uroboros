#!/bin/bash

set -o pipefail

arch="$1"
has_failed="false"

if [[ "$arch" == "intel" ]]; then
  #########
  # intel #
  #########

  tmpfile=$(mktemp)
  echo "##### raw intel outputs #####"
  if ! stdbuf -oL -eL python3 $(pwd)/test/test_all.py -a --arch intel 2>&1 | tee "$tmpfile"; then
    echo "##### test_all.py exited non-zero (intel) #####"
    has_failed="true"
  fi
  failed=$(grep "Uroboros failed on" "$tmpfile")
  mismatched=$(grep "Output mismatch for" "$tmpfile")
  failed=$(echo "$failed" | sort)
  mismatched=$(echo "$mismatched" | sort)

  echo "##### raw intel outputs #####"
  cat "$tmpfile"
  rm "$tmpfile"

  if [[ -n "$failed" ]]; then
    echo "##### expected failures not matching (intel) #####"
    echo "~ actual:"
    echo "$failed"
    has_failed="true"
  fi

  if [[ -n "$mismatched" ]]; then
    echo "##### expected runtime failures not matching (intel) #####"
    echo "~ actual:"
    echo "$mismatched"
    has_failed="true"
  fi

elif [[ "$arch" == "arm" ]]; then
  #######
  # arm #
  #######

  tmpfile=$(mktemp)
  echo "##### raw arm outputs #####"
  if ! stdbuf -oL -eL python3 $(pwd)/test/test_all.py -a --arch arm 2>&1 | tee "$tmpfile"; then
    echo "##### test_all.py exited non-zero (arm) #####"
    has_failed="true"
  fi
  failed=$(grep "Uroboros failed on" "$tmpfile")
  mismatched=$(grep "Output mismatch for" "$tmpfile")
  failed=$(echo "$failed" | sort)
  mismatched=$(echo "$mismatched" | sort)

  echo "##### raw arm outputs #####"
  cat "$tmpfile"
  rm "$tmpfile"

  if [[ -n "$failed" ]]; then
    echo "##### expected failures not matching (arm) #####"
    echo "~ actual:"
    echo "$failed"
    has_failed="true"
  fi

  if [[ -n "$mismatched" ]]; then
    echo "##### expected runtime failures not matching (arm) #####"
    echo "~ actual:"
    echo "$mismatched"
    has_failed="true"
  fi

else
  echo "Usage: $0 {intel|arm}"
  exit 1
fi

# error check

if [[ "$has_failed" == "true" ]]; then
  exit 1
fi

echo "testing success."
