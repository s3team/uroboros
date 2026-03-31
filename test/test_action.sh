#!/bin/bash

arch="$1"

if [[ "$arch" == "intel" ]]; then
  #########
  # intel #
  #########

  tmpfile=$(mktemp)
  python3 $(pwd)/test/test_all.py -a --arch intel &> "$tmpfile"
  failed=$(cat "$tmpfile" | grep "Uroboros failed on")
  mismatched=$(cat "$tmpfile" | grep "Output mismatch for")
  failed=$(echo "$failed" | sort)
  mismatched=$(echo "$mismatched" | sort)

  echo "##### raw intel outputs #####"
  cat "$tmpfile"
  rm "$tmpfile"

  has_failed="false"
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
  python3 $(pwd)/test/test_all.py -a --arch arm &> "$tmpfile"
  failed=$(cat "$tmpfile" | grep "Uroboros failed on")
  mismatched=$(cat "$tmpfile" | grep "Output mismatch for")
  failed=$(echo "$failed" | sort)
  mismatched=$(echo "$mismatched" | sort)

  echo "##### raw arm outputs #####"
  cat "$tmpfile"
  rm "$tmpfile"

  has_failed="false"
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
