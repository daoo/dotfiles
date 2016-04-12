#!/usr/bin/env bash

echo_err() {
  echo "$1" >&2
}

if [[ $# -ne 2 ]]; then
  echo_err "Usage: $0 LINE FILE"
  exit 1
fi

line="$1"
file="$2"

number_regex='^[0-9]+$'
if ! [[ "$line" =~ $number_regex ]]; then
  echo_err "Error: \"$line\" is not an non-negative integer."
  exit 1
fi

if ! [[ -r "$file" ]]; then
  echo_err "Error: could not read \"$file\"."
  exit 1
fi

str=$(tail "-n+$line" "$file" | head -n1)
if [[ -n "$str" ]]; then
  echo "$file: $str"
fi
