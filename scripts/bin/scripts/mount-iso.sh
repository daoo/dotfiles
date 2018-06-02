#!/usr/bin/env bash

iso="$1"
dir="$2"

if [[ -z "$iso" || -z "$dir" ]]; then
  echo "Usage:"
  echo "mount-iso FILE.ISO /MOUNT/POINT"
  exit 1
elif [[ ! -f "$iso" ]]; then
  echo "Error: \"$iso\" is not a file."
  exit 1
elif [[ ! -d "$dir" ]]; then
  echo "Error: \"$dir\" is not a directory."
  exit 1
fi

mount -o loop -t iso9660 "$iso" "$dir"
