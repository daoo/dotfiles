#!/usr/bin/env bash
#
# Recursively convert ISO-8859-1 to UTF-8.
#

find -type f -print0 | while read -r -d $'\0' path; do
  enc="$(file --brief --mime-encoding "$path")"
  if [[ "$enc" == "iso-8859-1" ]]; then
    path_utf8="${path}.utf-8"
    path_bak="${path}.bak"
    if [[ ! -f "${path_utf8}" ]]; then
      echo "Fixing $path"
      iconv -f ISO-8859-1 -t UTF-8 "$path" > "$path_utf8"
      cp "$path" "$path_bak"
      mv "$path_utf8" "$path"
    fi
  fi
done