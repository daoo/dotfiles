#!/usr/bin/env bash

file="$1"
if [[ -f "${file}" ]]; then
  base="$(basename "${file}")"
  tmp="$(mktemp "/tmp/${base}-XXXXXXXX")"

  objdump -d -M intel -l -S "$file" | c++filt > "${tmp}.asm"
  ${EDITOR:-nvim} "${tmp}.asm"

  rm -f "${tmp}.asm"
fi
