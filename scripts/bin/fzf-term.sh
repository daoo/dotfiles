#!/usr/bin/env bash

set -eu

term="${TERMCMD:-xterm}"
dir="${TMPDIR:-/tmp}"

id=$RANDOM
argsf="${dir}/fzf-args-$id"
fifo0="${dir}/fzf-fifo0-$id"
fifo1="${dir}/fzf-fifo1-$id"
fifo2="${dir}/fzf-fifo2-$id"

cleanup() {
  rm -f "$argsf" "$fifo0" "$fifo1" "$fifo2"
}

trap cleanup EXIT SIGINT SIGTERM

mkfifo "$fifo0"
mkfifo "$fifo1"
mkfifo "$fifo2"
cat <<< "fzf <$fifo0 >$fifo1; echo \$? >$fifo2" > "$argsf"
$term -name scratchpad -e bash "$argsf" &
cat <&0 >"$fifo0" &

cat "$fifo1"
exit "$(cat "$fifo2")"
