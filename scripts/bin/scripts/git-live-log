#!/usr/bin/env bash

# We want the output to fit within the terminal window without scrolling it so
# that the latest commit is shown at the top.
# As git log may output more lines than commits we limit the output using head.

# TODO: Handle long lines with escape codes

git_log="git --no-pager log --oneline --graph --decorate --color=always"
cmd="n=\$(tput lines); $git_log --max-count=\$((n-1)) $* | head -n\$((n-2))"

while true; do
  find .git/index .git/HEAD .git/ORIG_HEAD .git/FETCH_HEAD .git/logs .git/refs -type f | entr -c bash -c "$cmd"
  sleep 1
done
