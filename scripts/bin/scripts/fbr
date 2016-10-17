#!/usr/bin/env bash

preview="git log -n10 \$(echo {} | cut -f1 -d' ')"
branch=$(git branch -v | cut -c 3- | fzf --nth=1 --preview="$preview" | cut -f1 -d' ')
if [[ -n "$branch" ]]; then
  git checkout "$branch"
fi
