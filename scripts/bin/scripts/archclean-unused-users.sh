#!/usr/bin/env bash

echo "Users not present in \"/usr/lib/sysusers.d\" can possibly be removed:"

cut -d':' -f1 /etc/passwd | while read -r user; do
  if ! rg --quiet "$user" /etc/sysusers.d/ /usr/lib/sysusers.d/; then
    echo "$user"
    fd --owner "$user" --one-file-system --prune . /
  fi
done

echo "Groups not present in \"/usr/lib/sysusers.d\" can possibly be removed:"

cut -d':' -f1 /etc/group | while read -r group; do
  if ! rg --quiet "$group" /etc/sysusers.d/ /usr/lib/sysusers.d/; then
    echo "$group"
    fd --owner ":$group" --one-file-system --prune . /
  fi
done
