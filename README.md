# My dotfiles

To deploy, check out the repo and run `./install.sh`.
If you are migrating from `stow`, use `./install.sh --force` once to replace
symlinked parent directories with real directories.

The installer links files individually, so directories like `~/.config` stay as real
directories and programs can write their own state without polluting the repo.
