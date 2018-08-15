#!/usr/bin/env bash

shopt -s extglob

for arg in "$@"; do
  case $arg in
    allt|hela)
      make all
      ;;
    ren?(t)|städ@(a|ning))
      make clean
      ;;
    bygg?(e))
      make build
      ;;
    ludd|avluddning)
      make lint
      ;;
    uppsättning|installation|monter@(a|ing))
      make install
      ;;
    felsök?(a|ning)|avlus@(a|ning))
      make debug
      ;;
    fördelning|spridning)
      make dist
      ;;
    släpp|frisättning)
      make release
      ;;
    prov?(kör@(a|ning))|pröv@(a|ning)|kontroll)
      make test
      ;;
    hjälp|bistånd)
      make help
      ;;
    *)
      echo "förstår inte hur man gör $arg"
      ;;
  esac
done
