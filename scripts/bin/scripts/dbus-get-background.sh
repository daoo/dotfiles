#!/usr/bin/env bash

set -eu

echo $(dbus-send --system --print-reply=literal \
  --dest=org.freedesktop.Accounts \
  "/org/freedesktop/Accounts/User$(id -u)" \
  org.freedesktop.DBus.Properties.Get \
  string:org.freedesktop.DisplayManager.AccountsService \
  string:BackgroundFile | sed 's/^\s*variant\s*//')
