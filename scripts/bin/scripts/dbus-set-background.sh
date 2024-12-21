#!/usr/bin/env bash

set -eu

dbus-send --system \
  --dest=org.freedesktop.Accounts \
  "/org/freedesktop/Accounts/User$(id -u)" \
  org.freedesktop.DBus.Properties.Set \
  string:org.freedesktop.DisplayManager.AccountsService \
  string:BackgroundFile \
  variant:string:"$1"
