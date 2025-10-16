#!/usr/bin/env bash

set -euo pipefail

if ! systemctl is-active --quiet bluetooth.service; then
  echo "Starting bluetooth.service..."
  sudo systemctl start bluetooth.service
  sleep 1
fi

device="$(bluetoothctl devices | grep "^Device [a-fA-F0-9:]\+" | fzf)"
if [[ -n "$device" ]]; then
  name="$(cut -d' ' -f 3- <<<"$device")"
  address="$(cut -d' ' -f 2 <<<"$device")"
  echo "Powering on bluetooth..."
  bluetoothctl power on
  echo "Connecting to $name"
  bluetoothctl connect "$address"
fi
