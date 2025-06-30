#!/usr/bin/env bash

sudo find \
  /etc /home /opt /run /usr /var \
  -mount \
  \( -name containers -or -name Steam \) -prune -or \
  -xtype l
