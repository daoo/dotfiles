#!/usr/bin/env python

import subprocess
import sys


def query(server):
    cmd = ["qstat", "-raw", ",", "-q3s", server]
    return subprocess.check_output(cmd).decode("UTF-8").strip().rsplit("\n")


def player_count(lines):
    tmp = lines[0].split(",")
    game = tmp[0]
    status = tmp[2]

    if game != "Q3S":
        return -3
    if status == "DOWN":
        return -2
    if status == "TIMEOUT":
        return -1

    count = int(tmp[5])

    return count


if len(sys.argv) == 2:
    print(player_count(query(sys.argv[1])))
else:
    sys.stderr.write("Usage: %s SERVER\n" % (sys.argv[0]))
    sys.exit(1)
