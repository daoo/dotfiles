#!/usr/bin/env python

import os
import subprocess
import sys
import time


def notify_send(args):
    subprocess.call(["notify-send"] + args)


def query(server):
    cmd = ["qstat", "-raw", ",", "-P", "-q3s", server]
    return subprocess.check_output(cmd).decode("UTF-8").strip().rsplit("\n")


class Status:
    OK = 0
    DOWN = 1
    TIMEOUT = 2


class State:
    def __init__(self, address, name, status, map, player_count, player_names):
        assert isinstance(address, str) and len(address) > 0
        assert isinstance(name, str) and len(name) > 0
        assert status >= Status.OK and status <= Status.TIMEOUT
        assert isinstance(player_count, int)
        assert isinstance(player_names, list)
        assert player_count == len(player_names)

        self.address = address
        self.name = name
        self.status = status
        self.map = map
        self.player_count = player_count
        self.player_names = player_names


def server_state(lines):
    tmp = lines[0].split(",")
    game = tmp[0]
    address = tmp[1]
    name = tmp[2]

    if game != "Q3S":
        raise ValueError("not a quake 3 server (%s)" % game)

    status = Status.OK

    if name == "DOWN":
        status = Status.DOWN
        return State(address, name, status, map=None, player_count=0, player_names=[])

    if name == "TIMEOUT":
        status = Status.TIMEOUT
        return State(address, name, status, map=None, player_count=0, player_names=[])

    map = tmp[3]
    # tmp[4] is max player count
    player_count = int(tmp[5])

    if len(lines) <= 1:
        player_names = []
    else:
        player_names = [line.split(",")[0] for line in lines[1:]]

    return State(address, name, status, map, player_count, player_names)


def loop(server):
    count_file = os.path.join(os.environ["XDG_RUNTIME_DIR"], "quake-counter")
    with open(count_file, "w") as file:
        file.write(str(-1))

    player_count = 0
    player_names = set()

    while True:
        state = server_state(query(server))

        if state.status == Status.OK:

            if state.player_count != player_count:
                current_players = set(state.player_names)

                joined = current_players.difference(player_names)
                leaved = player_names.difference(current_players)

                notify_args = ["-i", "info", "Quake"]
                if leaved:
                    message = "%s left %s" % (', '.join(leaved), state.name)
                    notify_send(notify_args + [message])
                if joined:
                    message = "%s joined %s" % (', '.join(joined), state.name)
                    notify_send(notify_args + [message])

                player_names = set(state.player_names)
                player_count = state.player_count

                with open(count_file, "w") as file:
                    file.write(str(player_count))

                time.sleep(10)

            else:
                time.sleep(60)

        else:
            with open(count_file, "w") as file:
                file.write(str(-1))

            time.sleep(60)


if len(sys.argv) == 2:
    loop(sys.argv[1])
else:
    sys.stderr.write("Usage: %s SERVER\n" % (sys.argv[0]))
    sys.exit(1)
