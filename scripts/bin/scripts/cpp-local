#!/usr/bin/env python

import os
import sys

def print_file(dir, printed, path):
    if path in printed:
        return

    with open(dir + "/" + path, "r") as f:
        for line in f.readlines():
            if line.startswith("#include \""):
                new_path = line.split('"')[1]
                print_file(dir, printed & {path}, new_path)
            else:
                sys.stdout.write(line)

if len(sys.argv) == 3:
    dir  = sys.argv[1]
    file = sys.argv[2]
    print_file(dir, set(), os.path.relpath(file, dir))
else:
    sys.stderr.write("Usage: %s DIRECTORY FILE\n" % sys.argv[0])
