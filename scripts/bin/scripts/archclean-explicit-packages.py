#!/usr/bin/env python

import subprocess


def explicit_packages():
    for line in subprocess.check_output(["pacman", "-Qe"]).splitlines():
        yield line.split(b" ")[0].strip().decode("utf-8")


def required_by(package) -> list[str]:
    lines = subprocess.check_output(["pacman", "-Qi", package]).splitlines()
    for line in lines:
        if line.startswith(b"Required By"):
            tmp = line.split(b":")[1].strip()
            if tmp == b"None":
                return []
            else:
                return [s.strip().decode("utf-8") for s in tmp.split(b" ") if s.strip()]
    return []


for package in explicit_packages():
    requirees = required_by(package)
    if len(requirees) > 0:
        print(f"{package}: {' '.join(requirees)}")
