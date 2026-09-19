#!/usr/bin/env python3

import subprocess
import sys


def run_pacman(*args: str) -> str:
    return subprocess.check_output(["pacman", *args], text=True)


def explicit_packages() -> list[str]:
    output = run_pacman("-Qe")
    return [line.split()[0] for line in output.splitlines() if line]


def parse_required_by() -> dict[str, list[str]]:
    lines = run_pacman("-Qi").splitlines()
    required_by_map: dict[str, list[str]] = {}
    fields: dict[str, str] = {}
    current_key: str | None = None

    def flush_block() -> None:
        name = fields.get("Name")
        if not name:
            return

        raw_required_by = fields.get("Required By", "None").strip()
        if raw_required_by == "None":
            required_by_map[name] = []
        else:
            required_by_map[name] = raw_required_by.split()

    for line in lines + [""]:
        if not line.strip():
            flush_block()
            fields = {}
            current_key = None
            continue

        if ":" in line and not line.startswith(" "):
            key, value = line.split(":", 1)
            current_key = key.strip()
            fields[current_key] = value.strip()
            continue

        if line.startswith(" ") and current_key is not None:
            fields[current_key] = f"{fields[current_key]} {line.strip()}".strip()

    return required_by_map


def main() -> int:
    try:
        packages = explicit_packages()
        required_by_map = parse_required_by()

        for package in packages:
            requirees = required_by_map.get(package, [])
            if requirees:
                print(f"{package}: {' '.join(requirees)}")
    except subprocess.CalledProcessError as err:
        print(f"pacman failed with exit code {err.returncode}", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
