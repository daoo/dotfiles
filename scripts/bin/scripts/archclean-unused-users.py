#!/usr/bin/env python3

from __future__ import annotations

import os
import shlex
import shutil
import subprocess
import sys
from pathlib import Path

SYSUSERS_DIRS = (Path("/usr/lib/sysusers.d"), Path("/etc/sysusers.d"))
SCAN_ROOT = "/"
COLORS = {
    "reset": "\033[0m",
    "bold": "\033[1m",
    "red": "\033[31m",
    "green": "\033[32m",
    "yellow": "\033[33m",
}


def use_color(stream: object) -> bool:
    if os.getenv("NO_COLOR") is not None:
        return False
    if os.getenv("TERM") == "dumb":
        return False
    isatty = getattr(stream, "isatty", None)
    return bool(isatty and isatty())


def style(text: str, *names: str, stderr: bool = False) -> str:
    enabled = use_color(sys.stderr if stderr else sys.stdout)
    if not enabled or not names:
        return text
    return "".join(COLORS[name] for name in names) + text + COLORS["reset"]


def eprint(message: str) -> None:
    if message.startswith("error:"):
        message = style(message, "bold", "red", stderr=True)
    elif message.startswith("warning:"):
        message = style(message, "yellow", stderr=True)
    print(message, file=sys.stderr)


def parse_sysusers() -> tuple[set[str], set[str], int]:
    sysusers_files: list[Path] = []
    for directory in SYSUSERS_DIRS:
        sysusers_files.extend(sorted(directory.glob("*.conf")))

    if not sysusers_files:
        locations = " or ".join(str(path) for path in SYSUSERS_DIRS)
        raise FileNotFoundError(
            f"no sysusers files found in {locations}"
        )

    users: set[str] = set()
    groups: set[str] = set()
    malformed_lines = 0

    for conf_file in sysusers_files:
        for line_number, line in enumerate(
            conf_file.read_text(encoding="utf-8", errors="replace").splitlines(), start=1
        ):
            line = line.strip()
            if not line or line.startswith("#"):
                continue

            try:
                fields = shlex.split(line, comments=True, posix=True)
            except ValueError:
                malformed_lines += 1
                eprint(f"warning: malformed line ignored: {conf_file}:{line_number}")
                continue

            if not fields:
                continue

            directive = fields[0]
            if directive in {"u", "u!"} and len(fields) >= 2:
                username = fields[1]
                if username != "-":
                    users.add(username)
                    groups.add(username)
            elif directive in {"g", "g!"} and len(fields) >= 2:
                group = fields[1]
                if group != "-":
                    groups.add(group)
            elif directive in {"m", "m!"} and len(fields) >= 3:
                username = fields[1]
                group = fields[2]
                if username != "-":
                    users.add(username)
                if group != "-":
                    groups.add(group)

    return users, groups, malformed_lines


def fd_scan_paths(owner: str) -> list[str]:
    result = subprocess.run(
        ["fd", "--owner", owner, "--one-file-system", "--prune", ".", SCAN_ROOT],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL,
        text=True,
    )
    return result.stdout.splitlines()


def read_account_names(path: Path) -> list[str]:
    names: list[str] = []
    for line in path.read_text(encoding="utf-8", errors="replace").splitlines():
        if not line or line.startswith("#"):
            continue
        name, _, _ = line.partition(":")
        if name:
            names.append(name)
    return names


def print_missing_accounts(
    title: str,
    names: list[str],
    known: set[str],
    owner_prefix: str = "",
) -> None:
    missing = [name for name in names if name not in known]
    good_count = len(names) - len(missing)
    print(style(f"{title}: good={good_count} bad={len(missing)}", "bold", "green"))

    if not missing:
        print("- none")
        return

    scanned: list[tuple[str, list[str]]] = []
    for name in missing:
        scanned.append((name, fd_scan_paths(f"{owner_prefix}{name}")))

    scanned.sort(key=lambda item: len(item[1]), reverse=True)
    for name, paths in scanned:
        line = f"- {name:<24} files: {len(paths)}"
        print(style(line, "red"))
        for path in paths:
            print(f"  {path}")


def main() -> int:
    if os.geteuid() != 0:
        eprint(
            "warning: running without root privileges may miss files due to permission errors",
        )

    try:
        sysusers_users, sysusers_groups, malformed_lines = parse_sysusers()
    except FileNotFoundError as err:
        eprint(f"error: {err}")
        return 1

    if shutil.which("fd") is None:
        eprint("error: fd is required for scanning; install fd")
        return 1

    if not Path(SCAN_ROOT).exists():
        eprint(f"error: scan root does not exist: {SCAN_ROOT}")
        return 1

    passwd_users = read_account_names(Path("/etc/passwd"))
    passwd_groups = read_account_names(Path("/etc/group"))

    print_missing_accounts("users not in sysusers.d", passwd_users, sysusers_users)

    print()

    print_missing_accounts(
        "groups not in sysusers.d", passwd_groups, sysusers_groups, owner_prefix=":"
    )

    if malformed_lines > 0:
        print(f"note: ignored {malformed_lines} malformed sysusers.d lines", file=sys.stderr)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
