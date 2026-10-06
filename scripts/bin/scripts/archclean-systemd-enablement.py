#!/usr/bin/env python3
"""Compare installed post_install enable commands with systemd enablement links.

Recognizes simple unconditional systemctl enable commands and p11-kit's link
helper; other forms, conditionals, upgrade scripts and hooks need manual review.
"""

import os
import re
import subprocess
import sys
from pathlib import Path

DB = Path("/var/lib/pacman/local")
UNIT_TYPES = {"service", "socket", "timer", "target", "mount", "automount", "path", "slice", "swap"}
FUNCTION = r"^{}\s*\(\)\s*\{{\s*\n(.*?)^\}}"
ENABLE = re.compile(r"^(?:/usr/bin/)?systemctl (?:(--global) )?enable ([\w@.:-]+(?: [\w@.:-]+)*)$")


def body(script: str, name: str) -> str:
    match = re.search(FUNCTION.format(name), script, re.MULTILINE | re.DOTALL)
    return match.group(1) if match else ""


def commands(text: str) -> list[str]:
    return [line.strip() for line in text.replace("\\\n", " ").splitlines()
            if line.strip() and not line.lstrip().startswith("#")]


def requests(script: str) -> list[tuple[str, str, str]]:
    lines = commands(body(script, "post_install"))
    if any(re.search(r"\b(if|then|else|elif|fi|case|esac|for|while|until|do|done)\b|[;&|`<>{}]", line)
           for line in lines):
        return []

    found = []
    for line in lines:
        command = " ".join(line.split("#", 1)[0].split())
        match = ENABLE.fullmatch(command)
        if match:
            scope = "user-global" if match.group(1) else "system"
            found.extend((scope, unit, command) for unit in match.group(2).split())

    # p11-kit enables its socket with a post_install helper instead of systemctl.
    helper = body(script, "_global_units")
    unit = re.search(r"\blocal unit=([\w@.:-]+) dir=/etc/systemd/user/sockets\.target\.wants\b", helper)
    if (lines == ["_global_units enable"] and unit and "enable)" in helper
            and "ln -sf /usr/lib/systemd/user/$unit $dir/$unit" in helper):
        found.append(("user-global", unit.group(1), "_global_units enable (ln -sf .../$unit $dir/$unit)"))
    return found


def enabled_links() -> dict[tuple[str, str], list[Path]]:
    links: dict[tuple[str, str], list[Path]] = {}
    roots = (
        ("system", Path("/etc/systemd/system")),
        ("system", Path("/run/systemd/system")),
        ("user-global", Path("/etc/systemd/user")),
        ("user-global", Path("/run/systemd/user")),
    )
    for scope, root in roots:
        for link in root.rglob("*"):
            if link.parent != root and not (
                link.parent.parent == root
                and link.parent.suffix in (".wants", ".requires", ".upholds")
            ):
                continue
            if link.is_symlink() and link.suffix.lstrip(".") in UNIT_TYPES:
                target = link.resolve()
                if target != Path("/dev/null"):
                    links.setdefault((scope, target.name), []).append(link)
    return links


def also_units(scope: str, unit: str) -> set[str]:
    kind = "user" if scope == "user-global" else "system"
    for base in ("/etc/systemd", "/run/systemd", "/usr/lib/systemd"):
        path = Path(base) / kind / unit
        if path.is_file():
            install = path.read_text(errors="replace").partition("[Install]")[2].split("[", 1)[0]
            return {name for line in install.splitlines() if line.strip().startswith("Also=")
                    for name in line.partition("=")[2].split()}
    return set()


def mismatch(text: str) -> str:
    if sys.stdout.isatty() and os.getenv("NO_COLOR") is None and os.getenv("TERM") != "dumb":
        return f"\033[1;31m{text}\033[0m"
    return text


def main() -> int:
    if not DB.is_dir():
        print(f"error: pacman database not found: {DB}", file=sys.stderr)
        return 1
    try:
        requested: set[tuple[str, str]] = set()
        enabled_count = 0
        state_mismatches: list[tuple[str, str, str, str, Path, str]] = []
        for install in sorted(DB.glob("*/install")):
            script = install.read_text(errors="replace")
            found = requests(script)
            if not found:
                continue
            desc = (install.parent / "desc").read_text(errors="replace")
            package = re.search(r"^%NAME%\n([^\n]+)", desc, re.MULTILINE)
            if package is None:
                raise ValueError(f"missing package name in {install.parent / 'desc'}")
            for scope, unit, command in found:
                requested.add((scope, unit))
                result = subprocess.run(
                    ["systemctl", *(["--global"] if scope == "user-global" else []), "is-enabled", unit],
                    capture_output=True, text=True, check=False,
                )
                state = result.stdout.strip() or result.stderr.strip()
                if state in ("enabled", "enabled-runtime"):
                    enabled_count += 1
                else:
                    state_mismatches.append((package.group(1), unit, scope, state, install, command))

        print(f"Install-script enable requests: {enabled_count} enabled, "
              f"{len(state_mismatches)} not enabled")
        for package, unit, scope, state, install, command in state_mismatches:
            print(f"  {mismatch(f'{package}: {unit} ({scope}) -> {state}')}")
            print(f"    {install}: {command}")

        links = enabled_links()
        accompanied = {(scope, other) for scope, unit in links for other in also_units(scope, unit)}
        unexplained = {key: paths for key, paths in links.items()
                       if key not in requested and key not in accompanied}
        print(f"\nEnabled without a recognized install-script request: {len(unexplained)} (review)")
        for (scope, unit), paths in sorted(unexplained.items()):
            print(f"  {unit} ({scope})")
            for path in sorted(paths):
                print(f"    {path}")
    except (OSError, ValueError) as err:
        print(f"error: {err}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
