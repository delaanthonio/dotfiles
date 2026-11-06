"""Select a Dotbot profile and plan safe links before executing any changes."""

import argparse
import fnmatch
import glob
import json
import os
import platform
import shlex
import subprocess
import sys
import tempfile
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
PROFILES = ("minimal", "server", "workstation", "agents")
WORKSTATION_MODULES = (
    "emacs.d",
    "modules/oh-my-zsh",
    "modules/zsh-autosuggestions",
    "modules/zsh-syntax-highlighting",
)
STATE = ".config/dotfiles/profile.json"


def select_profiles(value):
    """Accept one base profile and an optional agents layer, or agents alone."""
    names = value.split(",")
    if (
        any(name not in PROFILES for name in names)
        or len(set(names)) != len(names)
        or sum(name != "agents" for name in names) > 1
    ):
        raise ValueError(
            "choose one of minimal, server, workstation, agents; "
            "append ,agents to a base profile"
        )
    return [name for name in PROFILES if name in names]


def links(root, profiles, system):
    """Expand the single manifest to concrete Dotbot links without shell code."""
    manifest = json.loads((root / "install.conf.yaml").read_text())
    for block in manifest:
        for target, options in block.get("link", {}).items():
            if not set(options["profiles"]).intersection(profiles):
                continue
            if options.get("platform", system) != system:
                continue
            source = options["path"]
            children = "children_except" in options and not set(
                options["children_except"]
            ).intersection(profiles)
            pattern = str(root / source / "*") if children else str(root / source)
            matches = (
                sorted(glob.glob(pattern, recursive=True))
                if children or options.get("glob")
                else [pattern]
            )
            if options.get("glob") and "**" in pattern:
                matches = [match for match in matches if Path(match).is_file()]
            if not matches:
                # Keep missing globs visible in previews and diagnostics.
                matches = [pattern]
            prefix = pattern
            for wildcard in "*?[":
                prefix = prefix.split(wildcard, 1)[0]
            glob_base = os.path.dirname(prefix)
            for match in matches:
                name = Path(match).name
                if children and any(fnmatch.fnmatch(name, rule) for rule in options["exclude"]):
                    continue
                if children and "minimal" in profiles and name in options.get("server_only", []):
                    continue
                destination = target.rstrip("/")
                if children or options.get("glob"):
                    destination += "/" + os.path.relpath(match, glob_base)
                yield destination, Path(match), "agents" in options["profiles"]


def blocked_parent(path, home):
    """Never write through an existing directory symlink, even with --force."""
    for parent in reversed(path.parents):
        if parent == home or home not in parent.parents:
            continue
        if parent.is_symlink() or (parent.exists() and not parent.is_dir()):
            return parent
    return None


def link_status(path, source, home, force):
    if blocked_parent(path, home):
        return "skip: unsafe parent"
    if not source.exists():
        return "skip: missing source"
    if path.is_symlink() and os.path.realpath(path) == os.path.realpath(source):
        return "unchanged"
    if os.path.lexists(path):
        return "backup" if force else "skip: existing target"
    return "create"


def backup_path(path):
    """Reserve a unique sibling name; never overwrite an earlier backup."""
    candidate = Path(str(path) + f".bak.{int(time.time())}")
    index = 0
    while os.path.lexists(candidate):
        index += 1
        candidate = Path(str(path) + f".bak.{int(time.time())}.{index}")
    return candidate


def command(arguments, dry_run):
    print("STEP " + shlex.join(str(arg) for arg in arguments), flush=True)
    if not dry_run:
        subprocess.run(arguments, cwd=ROOT, check=True)


def submodules(profiles, dry_run):
    paths = ["modules/dotbot"]
    if "workstation" in profiles:
        paths.extend(WORKSTATION_MODULES)
    for path in paths:
        ready = (ROOT / path / ".git").exists()
        if path == "modules/dotbot":
            ready = ready and (ROOT / path / "lib/pyyaml/lib/yaml/__init__.py").is_file()
        if not ready:
            command(
                [
                    "git",
                    "-C",
                    str(ROOT),
                    "submodule",
                    "update",
                    "--init",
                    "--recursive",
                    "--",
                    path,
                ],
                dry_run,
            )


def packages(profiles, system, dry_run):
    """Package managers are only reached through the explicit --packages gate."""
    base = next((name for name in profiles if name != "agents"), None)
    if base and system == "Darwin":
        if base == "workstation":
            command(["bash", str(ROOT / "config/homebrew/install.sh")], dry_run)
        else:
            names = ["fish", "starship", "neovim"]
            if base == "server":
                names.extend(["direnv", "tmux", "zellij"])
            command(["brew", "install", *names], dry_run)
    elif base and system == "Linux":
        names = ["fish", "neovim"]
        if base == "server":
            names.extend(["direnv", "tmux"])
        command(["sudo", "apt-get", "install", "-y", *names], dry_run)
        print("SKIP starship/zellij packages: install separately if unavailable in apt")
    if "agents" in profiles:
        script = ROOT / "config/npm/install.sh"
        print(f"STEP bash {script}" + ("" if script.is_file() else " [skip: missing script]"))
        if script.is_file() and not dry_run:
            subprocess.run(["bash", str(script)], cwd=ROOT, check=True)


def save_profile(home, profiles):
    path = home / STATE
    content = json.dumps({"managed_by": "dotfiles", "profiles": profiles}, indent=2) + "\n"
    if blocked_parent(path, home) or path.is_symlink():
        print("SKIP active profile: unsafe state path")
        return False
    if path.exists():
        try:
            previous = path.read_text()
            if json.loads(previous).get("managed_by") != "dotfiles":
                raise ValueError
        except (OSError, ValueError, AttributeError):
            print("SKIP active profile: existing unmanaged state")
            return False
        if previous == content:
            return False
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(content)
    return True


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--profile",
        default="minimal",
        help="minimal (default), server, workstation; optional ,agents; or agents",
    )
    parser.add_argument("--dry-run", action="store_true", help="print only; no writes or network")
    parser.add_argument(
        "--force", action="store_true", help="back up conflicting targets before linking"
    )
    parser.add_argument(
        "--force-agents",
        action="store_true",
        help="enable agents and back up only their conflicting targets",
    )
    parser.add_argument(
        "--packages", action="store_true", help="explicitly install profile packages"
    )
    args = parser.parse_args()
    try:
        profiles = select_profiles(args.profile)
    except ValueError as error:
        parser.error(str(error))
    if args.force_agents and "agents" not in profiles:
        profiles.append("agents")
    system = platform.system()
    if "workstation" in profiles and system != "Darwin":
        parser.error("workstation requires macOS; use minimal or server on Linux")
    home_value = os.environ.get("HOME", "")
    if not home_value or not Path(home_value).is_absolute():
        parser.error("HOME must be an absolute path")
    home = Path(home_value).resolve()
    print(f"PROFILE {','.join(profiles)} ({system})", flush=True)
    pending = []
    for target, source, agent in links(ROOT, profiles, system):
        destination = home / target[2:]
        status = link_status(destination, source, home, args.force or (agent and args.force_agents))
        print(f"LINK {target} <- {source.relative_to(ROOT)} [{status}]", flush=True)
        if status == "backup":
            backup = backup_path(destination)
            print(f"BACKUP {target} -> {backup}", flush=True)
        if status in ("create", "backup"):
            pending.append((destination, source, status))
    # Preview includes dependency steps even when source submodules are absent.
    if args.dry_run or pending:
        submodules(profiles, args.dry_run)
    if not args.dry_run and pending:
        selected = {}
        for destination, source, status in pending:
            # Recheck after dependency initialization and before any backup.
            current = link_status(destination, source, home, status == "backup")
            if current not in ("create", "backup"):
                print(f"SKIP changed target: {destination} [{current}]")
                continue
            if current == "backup":
                destination.rename(backup_path(destination))
            selected[str(destination)] = {"path": str(source), "create": True, "relink": False}
        with tempfile.TemporaryDirectory(prefix="dotfiles-") as directory:
            config = Path(directory) / "links.json"
            config.write_text(json.dumps([{"link": selected}]))
            subprocess.run(
                [
                    sys.executable,
                    str(ROOT / "modules/dotbot/bin/dotbot"),
                    "-d",
                    str(ROOT),
                    "-c",
                    str(config),
                ],
                cwd=ROOT,
                check=True,
            )
        changed_links = len(selected)
    else:
        changed_links = 0
    if args.packages:
        packages(profiles, system, args.dry_run)
    else:
        print("SKIP packages: use --packages to opt in")
    if args.dry_run:
        print(f"STATE ~/{STATE} <- {','.join(profiles)} (preview only)")
        print("Dry run: no changes")
    else:
        changed_state = save_profile(home, profiles)
        print(f"Changes: {changed_links} links, {int(changed_state)} profile record")
        if not changed_links and not changed_state:
            print("No link/profile changes; package steps ran" if args.packages else "No changes")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        print(f"ERROR: {error}", file=sys.stderr)
        sys.exit(1)
