"""Installer regression tests. Every install uses a disposable HOME."""

import contextlib
import io
import json
import os
import runpy
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
INSTALL = runpy.run_path(str(ROOT / "bin/install-dotfiles.py"))


def snapshot(home):
    return {
        str(path.relative_to(home)): (
            ("link", os.readlink(path))
            if path.is_symlink()
            else ("directory",) if path.is_dir() else ("file", path.read_bytes())
        )
        for path in home.rglob("*")
    }


class ProfileTests(unittest.TestCase):
    def test_profile_selection(self):
        for profile in ("minimal", "server", "workstation", "agents", "server,agents"):
            self.assertTrue(INSTALL["select_profiles"](profile))
        for profile in ("", "unknown", "minimal,server", "agents,agents", "minimal,"):
            with self.assertRaisesRegex(ValueError, "minimal, server, workstation, agents"):
                INSTALL["select_profiles"](profile)

    def test_server_matches_safe_shell_subset(self):
        selected = {
            target: source for target, source, _ in INSTALL["links"](ROOT, ["server"], "Linux")
        }
        for target in (
            "~/.config/nvim",
            "~/.config/starship.toml",
            "~/.gitignore_global",
            "~/.config/direnv/direnvrc",
            "~/.config/tmux/tmux.conf",
            "~/.config/zellij/config.kdl",
        ):
            self.assertIn(target, selected)
        for target in selected:
            self.assertNotIn("Library", target)
            self.assertNotIn("claude", target)
            self.assertNotIn("emacs", target)
            self.assertNotEqual(target, "~/.gitconfig")
            self.assertNotIn(target, ("~/.zshenv", "~/.zshrc"))
        conf = {Path(target).name for target in selected if "/conf.d/" in target}
        expected = {
            p.name
            for p in (ROOT / "config/fish/conf.d").glob("*")
            if p.name not in ("brew.fish", "claude.fish") and not p.name.startswith("emacs")
        }
        self.assertEqual(conf, expected)

    def test_minimal_and_agents_are_independent(self):
        minimal = list(INSTALL["links"](ROOT, ["minimal"], "Darwin"))
        self.assertFalse(any(agent for _, _, agent in minimal))
        self.assertFalse(
            any(
                "direnv" in target or "tmux" in target or "zellij" in target
                for target, _, _ in minimal
            )
        )
        agents = list(INSTALL["links"](ROOT, ["agents"], "Linux"))
        self.assertTrue(agents)
        self.assertTrue(all(agent for _, _, agent in agents))
        self.assertFalse(any(".codex" in target or ".factory" in target for target, _, _ in agents))

    def test_recursive_globs_keep_dotbot_relative_paths(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "commands/nested").mkdir(parents=True)
            (root / "commands/top.md").write_text("top")
            (root / "commands/nested/task.md").write_text("nested")
            (root / "install.conf.yaml").write_text(
                json.dumps(
                    [
                        {
                            "link": {
                                "~/.claude/commands/": {
                                    "path": "commands/**",
                                    "glob": True,
                                    "profiles": ["agents"],
                                },
                            }
                        }
                    ]
                )
            )
            targets = {target for target, _, _ in INSTALL["links"](root, ["agents"], "Linux")}
            self.assertEqual(
                targets,
                {
                    "~/.claude/commands/top.md",
                    "~/.claude/commands/nested/task.md",
                },
            )

    def test_dry_run_never_executes_or_writes(self):
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory)
            target = home / ".config/starship.toml"
            target.parent.mkdir()
            target.write_text("local edits\n")
            before = snapshot(home)
            with mock.patch.dict(os.environ, HOME=directory), mock.patch(
                "sys.argv", ["install", "--dry-run", "--force", "--packages"]
            ), mock.patch("subprocess.run") as execute, contextlib.redirect_stdout(
                io.StringIO()
            ) as output:
                self.assertEqual(INSTALL["main"](), 0)
            execute.assert_not_called()
            self.assertEqual(snapshot(home), before)
            self.assertIn("BACKUP ~/.config/starship.toml", output.getvalue())
            self.assertIn("Dry run: no changes", output.getvalue())

    def test_unsafe_parent_and_backup_collisions(self):
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory) / "home"
            outside = Path(directory) / "outside"
            home.mkdir()
            outside.mkdir()
            (home / ".config").symlink_to(outside)
            target = home / ".config/starship.toml"
            self.assertEqual(
                INSTALL["link_status"](target, ROOT / "starship.toml", home, True),
                "skip: unsafe parent",
            )
            regular = home / "file"
            regular.write_text("original")
            with mock.patch("time.time", return_value=123):
                first = INSTALL["backup_path"](regular)
                first.write_text("older backup")
                self.assertNotEqual(first, INSTALL["backup_path"](regular))
                self.assertEqual(first.read_text(), "older backup")

    def test_default_run_never_invokes_packages(self):
        with tempfile.TemporaryDirectory() as directory:
            with mock.patch.dict(os.environ, HOME=directory), mock.patch(
                "sys.argv", ["install"]
            ), mock.patch("subprocess.run") as execute, mock.patch.dict(
                INSTALL["main"].__globals__, links=lambda *_: []
            ), contextlib.redirect_stdout(
                io.StringIO()
            ):
                self.assertEqual(INSTALL["main"](), 0)
            execute.assert_not_called()

    def test_package_and_submodule_selection(self):
        for profiles, system, expected in (
            (["minimal"], "Linux", ["sudo", "apt-get", "install", "-y", "fish", "neovim"]),
            (
                ["server"],
                "Linux",
                ["sudo", "apt-get", "install", "-y", "fish", "neovim", "direnv", "tmux"],
            ),
            (["workstation"], "Darwin", ["bash", str(ROOT / "config/homebrew/install.sh")]),
        ):
            with mock.patch("subprocess.run") as execute, contextlib.redirect_stdout(io.StringIO()):
                INSTALL["packages"](profiles, system, False)
            self.assertEqual([call.args[0] for call in execute.call_args_list], [expected])
        with mock.patch("pathlib.Path.exists", return_value=False), mock.patch(
            "subprocess.run"
        ) as execute, contextlib.redirect_stdout(io.StringIO()):
            INSTALL["submodules"](["server", "agents"], False)
        self.assertEqual(execute.call_count, 1)
        self.assertEqual(execute.call_args.args[0][-1], "modules/dotbot")

    def test_workstation_is_rejected_on_linux(self):
        with mock.patch("platform.system", return_value="Linux"), mock.patch(
            "sys.argv", ["install", "--profile", "workstation", "--dry-run"]
        ), contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as error:
            INSTALL["main"]()
        self.assertEqual(error.exception.code, 2)

    def test_homebrew_uses_root_brewfile_without_upgrades(self):
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory)
            brew = home / "brew"
            brew.write_text('#!/bin/sh\nprintf "%s\\n" "$@" > "$BREW_TEST_LOG"\n')
            brew.chmod(0o755)
            log = home / "arguments"
            subprocess.run(
                ["bash", str(ROOT / "config/homebrew/install.sh")],
                env={
                    **os.environ,
                    "HOME": directory,
                    "PATH": f"{directory}:/usr/bin:/bin",
                    "DOTFILES_BREWFILE": "",
                    "BREW_TEST_LOG": str(log),
                },
                capture_output=True,
                text=True,
                check=True,
            )
            arguments = log.read_text().splitlines()
            self.assertEqual(arguments[:2], ["bundle", "--no-upgrade"])
            self.assertEqual(
                Path(arguments[2].removeprefix("--file=")).resolve(), ROOT / "Brewfile"
            )


@unittest.skipUnless(
    (ROOT / "modules/dotbot/bin/dotbot").is_file(),
    "initialize modules/dotbot to run isolated integration tests",
)
class InstallTests(unittest.TestCase):
    def install(self, home, *arguments):
        result = subprocess.run(
            [str(ROOT / "install"), *arguments],
            env={**os.environ, "HOME": str(home)},
            cwd="/",
            text=True,
            capture_output=True,
            timeout=60,
            check=False,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return result.stdout

    def test_isolated_installs_and_idempotence(self):
        for profile in ("minimal", "server", "agents", "minimal,agents", "server,agents"):
            with self.subTest(profile=profile), tempfile.TemporaryDirectory() as directory:
                home = Path(directory)
                self.install(home, "--profile", profile, "--dry-run")
                self.assertEqual(snapshot(home), {})
                first = self.install(home, "--profile", profile)
                self.assertNotIn("Changes: 0 links", first)
                before = snapshot(home)
                second = self.install(home, "--profile", profile)
                self.assertIn("Changes: 0 links, 0 profile record", second)
                self.assertEqual(snapshot(home), before)
                saved = json.loads((home / INSTALL["STATE"]).read_text())
                self.assertEqual(saved["profiles"], INSTALL["select_profiles"](profile))

    def test_regular_files_are_skipped_then_backed_up_explicitly(self):
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory)
            target = home / ".config/starship.toml"
            target.parent.mkdir()
            target.write_text("keep local edits\n")
            nvim = home / ".config/nvim"
            nvim.mkdir()
            (nvim / "local.lua").write_text("keep local editor config\n")
            output = self.install(home)
            self.assertIn(
                "~/.config/starship.toml <- starship.toml [skip: existing target]", output
            )
            self.assertEqual(target.read_text(), "keep local edits\n")
            self.assertFalse(list(target.parent.glob("starship.toml.bak.*")))
            self.assertEqual((nvim / "local.lua").read_text(), "keep local editor config\n")
            self.install(home, "--force")
            self.assertTrue(target.is_symlink())
            self.assertTrue(nvim.is_symlink())
            backups = list(nvim.parent.glob("nvim.bak.*"))
            self.assertEqual(len(backups), 1)
            self.assertEqual((backups[0] / "local.lua").read_text(), "keep local editor config\n")
            self.assertEqual(
                [p.read_text() for p in target.parent.glob("starship.toml.bak.*")],
                ["keep local edits\n"],
            )

    def test_conflicting_symlinks_are_preserved(self):
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory)
            original = home / "original"
            original.write_text("local ignore rules\n")
            target = home / ".gitignore_global"
            target.symlink_to(original)
            self.assertIn("skip: existing target", self.install(home))
            self.assertEqual(os.readlink(target), str(original))
            self.install(home, "--force")
            backups = list(home.glob(".gitignore_global.bak.*"))
            self.assertEqual(len(backups), 1)
            self.assertEqual(os.readlink(backups[0]), str(original))
            self.assertEqual(original.read_text(), "local ignore rules\n")

    def test_force_agents_does_not_force_shell_targets(self):
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory)
            for name in (".config/starship.toml", ".claude/settings.json"):
                path = home / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text("local edits\n")
            self.install(home, "--force-agents")
            self.assertFalse((home / ".config/starship.toml").is_symlink())
            self.assertTrue((home / ".claude/settings.json").is_symlink())
            self.assertEqual(len(list((home / ".claude").glob("settings.json.bak.*"))), 1)

    def test_symlinked_parent_is_never_followed(self):
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory) / "home"
            outside = Path(directory) / "outside"
            home.mkdir()
            outside.mkdir()
            (home / ".config").symlink_to(outside)
            output = self.install(home, "--force")
            self.assertIn("skip: unsafe parent", output)
            self.assertEqual(snapshot(outside), {})


if __name__ == "__main__":
    unittest.main()
