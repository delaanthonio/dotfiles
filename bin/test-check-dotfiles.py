"""Regression tests for the read-only checker, using disposable fixtures only."""

import contextlib
import io
import json
from pathlib import Path
import runpy
import tempfile
import unittest


CHECK = runpy.run_path(str(Path(__file__).with_name("check-dotfiles")))


class CheckerTests(unittest.TestCase):
    def test_active_profile_checks_only_selected_links(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            home = root / "home"
            (home / ".config/dotfiles").mkdir(parents=True)
            (home / ".config/dotfiles/profile.json").write_text(json.dumps({
                "managed_by": "dotfiles", "profiles": ["agents"],
            }))
            (root / "source").write_text("data")
            (home / "agent").symlink_to(root / "source")
            (root / "install.conf.yaml").write_text(json.dumps([{"link": {
                "~/agent": {"path": "source", "profiles": ["agents"]},
                "~/shell": {"path": "missing", "profiles": ["minimal"]},
            }}]))
            report = CHECK["Report"]()
            with contextlib.redirect_stdout(io.StringIO()) as output:
                CHECK["check_links"](root, home, report)
            self.assertIn("active profile: agents", output.getvalue())
            self.assertEqual((report.passed, report.failed, report.warned), (2, 0, 0))

    def test_invalid_profile_record_is_reported(self):
        with tempfile.TemporaryDirectory() as directory:
            home = Path(directory)
            state = home / ".config/dotfiles/profile.json"
            state.parent.mkdir(parents=True)
            state.write_text('{"managed_by": "dotfiles", "profiles": ["unknown"]}')
            report = CHECK["Report"]()
            with contextlib.redirect_stdout(io.StringIO()):
                CHECK["check_profile_links"](home, home, report)
            self.assertEqual(report.failed, 1)

    def test_agent_render_matches_builder_format(self):
        expected = CHECK["expected_agent"](
            "---\nname: sample\n---\n\nBody\n",
            "model: sonnet\n", {"sonnet": "vendor/full"},
        )
        self.assertEqual(expected, "---\nmodel: vendor/full\n---\n\nBody\n")

    def test_content_drift_fails_without_writes(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            fixture = {
                "agents/scripts/build-agents.sh":
                    'declare -A AGENT_NAMES=(\n[sample]="sample"\n)\n'
                    'declare -A MODEL_MAP=(\n[sonnet]="vendor/full"\n)\n',
                "agents/opencode-templates/sample.yaml": "model: sonnet\n",
                "claude/agents/sample.md": "---\nname: sample\n---\nBody\n",
                "config/opencode/agents/sample.md": "stale\n",
            }
            for name, content in fixture.items():
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(content)
            before = {p: (p.read_bytes(), p.stat().st_mtime_ns)
                      for p in root.rglob("*") if p.is_file()}
            report = CHECK["Report"]()
            with contextlib.redirect_stdout(io.StringIO()):
                CHECK["check_agents"](root, report)
            self.assertEqual(report.failed, 1)
            self.assertEqual(before, {p: (p.read_bytes(), p.stat().st_mtime_ns)
                                     for p in root.rglob("*") if p.is_file()})
            (root / "config/opencode/agents/sample.md").write_text(
                "---\nmodel: vendor/full\n---\nBody\n"
            )
            report = CHECK["Report"]()
            with contextlib.redirect_stdout(io.StringIO()):
                CHECK["check_agents"](root, report)
            self.assertEqual((report.passed, report.failed), (1, 0))

    @unittest.skipIf(CHECK["yaml"] is None, "PyYAML required")
    def test_bad_syntax_is_reported_without_exposing_contents(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "bad.json").write_text('{"private-value":')
            (root / "bad.yaml").write_text("private-value: [")
            (root / "good.json").write_text("{}")
            report = CHECK["Report"]()
            output = io.StringIO()
            with contextlib.redirect_stdout(output):
                CHECK["check_files"](root, report)
            self.assertEqual((report.passed, report.failed), (1, 2))
            self.assertNotIn("private-value", output.getvalue())

    def test_shell_validation_does_not_execute_script(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            marker = root / "must-not-exist"
            (root / "safe.sh").write_text(f'#!/bin/bash\ntouch "{marker}"\n')
            (root / "broken.sh").write_text("#!/bin/bash\nif then\n")
            report = CHECK["Report"]()
            with contextlib.redirect_stdout(io.StringIO()):
                CHECK["check_files"](root, report)
            self.assertGreaterEqual(report.failed, 1)
            self.assertFalse(marker.exists())

    @unittest.skipIf(CHECK["yaml"] is None, "PyYAML required")
    def test_links_and_conditions_are_read_only(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            home = root / "home"
            home.mkdir()
            (root / "source").write_text("data")
            (home / "good").symlink_to(root / "source")
            (home / "broken").symlink_to(root / "absent")
            (root / "install.conf.yaml").write_text(
                "- link:\n"
                "    ~/good: source\n"
                "    ~/broken: source\n"
                "    ~/missing: source\n"
                "    ~/conditional:\n"
                "      path: source\n"
                f"      if: 'touch {root}/must-not-exist'\n"
            )
            report = CHECK["Report"]()
            with contextlib.redirect_stdout(io.StringIO()):
                CHECK["check_links"](root, home, report)
            self.assertEqual((report.passed, report.failed, report.warned), (1, 1, 2))
            self.assertFalse((root / "must-not-exist").exists())
            self.assertFalse((home / "missing").exists())

    def test_scan_excludes_dependencies_and_symlinks(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "modules").mkdir()
            (root / "modules/ignored.json").write_text("invalid")
            (root / "owned.json").write_text("{}")
            (root / "linked.json").symlink_to(root / "owned.json")
            self.assertEqual(list(CHECK["files"](root)), [root / "owned.json"])


if __name__ == "__main__":
    unittest.main()
