"""Regression tests for the read-only checker, using disposable fixtures only."""

import contextlib
import io
from pathlib import Path
import runpy
import tempfile
import unittest


CHECK = runpy.run_path(str(Path(__file__).with_name("check-dotfiles")))


class CheckerTests(unittest.TestCase):
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
