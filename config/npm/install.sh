#!/usr/bin/env bash
# Install the global npm CLIs listed in config/npm/package.json ("globals"
# array). Idempotent: only installs packages that aren't already present, so it's
# cheap to run on every ./install — a fresh machine gets the full set, an existing
# one is a near no-op. Versions stay current via topgrade's `node` step.
#
# Resolves the installed Node selected by mise without downloading runtimes.
# Missing prerequisites are skipped; package installation errors remain failures.

set -euo pipefail

dir="$(cd "$(dirname "$0")" && pwd)"
manifest="$dir/package.json"

command -v mise >/dev/null 2>&1 || { echo "npm-globals: mise not found, skipping"; exit 0; }
if ! node_root=$(mise where node 2>/dev/null); then
  echo "npm-globals: no installed mise Node selected; run mise use --global node@24, skipping"
  exit 0
fi
if [[ ! -x "$node_root/bin/node" || ! -x "$node_root/bin/npm" ]]; then
  echo "npm-globals: selected mise Node is missing node/npm, skipping"
  exit 0
fi
export PATH="$node_root/bin:$PATH"
[ -f "$manifest" ] || { echo "npm-globals: $manifest missing, skipping"; exit 0; }

# Read the package names from the "globals" array (bash 3.2 compatible — no mapfile).
pkgs=$(node -e 'for (const p of (require(process.argv[1]).globals||[])) console.log(p)' "$manifest")

missing=""
for pkg in $pkgs; do
  npm ls -g "$pkg" >/dev/null 2>&1 || missing="$missing $pkg"
done

if [ -z "$missing" ]; then
  echo "npm-globals: all present"
else
  echo "npm-globals: installing$missing"
  # shellcheck disable=SC2086
  npm install -g $missing
fi
