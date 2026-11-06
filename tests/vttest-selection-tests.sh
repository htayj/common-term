#!/usr/bin/env bash
# Invoke with bash tests/vttest-selection-tests.sh. Pure module only.
set -euo pipefail
root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
if command -v sbcl >/dev/null 2>&1; then sbcl=$(command -v sbcl)
else sbcl="$root/.build/deps/bin/sbcl"; fi
[[ -x $sbcl ]] || { echo 'SBCL unavailable for pure selection controls' >&2; exit 2; }
owned=$(mktemp -d "${TMPDIR:-/tmp}/vttest-selection.XXXXXX")
mkdir "$owned/home" "$owned/cache" "$owned/config" "$owned/data" "$owned/tmp"
printf 'Pure selection control home: %s\n' "$owned"
timeout 30 env -i PATH="$PATH" HOME="$owned/home" XDG_CACHE_HOME="$owned/cache" \
  XDG_CONFIG_HOME="$owned/config" XDG_DATA_HOME="$owned/data" TMPDIR="$owned/tmp" \
  LC_ALL=C "$sbcl" --noinform --no-sysinit --no-userinit --disable-debugger \
  --script "$root/tests/vttest-selection-tests.lisp"
