#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
exec </dev/null
unset DISPLAY XAUTHORITY WAYLAND_DISPLAY WAYLAND_SOCKET XDG_RUNTIME_DIR
export LC_ALL=C.UTF-8
for tool in Xvfb xdotool; do
 command -v "$tool" >/dev/null || { echo "x11-deccolm prerequisite: missing $tool" >&2; exit 1; }
done
mkdir -p .build/x11-deccolm
artifacts=$(mktemp -d "$PWD/.build/x11-deccolm/run.XXXXXX")
printf 'X11 DECCOLM artifacts: %s\n' "$artifacts"
exec scripts/sbcl --script tests/x11-deccolm-gate.lisp "$artifacts/"
