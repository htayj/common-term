#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
root=$PWD
artifact_root=$root/.build/x11
mkdir -p "$artifact_root"
artifacts=${LISPTERM_X11_ARTIFACTS:-$(mktemp -d "$artifact_root/run.XXXXXX")}
mkdir -p "$artifacts"
artifacts=$(cd "$artifacts" && pwd)
if [[ -f evidence/display-smoke.png ]]; then
 sha256sum evidence/display-smoke.png > "$artifacts/original-evidence.sha256"
fi
for n in $(seq 100 199); do
 if [[ ! -e /tmp/.X11-unix/X$n && ! -e /tmp/.X$n-lock ]]; then
  export LISPTERM_TEST_DISPLAY=:$n
  break
 fi
done
: "${LISPTERM_TEST_DISPLAY:?No free test display}"
if ! command -v xdotool >/dev/null; then
 export XDOTOOL="$(nix build --no-link --print-out-paths nixpkgs#xdotool)/bin/xdotool"
fi
for depth in ${LISPTERM_TEST_DEPTH:-16 24}; do
 LISPTERM_X11_CAPTURE="$artifacts/depth-$depth.png" LISPTERM_TEST_DEPTH="$depth" \
  scripts/sbcl --non-interactive --load tests/x11-gate.lisp
done
if [[ -s $artifacts/original-evidence.sha256 ]]; then
 sha256sum --check "$artifacts/original-evidence.sha256"
fi
printf '%s\n' "$artifacts" > "$artifact_root/latest"
printf 'X11 artifacts: %s\n' "$artifacts"
