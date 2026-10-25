#!/usr/bin/env bash
# Real owner-thread host smoke test on isolated X11 and native Wayland servers.
set -euo pipefail
cd "$(dirname "$0")/.."
root=$PWD
backend=${1:-x11}
case $backend in x11|wayland) ;; *) echo 'Usage: tests/host-native.sh [x11|wayland]' >&2; exit 2;; esac
artifacts="$root/.build/evidence/host-$backend"
mkdir -p "$artifacts"
rm -f "$artifacts"/{ready,keys,rollback,live-redefinition,capture-ready,captured,acceptance,passed}
client='' server=''
cleanup() {
  [[ -z $client ]] || { kill "$client" 2>/dev/null || true; wait "$client" 2>/dev/null || true; }
  if [[ $backend == wayland ]]; then tests/wayland-gate.sh stop "$artifacts/compositor"; fi
  [[ -z $server ]] || { kill "$server" 2>/dev/null || true; wait "$server" 2>/dev/null || true; }
}
trap cleanup EXIT
wait_for() {
  for ((i=0;i<600;i++)); do
    [[ -f $artifacts/$1 ]] && return
    kill -0 "$client" 2>/dev/null || { cat "$artifacts/client.log" >&2; return 1; }
    sleep .05
  done
  echo "Timed out waiting for $1" >&2; cat "$artifacts/client.log" >&2; return 1
}
if [[ $backend == wayland ]]; then
  tests/wayland-gate.sh start "$artifacts/compositor"
  source "$artifacts/compositor/env.sh"
  [[ -z ${DISPLAY+x} ]] || { echo 'DISPLAY must be unset for native Wayland' >&2; exit 1; }
else
  selected=''
  for ((n=100;n<200;n++)); do
    if [[ ! -e /tmp/.X11-unix/X$n && ! -e /tmp/.X$n-lock ]]; then selected=:$n; break; fi
  done
  export DISPLAY=${selected:?No free isolated X11 display}
  unset WAYLAND_DISPLAY
  Xvfb "$DISPLAY" -screen 0 1280x800x24 -nolisten tcp -ac > "$artifacts/xvfb.log" 2>&1 & server=$!
  for ((i=0;i<100;i++)); do [[ -S /tmp/.X11-unix/X${DISPLAY#:} ]] && break; sleep .05; done
  sleep .1
  kill -0 "$server" || { cat "$artifacts/xvfb.log" >&2; exit 1; }
  if ! command -v xdotool >/dev/null; then
    candidates=(/nix/store/*-xdotool-*/bin/xdotool); export XDOTOOL=${candidates[0]}
  else export XDOTOOL=xdotool; fi
fi
export LISPTERM_HOST_BACKEND=$backend LISPTERM_HOST_ARTIFACTS="$artifacts/"
"$root/scripts/sbcl" --script tests/run-host-tests.lisp --native > "$artifacts/client.log" 2>&1 & client=$!
wait_for ready
if [[ $backend == wayland ]]; then
  "$LISPTERM_WTYPE" -s 300 NATIVEKEY
else
  id=$("$XDOTOOL" search --name 'Lispterm native host' | head -1)
  "$XDOTOOL" windowfocus "$id"
  "$XDOTOOL" type --clearmodifiers NATIVEKEY
fi
wait_for keys
wait_for capture-ready
sleep .3
if [[ $backend == wayland ]]; then
  "$LISPTERM_GRIM" "$artifacts/host.png"
else
  # Capture the actual X server image, not a framebuffer-only reconstruction.
  "${LISPTERM_IMPORT:-import}" -window root "$artifacts/host.png"
fi
touch "$artifacts/captured"
wait "$client"; client=''
test -f "$artifacts/passed"
cat "$artifacts/passed"
