#!/usr/bin/env bash
# A fresh display and HOME/state tree. No input is sent to a user's desktop.
set -euo pipefail
cd "$(dirname "$0")/.."
root=$PWD
backend=${1:-wayland}
[[ $backend == wayland || $backend == x11 ]] || { echo 'Expected wayland or x11' >&2; exit 2; }
artifact_root=$root/.build/autolith/$backend
mkdir -p "$artifact_root"
artifacts=${LISPTERM_AUTOLITH_ARTIFACTS:-$(mktemp -d "$artifact_root/run.XXXXXX")}
mkdir -p "$artifacts"
artifacts=$(cd "$artifacts" && pwd)
run=$artifacts
# Fresh native captures never replace the original acceptance evidence.
originals=(evidence/autolith-in-lispterm.png evidence/autolith-wayland.png evidence/autolith-x11.png evidence/autolith.org)
for original in "${originals[@]}"; do
 if [[ -f $original ]]; then sha256sum "$original"; fi
done > "$artifacts/original-evidence.sha256"
mkdir -p "$run"/{home,config/autolith,data,state,cache}
# Skip onboarding only in this disposable configuration; exercise the real prompt.
cat > "$run/config/autolith/init.lisp" <<'LISP'
(setf (config :onboarding-p) nil (config :boot-screen-p) nil
      (config :boot-screen-linger-p) nil (config :boot-screen-seconds) 0
      (config :boot-screen-tip-seconds) 1)
(load "tests/autolith-probe.lisp")
LISP
rm -f "$artifacts"/{ready,resized,capture-ready,captured,passed,failure,inside-runtime.sexp,active-image.sexp,live-renderer.sexp}
client=''; server=''
cleanup() {
 if [[ -n $client ]]; then
  pkill -TERM -P "$client" 2>/dev/null || true
  kill "$client" 2>/dev/null || true
 fi
 [[ -z $server ]] || kill "$server" 2>/dev/null || true
 if [[ $backend == wayland ]]; then tests/wayland-gate.sh stop "$artifacts/compositor"; fi
}
trap cleanup EXIT
if [[ $backend == wayland ]]; then
 tests/wayland-gate.sh start "$artifacts/compositor"
 source "$artifacts/compositor/env.sh"
else
 unset WAYLAND_DISPLAY SWAYSOCK
 for number in $(seq 210 260); do
  if [[ ! -e /tmp/.X11-unix/X$number && ! -e /tmp/.X$number-lock ]]; then export DISPLAY=:$number; break; fi
 done
 : "${DISPLAY:?No unused X display}"
 Xvfb "$DISPLAY" -screen 0 1280x800x24 -nolisten tcp > "$artifacts/xvfb.log" 2>&1 & server=$!
 for ((i=0;i<100;i++)); do [[ -S /tmp/.X11-unix/X${DISPLAY#:} ]] && break; sleep .05; done
 candidates=(/nix/store/*xdotool-*/bin/xdotool)
 XDOTOOL=${LISPTERM_XDOTOOL:-${candidates[0]}}
 [[ -x $XDOTOOL ]] || { echo 'xdotool missing' >&2; exit 1; }
fi
export HOME=$run/home XDG_CONFIG_HOME=$run/config XDG_DATA_HOME=$run/data
export XDG_STATE_HOME=$run/state XDG_CACHE_HOME=$run/cache
export AUTOLITH_ASDF_CACHE=$run/cache/autolith
export LISPTERM_AUTOLITH_ARTIFACTS=$artifacts/ LISPTERM_TEST_BACKEND=$backend
unset AUTOLITH_SITE_CONFIG_ROOT AUTOLITH_SESSION_STYLE AUTOLITH_ACTIVE_CORE
# No pre-existing terminal, no Autolith child process: MAIN runs in this SBCL.
bin/lispterm --autolith --backend "$backend" --title 'Lispterm Autolith evidence' \
 --rows 30 --columns 100 \
 < /dev/null > "$artifacts/client.log" 2>&1 & client=$!
wait_for() {
 for ((i=0;i<2200;i++)); do
  [[ -f $artifacts/$1 ]] && return
  if [[ -f $artifacts/failure ]] || ! kill -0 "$client" 2>/dev/null; then
   cat "$artifacts/failure" "$artifacts/screen.txt" 2>/dev/null || true
   tail -70 "$artifacts/client.log"; return 1
  fi
  sleep .05
 done
 echo "Timeout waiting for $1" >&2; tail -70 "$artifacts/client.log"; return 1
}
wait_for ready
if [[ $backend == wayland ]]; then
 "$LISPTERM_SWAYMSG" output HEADLESS-1 mode 1120x720 >/dev/null
else
 # Autolith updates WM_NAME through OSC, so select the sole visible test window.
 window=$("$XDOTOOL" search --onlyvisible --name . | head -1)
 "$XDOTOOL" windowsize "$window" 1120 720
fi
touch "$artifacts/resized"
wait_for capture-ready
if [[ $backend == wayland ]]; then
 "$LISPTERM_GRIM" "$artifacts/autolith-in-lispterm.png"
 cp "$artifacts/autolith-in-lispterm.png" "$artifacts/autolith-wayland.png"
else
 import -display "$DISPLAY" -window root "$artifacts/autolith-x11.png"
fi
touch "$artifacts/captured"
wait_for passed
wait "$client"; client=''
cat "$artifacts/inside-runtime.sexp" "$artifacts/active-image.sexp" "$artifacts/after-resize.sexp" "$artifacts/live-renderer.sexp"
if [[ -s $artifacts/original-evidence.sha256 ]]; then
 sha256sum --check "$artifacts/original-evidence.sha256"
fi
printf '%s\n' "$artifacts" > "$artifact_root/latest"
printf 'Autolith artifacts: %s\n' "$artifacts"
printf '\nAUTOLITH %s SAME-HEAP GATE PASS\n' "$backend"
