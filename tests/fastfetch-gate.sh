#!/usr/bin/env bash
# Capture only a new Lispterm host on a private display, with no parent TTY.
set -euo pipefail
cd "$(dirname "$0")/.."
root=$PWD
backend=${1:-}
[[ $backend == x11 || $backend == wayland ]] || { echo 'Usage: tests/fastfetch-gate.sh x11|wayland' >&2; exit 2; }
if [[ -z ${FASTFETCH:-} && -f .git/autolith-builds/env/setup.sh ]]; then
  source .git/autolith-builds/env/setup.sh
fi
FASTFETCH=${FASTFETCH:-$(command -v fastfetch)}
[[ -x $FASTFETCH ]]
if [[ ! -f ${LISPTERM_FONT:-} ]]; then
  fonts=(/nix/store/*-terminus-font-*/share/fonts/terminus/ter-u16n.otb)
  export LISPTERM_FONT=${fonts[0]}
fi
[[ -f $LISPTERM_FONT ]] || { echo 'Missing Terminus 16px font' >&2; exit 1; }
art=$(mktemp -d "$root/.git/fastfetch-${backend}.XXXXXX")
# Keep the original acceptance evidence immutable across fresh native runs.
originals=(evidence/fastfetch-x11.png evidence/fastfetch-wayland.png evidence/fastfetch.org)
for original in "${originals[@]}"; do
  if [[ -f $original ]]; then sha256sum "$original"; fi
done > "$art/original-evidence.sha256"
export LISPTERM_FASTFETCH_ARTIFACTS="$art/" LISPTERM_FASTFETCH_BACKEND=$backend
export LISPTERM_FASTFETCH_NONCE="FF-${backend}-$(date +%s)-$$"
export LISPTERM_FASTFETCH_BASH=${BASH}
export FASTFETCH
client='' server=''
cleanup() {
  [[ -z $client ]] || kill "$client" 2>/dev/null || true
  if [[ $backend == wayland ]]; then tests/wayland-gate.sh stop "$art/compositor"; fi
  [[ -z $server ]] || { kill "$server" 2>/dev/null || true; wait "$server" 2>/dev/null || true; }
}
trap cleanup EXIT
cat > "$art/child.sh" <<'CHILD'
set -uo pipefail
export TERM=xterm-256color COLORTERM=truecolor LANG=C.UTF-8
printf 'Lispterm %s | Fastfetch | %s\r\n\n' "$LISPTERM_FASTFETCH_BACKEND" "$LISPTERM_FASTFETCH_NONCE"
wrapper_pid=$BASHPID
printf 'wrapper_pid=%s\n' "$wrapper_pid" > "$LISPTERM_FASTFETCH_ARTIFACTS/process.txt"
for fd in 0 1 2; do printf 'fd%s=%s\n' "$fd" "$(readlink /proc/$wrapper_pid/fd/$fd)" >> "$LISPTERM_FASTFETCH_ARTIFACTS/process.txt"; done
stty size < /dev/tty >> "$LISPTERM_FASTFETCH_ARTIFACTS/process.txt"
"$FASTFETCH" --config none --logo nixos --pipe false --structure OS:Kernel:CPU:Memory:Swap:Uptime:Colors < /dev/tty &
ff=$!
printf 'fastfetch_pid=%s\n' "$ff" >> "$LISPTERM_FASTFETCH_ARTIFACTS/process.txt"
wait "$ff"; code=$?
printf '\nFASTFETCH_EXIT=%s %s\r\n' "$code" "$LISPTERM_FASTFETCH_NONCE"
printf '%s\n' "$code" > "$LISPTERM_FASTFETCH_ARTIFACTS/fastfetch.exit"
exit "$code"
CHILD
"$FASTFETCH" --version > "$art/version.txt"
sha256sum "$FASTFETCH" > "$art/binary.sha256"
if [[ $backend == x11 ]]; then
  unset WAYLAND_DISPLAY SWAYSOCK
  for ((n=110;n<210;n++)); do
    [[ -e /tmp/.X11-unix/X$n || -e /tmp/.X$n-lock ]] && continue
    export DISPLAY=:$n
    Xvfb "$DISPLAY" -screen 0 1280x800x24 -nolisten tcp > "$art/xvfb.log" 2>&1 & server=$!
    for ((i=0;i<100;i++)); do [[ -S /tmp/.X11-unix/X$n ]] && break; kill -0 "$server"; sleep .05; done
    [[ -S /tmp/.X11-unix/X$n ]] && break
    echo 'Xvfb startup failed' >&2; exit 1
  done
  [[ -n $server ]] || { echo 'No free isolated X display' >&2; exit 1; }
else
  tests/wayland-gate.sh start "$art/compositor"
  source "$art/compositor/env.sh"
  [[ -z ${DISPLAY+x} ]]
fi
export LISPTERM_SBCL=${LISPTERM_SBCL:-$root/.build/deps/bin/sbcl}
"$root/scripts/sbcl" --script tests/fastfetch-gate.lisp < /dev/null > "$art/client.log" 2>&1 & client=$!
for ((i=0;i<900;i++)); do
  [[ -f $art/ready ]] && break
  kill -0 "$client" 2>/dev/null || { cat "$art/client.log" >&2; exit 1; }
  sleep .05
done
[[ -f $art/ready ]] || { cat "$art/client.log" >&2; echo 'Capture startup timeout' >&2; exit 1; }
if [[ $backend == x11 ]]; then
  xdotool_bin=$(command -v xdotool || true)
  if [[ -z $xdotool_bin ]]; then candidates=(/nix/store/*xdotool-*/bin/xdotool); xdotool_bin=${candidates[0]}; fi
  window=$("$xdotool_bin" search --name '^Lispterm x11 Fastfetch$' | head -1)
  [[ -n $window ]]
  printf 'DISPLAY=%s\nwindow=%s\n' "$DISPLAY" "$window" > "$art/transport.txt"
  import -display "$DISPLAY" -window "$window" "$art/original.png"
else
  "$LISPTERM_SWAYMSG" -t get_tree > "$art/tree.json"
  "$LISPTERM_SWAYMSG" -t get_outputs > "$art/outputs.json"
  printf 'DISPLAY=unset\nWAYLAND_DISPLAY=%s\n' "$WAYLAND_DISPLAY" > "$art/transport.txt"
  "$LISPTERM_GRIM" "$art/original.png"
fi
touch "$art/captured"
wait "$client"; client=''
[[ $(cat "$art/fastfetch.exit") == 0 ]]
grep -Fq "$LISPTERM_FASTFETCH_NONCE" "$art/grid.txt"
grep -Fq "$LISPTERM_FASTFETCH_NONCE" "$art/transcript.raw"
grep -Fq "FASTFETCH_EXIT=0 $LISPTERM_FASTFETCH_NONCE" "$art/transcript.raw"
# The fresh capture belongs only to this ignored per-run artifact directory.
if [[ -s $art/original-evidence.sha256 ]]; then
  sha256sum --check "$art/original-evidence.sha256"
fi
sha256sum "$art/transcript.raw" "$art/grid.txt" "$art/original.png" > "$art/provenance.sha256"
printf '%s\n' "$art" > ".git/fastfetch-${backend}-latest"
echo "Fastfetch $backend passed: $art"
cat "$art/host.sexp"
