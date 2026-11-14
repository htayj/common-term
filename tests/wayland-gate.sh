#!/usr/bin/env bash
# Reusable native, isolated headless compositor. Never connects to the user's seat.
set -euo pipefail
cd "$(dirname "$0")/.."
root=$PWD
LISPTERM_VTTEST_FUNCTIONS_ONLY=1 source tests/vttest-gate.sh
resolve() {
  local name=$1 pattern=$2 found
  if found=$(command -v "$name"); then printf '%s\n' "$found"; return; fi
  local candidates=($pattern)
  [[ -x ${candidates[0]} ]] || { echo "Missing $name; install Sway, wtype and grim" >&2; return 1; }
  printf '%s\n' "${candidates[0]}"
}
SWAY=${LISPTERM_SWAY:-$(resolve sway '/nix/store/*sway-unwrapped-*/bin/sway')}
SWAYMSG=${LISPTERM_SWAYMSG:-$(resolve swaymsg '/nix/store/*sway-unwrapped-*/bin/swaymsg')}
WTYPE=${LISPTERM_WTYPE:-$(resolve wtype '/nix/store/*wtype-*/bin/wtype')}
GRIM=${LISPTERM_GRIM:-$(resolve grim '/nix/store/*grim-*/bin/grim')}
if [[ -z ${LISPTERM_XKBCOMMON_LIBRARY:-} ]]; then
  candidates=(/nix/store/*libxkbcommon-*/lib/libxkbcommon.so.0)
  [[ ! -f ${candidates[0]} ]] || export LISPTERM_XKBCOMMON_LIBRARY=${candidates[0]}
fi
artifacts=${LISPTERM_WAYLAND_ARTIFACTS:-$root/.build/evidence/wayland}
mkdir -p "$artifacts"; artifacts=$(realpath "$artifacts")
export LISPTERM_WAYLAND_ARTIFACTS="$artifacts/"
start() {
  local dir=$1 runtime pid socket scale=${2:-1}
  fixture_scale wayland "$scale" || { echo "Wayland scale must be 1|2" >&2; return 2; }
  mkdir -p "$dir" || return 1
  if [[ -f $dir/runtime-path ]]; then runtime=$(cat "$dir/runtime-path"); else
    runtime=$(mktemp -d /tmp/lispterm-wayland.XXXXXX) || return 1
    chmod 700 "$runtime" && printf '%s\n' "$runtime" > "$dir/runtime-path" || return 1
  fi
  private_runtime "$runtime" || { echo 'Unsafe runtime directory' >&2; exit 1; }
  cat > "$dir/sway.conf" <<CONFIG || return 1
xwayland disable
output HEADLESS-1 mode $((1280 * scale))x$((800 * scale)) scale $scale
seat seat0 fallback true
font monospace 12
default_border none
focus_follows_mouse no
CONFIG
  clear_session_endpoints
  env -u DISPLAY -u WAYLAND_DISPLAY -u WAYLAND_SOCKET -u SWAYSOCK -u HYPRLAND_INSTANCE_SIGNATURE \
    -u DBUS_SESSION_BUS_ADDRESS XDG_RUNTIME_DIR="$runtime" WLR_BACKENDS=headless \
    WLR_HEADLESS_OUTPUTS=1 WLR_RENDERER=pixman WLR_LIBINPUT_NO_DEVICES=1 \
    "$SWAY" --config "$dir/sway.conf" > "$dir/compositor.log" 2>&1 & pid=$!
  remember "$pid"
  echo "$pid" > "$dir/compositor.pid"
  printf '%s\n' "${owned[$pid]}" > "$dir/compositor.start"
  for ((i=0;i<100;i++)); do
    socket="$runtime/sway-ipc.$UID.$pid.sock"
    if [[ -S $socket && -S $runtime/wayland-1 ]]; then break; fi
    kill -0 "$pid" 2>/dev/null || { cat "$dir/compositor.log" >&2; return 1; }
    sleep .05
  done
  [[ -S $socket && -S $runtime/wayland-1 ]] || { echo 'Sway startup timed out' >&2; return 1; }
  is_owned "$pid" && grep -zFxq -- "$dir/sway.conf" "/proc/$pid/cmdline" || return 1
  socket_inode_match "$pid" "$runtime/wayland-1" > "$dir/isolation.txt" &&
    socket_inode_match "$pid" "$socket" >> "$dir/isolation.txt" || return 1
  XDG_RUNTIME_DIR="$runtime" WAYLAND_DISPLAY=wayland-1 SWAYSOCK="$socket" \
    timeout 5 "$SWAYMSG" -t get_outputs > "$dir/outputs.json" || return 1
  fixture_outputs "$dir/outputs.json" || return 1
  printf 'fixture-scale=%s\n' "$scale" >> "$dir/isolation.txt"
  {
    printf 'for key in ${!DBUS_@}; do unset "$key"; done\n'
    printf 'unset DISPLAY WAYLAND_DISPLAY WAYLAND_SOCKET SWAYSOCK XAUTHORITY HYPRLAND_INSTANCE_SIGNATURE\n'
    printf 'export XDG_RUNTIME_DIR=%q\nexport WAYLAND_DISPLAY=wayland-1\nexport SWAYSOCK=%q\n' "$runtime" "$socket"
    printf 'export LISPTERM_XKBCOMMON_LIBRARY=%q\n' "${LISPTERM_XKBCOMMON_LIBRARY:-}"
    printf 'export LISPTERM_SWAYMSG=%q\nexport LISPTERM_WTYPE=%q\nexport LISPTERM_GRIM=%q\n' "$SWAYMSG" "$WTYPE" "$GRIM"
  } > "$dir/env.sh"
}
stop() {
  local dir=$1 pid
  if [[ -f $dir/compositor.pid ]]; then
    pid=$(cat "$dir/compositor.pid")
    # Only terminate the recorded process when its config identifies this harness.
    if [[ -r $dir/compositor.start && $(process_token "$pid") == "$(cat "$dir/compositor.start")" ]] && grep -zFxq -- "$dir/sway.conf" "/proc/$pid/cmdline"; then
      kill "$pid" 2>/dev/null || true
      for ((i=0;i<100;i++)); do [[ ! -e /proc/$pid ]] && break; sleep .02; done
    fi
    rm -f "$dir/compositor.pid"
  fi
}
wait_for() {
  local file=$1
  for ((i=0;i<300;i++)); do [[ -f $file ]] && return; sleep .05; done
  echo "Timed out waiting for $file" >&2; cat "$artifacts/client.log" >&2; return 1
}
[[ ${1:-run} == stop ]] || fixture_lock
case ${1:-run} in
  start) dir=$(realpath -m "${2:-$artifacts}");
    trap 'code=$?; if ((code)); then stop "$dir"; fi' EXIT
    start "$dir" "${3:-${LISPTERM_VTTEST_SCALE:-1}}"; exit;;
  stop) stop "$(realpath -m "${2:-$artifacts}")"; exit;;
  run) ;;
  *) echo 'Usage: tests/wayland-gate.sh [run | start DIR [SCALE] | stop DIR]' >&2; exit 2;;
esac
rm -f "$artifacts"/{ready,keys,resized,scale2,capture-requested,capture-ready,disconnect-requested,connection-lost,reopen-requested,passed}
client=''
art=$artifacts backend=wayland scale=1
trap 'stop_owned; stop "$artifacts"' EXIT
start "$artifacts"
# shellcheck disable=SC1090
source "$artifacts/env.sh"
"$root/scripts/sbcl" --script tests/run-wayland-tests.lisp > "$artifacts/client.log" 2>&1 & client=$!
remember "$client"
audit_client_tree || { stop_owned; exit 1; }
wait_for "$artifacts/ready"
# wtype creates a real zwp_virtual_keyboard_v1 on this compositor, not synthetic client events.
"$WTYPE" -s 300 -k a -k Up -M ctrl -k c -m ctrl -P b -s 900 -p b
wait_for "$artifacts/keys"
"$SWAYMSG" output HEADLESS-1 mode 1024x768 >/dev/null
wait_for "$artifacts/resized"
"$SWAYMSG" output HEADLESS-1 mode 1600x960 scale 2 >/dev/null
wait_for "$artifacts/scale2"
touch "$artifacts/capture-requested"
wait_for "$artifacts/capture-ready"
sleep .3
"$GRIM" "$artifacts/native-wayland.png"
"$SWAYMSG" -t get_outputs > "$artifacts/outputs.json"
touch "$artifacts/disconnect-requested"
stop "$artifacts"
wait_for "$artifacts/connection-lost"
start "$artifacts"
touch "$artifacts/reopen-requested"
wait "$client"; client=''
test -f "$artifacts/passed"
cat "$artifacts/passed"
echo 'Wayland gate: passed native input/resize/scale/disconnect/reopen/screenshot, DISPLAY unset'
