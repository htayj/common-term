#!/usr/bin/env bash
# Recorder v2: private fixtures, bounded ownership-based cleanup, explicit verdict mode.
set -euo pipefail
cd "$(dirname "$0")/.."
root=$PWD
# These functions are also sourced by the disposable non-GUI cleanup test.
declare -A owned=()
process_token() {
  local stat rest
  [[ $1 =~ ^[0-9]+$ && -r /proc/$1/stat ]] || return 1
  stat=$(cat "/proc/$1/stat") || return 1
  rest=${stat##*) }; read -ra fields <<< "$rest"
  [[ ${fields[0]} != Z ]] || return 1
  printf '%s\n' "${fields[19]}"
}
remember() { local token; token=$(process_token "$1") || return 0; owned[$1]=$token; }
is_owned() { [[ -v owned[$1] ]] && [[ $(process_token "$1" 2>/dev/null) == "${owned[$1]}" ]]; }
collect_tree() {
  local pid=$1 child
  is_owned "$pid" || return 0
  while read -r child; do
    [[ -n $child ]] || continue
    remember "$child"; collect_tree "$child"
  done < <(ps -o pid= --ppid "$pid" 2>/dev/null)
}
stop_owned() {
  local pid i alive
  # Snapshot while roots are alive, before any parent can orphan its children.
  for pid in "${!owned[@]}"; do collect_tree "$pid"; done
  for pid in "${!owned[@]}"; do
    is_owned "$pid" || continue
    printf 'TERM %s %s\n' "$pid" "${owned[$pid]}" >> "$art/cleanup.log"
    kill -TERM "$pid" 2>/dev/null || true
  done
  for ((i=0;i<50;i++)); do
    alive=0; for pid in "${!owned[@]}"; do is_owned "$pid" && alive=1; done
    ((alive)) || break; sleep .04
  done
  for pid in "${!owned[@]}"; do
    is_owned "$pid" || continue
    printf 'KILL %s %s\n' "$pid" "${owned[$pid]}" >> "$art/cleanup.log"
    kill -KILL "$pid" 2>/dev/null || true
  done
  for ((i=0;i<50;i++)); do
    alive=0; for pid in "${!owned[@]}"; do is_owned "$pid" && alive=1; done
    ((alive)) || break; sleep .04
  done
  for pid in "${!owned[@]}"; do
    if is_owned "$pid"; then printf 'SURVIVED %s\n' "$pid" >> "$art/cleanup.log"; return 1; fi
  done
  printf 'all-owned-stopped\n' >> "$art/cleanup.log"
}
watchdog_check() { ((SECONDS < $1)) || { echo "vttest total watchdog expired" >&2; return 124; }; }
if [[ ${LISPTERM_VTTEST_FUNCTIONS_ONLY:-} == 1 ]]; then return 0; fi
backend=${1:-}
[[ $backend == x11 || $backend == wayland ]] || { echo 'Usage: tests/vttest-gate.sh x11|wayland' >&2; exit 2; }
mode=${LISPTERM_VTTEST_MODE:-record}
[[ $mode == record || $mode == accept ]] || { echo 'Mode must be record or accept' >&2; exit 2; }
export LISPTERM_VTTEST_MODE=$mode
export PATH="$root/.build/test-tools/bin:$PATH"
export LISPTERM_SBCL=${LISPTERM_SBCL:-$root/.build/deps/bin/sbcl}
pinned="$root/.build/test-tools/bin/vttest"
export VTTEST=${VTTEST:-$pinned}
[[ -x $VTTEST ]]
if [[ $mode == accept ]]; then
  [[ $(realpath "$VTTEST") == "$(realpath "$pinned")" ]] || { echo 'Acceptance requires pinned vttest' >&2; exit 1; }
  [[ $(sha256sum "$VTTEST" | cut -d' ' -f1) == 622bc81d4e2001824ae7012c234294072e0738834800d6642670237adcf2a2a9 ]] || {
    echo 'Acceptance binary hash differs from reviewed pinned x86_64 build' >&2; exit 1;
  }
fi
if [[ ! -f ${LISPTERM_FONT:-} ]]; then
  fonts=(/nix/store/*-terminus-font-*/share/fonts/terminus/ter-u16n.otb)
  export LISPTERM_FONT=${fonts[0]}
fi
mkdir -p "$root/.build/evidence"
exec 9> "$root/.build/vttest-gui.lock"
flock -w 120 9
art=$(mktemp -d "$root/.build/evidence/vttest-${backend}-v2.XXXXXX")
export LISPTERM_VTTEST_ARTIFACTS="$art/" LISPTERM_VTTEST_BACKEND=$backend LISPTERM_VTTEST_BASH=$BASH
export LISPTERM_VTTEST_TITLE="Lispterm-vttest-${art##*.}-$$"
client='' server='' keyboard_holder='' runtime=''
cleanup() {
  local code=$? pid token
  trap - EXIT INT TERM
  set +e
  # The compositor helper was launched by this fixture. Its exact private config
  # is a second identity check when startup failed before its PID was adopted.
  if [[ -f $art/compositor/compositor.pid ]]; then
    pid=$(cat "$art/compositor/compositor.pid")
    if [[ $pid =~ ^[0-9]+$ && -r /proc/$pid/cmdline ]] && grep -zFxq -- "$art/compositor/sway.conf" "/proc/$pid/cmdline"; then remember "$pid"; fi
  fi
  : > "$art/process-tree-final.txt"
  for pid in "${!owned[@]}"; do
    is_owned "$pid" && ps -p "$pid" -o pid,ppid,pgid,sid,lstart,args >> "$art/process-tree-final.txt"
  done
  local stopped=1
  stop_owned || { code=1; stopped=0; }
  if [[ $stopped == 1 && -f $art/compositor/runtime-path ]]; then
    runtime=$(cat "$art/compositor/runtime-path")
    if [[ $runtime == /tmp/lispterm-wayland.* && -d $runtime && -O $runtime && ! -L $runtime ]]; then rm -rf -- "$runtime"; fi
  fi
  printf '%s\n' "$code" > "$art/harness.exit"
  # Hash all data and metadata after cleanup, with relative filenames.
  if ! (cd "$art"; find . -type f ! -name provenance.sha256 -print0 | sort -z | xargs -0 sha256sum > provenance.sha256); then
    code=1; printf '%s\n' "$code" > "$art/harness.exit"
  fi
  printf '%s\n' "$art" > "$root/.build/evidence/vttest-${backend}-latest"
  echo "vttest $mode exit=$code: $art"
  exit "$code"
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
cp tests/vttest-gate.sh tests/vttest-gate.lisp tests/vttest-protocol.lisp tests/verify.sh scripts/deps.nix "$art/"
{
  printf 'recorder-version=2\nmode=%s\nfilter=%s\nbackend=%s\ntitle=%s\n' "$mode" "${LISPTERM_VTTEST_FILTER:-all}" "$backend" "$LISPTERM_VTTEST_TITLE"
  printf 'pinned-binary=%s\noverride=%s\n' "$(realpath "$pinned")" "$([[ $(realpath "$VTTEST") == "$(realpath "$pinned")" ]] && echo false || echo true)"
  printf 'binary=%s\nsbcl=%s\nfont=%s\n' "$(realpath "$VTTEST")" "$(realpath "$LISPTERM_SBCL")" "$LISPTERM_FONT"
  printf 'command='; printf '%q ' "$root/scripts/sbcl" --script tests/vttest-gate.lisp; printf '\n'
  printf 'TERM=vt100 LANG=C child-command=vttest -l LABEL.vttest.log 24x80.132\n'
} > "$art/fixture.txt"
cat > "$art/child.sh" <<'CHILD'
export TERM=vt100 LANG=C
unset COLORTERM
label=$1
printf 'pid=%s\n' "$BASHPID" > "$LISPTERM_VTTEST_ARTIFACTS/$label.process.txt"
cat "/proc/$BASHPID/stat" >> "$LISPTERM_VTTEST_ARTIFACTS/$label.process.txt"
for fd in 0 1 2; do readlink "/proc/$BASHPID/fd/$fd" >> "$LISPTERM_VTTEST_ARTIFACTS/$label.process.txt"; done
stty size < /dev/tty >> "$LISPTERM_VTTEST_ARTIFACTS/$label.process.txt"
"$VTTEST" -l "$LISPTERM_VTTEST_ARTIFACTS/$label.vttest.log" 24x80.132 < /dev/tty
code=$?
printf '%s\n' "$code" > "$LISPTERM_VTTEST_ARTIFACTS/$label.exit"
exit "$code"
CHILD
"$VTTEST" -V > "$art/version.txt"
grep -Fq '(20251205)' "$art/version.txt"
sha256sum "$VTTEST" "$LISPTERM_FONT" > "$art/binary-font.sha256"
git rev-parse HEAD > "$art/run-head.txt"
printf '%s\n' 5b3336f91735ad042ea426ba40a3cfe51f7657e8 > "$art/historical-baseline-head.txt"
git status --porcelain=v1 > "$art/git-status.txt"
git diff --binary HEAD > "$art/working-tree.diff"
# Include all actual source files, including untracked task files, not just HEAD.
find src engine tests scripts -type f -print0 | sort -z | xargs -0 sha256sum > "$art/source.sha256"
for tool in Xvfb xdotool import sway swaymsg wtype grim jq timeout ps; do
  command -v "$tool" >> "$art/dependencies.txt" 2>/dev/null || true
done
if [[ $backend == x11 ]]; then
  unset WAYLAND_DISPLAY SWAYSOCK
  Xvfb -displayfd 3 -screen 0 1280x800x24 -nolisten tcp 3> "$art/display-number" > "$art/xvfb.log" 2>&1 & server=$!
  remember "$server"
  for ((i=0;i<100;i++)); do
    is_owned "$server" || { echo 'Owned Xvfb failed' >&2; exit 1; }
    [[ -s $art/display-number ]] && break; sleep .05
  done
  number=$(cat "$art/display-number"); [[ $number =~ ^[0-9]+$ ]]
  export DISPLAY=:$number
  timeout 10 xdpyinfo -display "$DISPLAY" > "$art/x-display.txt"
  printf 'DISPLAY=%s\nserver=%s\nstart=%s\n' "$DISPLAY" "$server" "${owned[$server]}" > "$art/transport.txt"
else
  timeout 15 tests/wayland-gate.sh start "$art/compositor"
  source "$art/compositor/env.sh"
  server=$(cat "$art/compositor/compositor.pid"); remember "$server"
  [[ -z ${DISPLAY+x} ]]
  # Initial size only. Subsequent capture rectangles follow actual native geometry.
  for rule in 'floating enable' 'resize set 640 384' 'move position 0 0'; do
    timeout 5 "$LISPTERM_SWAYMSG" "for_window [title=\"^$LISPTERM_VTTEST_TITLE\$\"] $rule" >> "$art/window-rule.json"
  done
  printf 'DISPLAY=unset\nWAYLAND_DISPLAY=%s\n' "$WAYLAND_DISPLAY" > "$art/transport.txt"
  "$LISPTERM_WTYPE" -s "$(( (${LISPTERM_VTTEST_TIMEOUT:-1800} + 60) * 1000 ))" > "$art/keyboard-holder.log" 2>&1 & keyboard_holder=$!
  remember "$keyboard_holder"; sleep .3; is_owned "$keyboard_holder"
fi
unset CL_SOURCE_REGISTRY ASDF_OUTPUT_TRANSLATIONS
"$root/scripts/sbcl" --script tests/vttest-gate.lisp < /dev/null > "$art/client.log" 2>&1 & client=$!
remember "$client"
# Strictly one exact-title window on this private display. Native PID must equal
# our client. X11 may omit _NET_WM_PID, so private server + unguessable title is
# the identity fallback; never search an inherited display or choose head -1.
window_identity() {
  if [[ $backend == x11 ]]; then
    mapfile -t windows < <(timeout 5 xdotool search --name "^$LISPTERM_VTTEST_TITLE\$" || true)
    [[ ${#windows[@]} == 1 ]] || { echo 'Expected one owned X11 window' >&2; return 1; }
    window=${windows[0]}
    owner=$(timeout 5 xdotool getwindowpid "$window" 2>/dev/null || true)
    [[ -z $owner || $owner == "$client" ]] || { echo 'X11 PID mismatch' >&2; return 1; }
  else
    timeout 5 "$LISPTERM_SWAYMSG" -t get_tree > "$art/current-tree.json"
    node=$(jq -ce --arg title "$LISPTERM_VTTEST_TITLE" --argjson pid "$client" \
      '[.. | objects | select(.name? == $title and .pid? == $pid and .type? == "floating_con")] | if length == 1 then .[0] else error("ambiguous/missing owned native window") end' "$art/current-tree.json")
    window=$(jq -r '.id' <<< "$node")
  fi
}
end=$((SECONDS + ${LISPTERM_VTTEST_TIMEOUT:-1800}))
while is_owned "$client"; do
  watchdog_check "$end"
  collect_tree "$client"
  if [[ -f $art/capture-request ]]; then
    read -r name expected_width expected_height < "$art/capture-request"
    [[ $name =~ ^[0-9]+-[a-z0-9_-]+$ && $expected_width =~ ^[0-9]+$ && $expected_height =~ ^[0-9]+$ ]]
    window_identity
    if [[ $backend == x11 ]]; then
      timeout 5 xdotool getwindowgeometry --shell "$window" > "$art/$name.geometry.txt"
      width=$(sed -n 's/^WIDTH=//p' "$art/$name.geometry.txt")
      height=$(sed -n 's/^HEIGHT=//p' "$art/$name.geometry.txt")
      timeout 10 import -display "$DISPLAY" -window "$window" "$art/$name.png"
    else
      cp "$art/current-tree.json" "$art/$name.tree.json"
      jq '{id,pid,name,rect,window_rect,geometry}' <<< "$node" > "$art/$name.geometry.json"
      read -r x y width height < <(jq -r '[.rect.x,.rect.y,.rect.width,.rect.height] | @tsv' <<< "$node")
      timeout 10 "$LISPTERM_GRIM" -g "$x,$y ${width}x$height" "$art/$name.png"
    fi
    printf 'expected=%sx%s actual=%sx%s window=%s\n' "$expected_width" "$expected_height" "$width" "$height" "$window" > "$art/$name.dimensions.txt"
    if [[ $mode == accept ]]; then
      [[ $width == "$expected_width" && $height == "$expected_height" ]] || { echo 'Capture geometry mismatch' >&2; exit 1; }
    fi
    timeout 5 identify -format '%w %h\n' "$art/$name.png" > "$art/$name.image-size.txt"
    read -r image_width image_height < "$art/$name.image-size.txt"
    [[ $image_width == "$width" && $image_height == "$height" ]]
    rm "$art/capture-request"; touch "$art/captured"
  fi
  if [[ -f $art/key-request ]]; then
    key=$(cat "$art/key-request")
    [[ $key =~ ^(Up|Down|Left|Right|Tab|KP_1|KP_2|KP_Enter)$ ]]
    window_identity
    printf '%s %s window=%s\n' "$(date +%s.%N)" "$key" "$window" >> "$art/native-keys.txt"
    if [[ $backend == x11 ]]; then
      timeout 5 xdotool windowfocus --sync "$window"
      timeout 5 xdotool key --clearmodifiers "$key"
    else
      timeout 5 "$LISPTERM_SWAYMSG" "[con_id=$window] focus" >> "$art/focus.json"
      timeout 5 "$LISPTERM_WTYPE" -s 300 -k "$key" -s 100
    fi
    rm "$art/key-request"; touch "$art/key-done"
  fi
  sleep .05
done
wait "$client"; client=''
[[ -f $art/finished.sexp ]]
check_mode=measure; [[ $mode != accept ]] || check_mode=accept
scripts/sbcl --script tests/vttest-protocol.lisp --vttest-check "$check_mode" "$art" > "$art/offline-check.log"
echo "Recorder v2 $mode complete (mechanical checks only, visual acceptance separate)"
