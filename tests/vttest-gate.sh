#!/usr/bin/env bash
# Recorder v3: private fixtures, bounded ownership-based cleanup, explicit verdict mode.
set -Eeuo pipefail
cd "$(dirname "$0")/.."
root=$PWD
# These functions are also sourced by the disposable non-GUI cleanup test.
declare -A owned=() audited=()
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
# Shared shell interface for the later native fixture: source with FUNCTIONS_ONLY.
fixture_scale() {
  [[ $2 == 1 || $2 == 2 ]] && [[ $1 == wayland || ( $1 == x11 && $2 == 1 ) ]]
}
fixture_lock() {
  local path=$root/.build/vttest-gui.lock
  if [[ ${LISPTERM_GUI_LOCK_HELD:-} == 1 ]]; then
    # fdinfo describes the lock attached to this open file description, including
    # inherited descriptors. A marker or another process's lock is insufficient.
    [[ /proc/$BASHPID/fd/9 -ef $path ]] && grep -q 'FLOCK *ADVISORY *WRITE' /proc/$BASHPID/fdinfo/9 || return 1
  else
    exec 9> "$path"
    flock -w 120 9 || return 1
    export LISPTERM_GUI_LOCK_HELD=1
  fi
}
clear_session_endpoints() {
  local key
  for key in ${!DBUS_@}; do unset "$key"; done
  unset DISPLAY WAYLAND_DISPLAY SWAYSOCK XAUTHORITY HYPRLAND_INSTANCE_SIGNATURE
}
private_runtime() {
  [[ $1 =~ ^/tmp/lispterm-(x11|wayland)\.[[:alnum:]]+$ && -d $1 && -O $1 && ! -L $1 ]] &&
    [[ $(stat -c %a "$1") == 700 ]]
}
socket_inode_match() {
  # Match kernel socket inodes, not the misleading fd symlink pathname.
  local fd target inode row path=$2 fds=${3:-/proc/$1/fd} table=${4:-/proc/net/unix}
  for fd in "$fds"/*; do
    target=$(readlink "$fd") || continue
    [[ $target =~ ^socket:\[([0-9]+)\]$ ]] || continue
    inode=${BASH_REMATCH[1]}
    row=$(awk -v n="$inode" -v p="$path" '$7==n && $8==p {print $7, $8}' "$table")
    if [[ -n $row ]]; then printf '%s\n' "$row"; return 0; fi
  done
  return 1
}
fixture_outputs() {
  jq -e --argjson scale "$scale" '
    length > 0 and all(.[]; (.name | startswith("HEADLESS-")) and .active == true and
      .scale == $scale and .current_mode.width >= (1280*$scale) and
      .current_mode.height >= (800*$scale))' "$1" > /dev/null
}
fixture_isolation() {
  private_runtime "$XDG_RUNTIME_DIR" && is_owned "$server" || return 1
  [[ ! -v DBUS_SESSION_BUS_ADDRESS && ! -v DBUS_SESSION_BUS_PID && ! -v DBUS_SESSION_BUS_WINDOWID && ! -v XAUTHORITY ]] || return 1
  if [[ $backend == x11 ]]; then
    local number; number=$(cat "$art/display-number")
    [[ $number =~ ^[0-9]+$ && $DISPLAY == :$number && ! -v WAYLAND_DISPLAY && ! -v SWAYSOCK ]] || return 1
    grep -zFxq -- '-displayfd' /proc/"$server"/cmdline || return 1
    socket_inode_match "$server" "/tmp/.X11-unix/X$number" ||
      socket_inode_match "$server" "@/tmp/.X11-unix/X$number" || return 1
  else
    [[ ! -v DISPLAY && $WAYLAND_DISPLAY == wayland-1 && $XDG_RUNTIME_DIR == "$(cat "$art/compositor/runtime-path")" &&
       $SWAYSOCK == "$XDG_RUNTIME_DIR/sway-ipc.$UID.$server.sock" ]] || return 1
    grep -zFxq -- "$art/compositor/sway.conf" /proc/"$server"/cmdline || return 1
    socket_inode_match "$server" "$XDG_RUNTIME_DIR/$WAYLAND_DISPLAY" &&
      socket_inode_match "$server" "$SWAYSOCK" || return 1
    timeout 5 "$LISPTERM_SWAYMSG" -t get_outputs > "$art/isolation-outputs.json" || return 1
    fixture_outputs "$art/isolation-outputs.json" || return 1
  fi
  printf 'backend=%s scale=%s server=%s start=%s runtime=%s\n' "$backend" "$scale" "$server" "${owned[$server]}" "$XDG_RUNTIME_DIR"
}
audit_endpoint_file() {
  local item key value seen_runtime=0 seen_display=0
  while IFS= read -r -d '' item; do
    key=${item%%=*}; value=${item#*=}
    case $key in
      XDG_RUNTIME_DIR) [[ $value == "$XDG_RUNTIME_DIR" ]] || return 1; seen_runtime=1;;
      DISPLAY) [[ $backend == x11 && $value == "$DISPLAY" ]] || return 1; seen_display=1;;
      WAYLAND_DISPLAY) [[ $backend == wayland && $value == "$WAYLAND_DISPLAY" ]] || return 1; seen_display=1;;
      SWAYSOCK) [[ $backend == wayland && $value == "$SWAYSOCK" ]] || return 1;;
      DBUS_*|XAUTHORITY) return 1;;
    esac
  done < "$1"
  [[ $seen_runtime == 1 && $seen_display == 1 ]]
}
# Record locations/status only, never command text or environment values.
fixture_error() {
  printf 'status=%s source=%s line=%s function=%s\n' "$1" "${2##*/}" "$3" "$4" >> "$art/shell-errors.log"
}
fixture_wait_client() {
  local code=0
  wait "$client" || code=$?
  printf 'client-wait pid=%s start=%s status=%s\n' "$client" "${owned[$client]:-unknown}" "$code" >> "$art/client-wait.log"
  return "$code"
}
# Publish once, atomically. An existing file, symlink or directory is historical.
fixture_latest_pointer() {
  local pointer=$1
  printf '%s\n' "$art" > "$art/latest-pointer.txt" || return 1
  ln -T -- "$art/latest-pointer.txt" "$pointer" 2>/dev/null || [[ -e $pointer || -L $pointer ]]
}
audit_client_tree() {
  local pid child
  if ! is_owned "$client"; then
    # Only polling after startup may accept a vanished, previously owned root.
    # A live PID with a different start token is an ownership failure.
    [[ ${1:-} == allow-exit && -v owned[$client] ]] && ! process_token "$client" > /dev/null 2>&1 || return 1
  fi
  collect_tree "$client"
  # Audit the client subtree only, not the server or unrelated owned helpers.
  # Previously audited descendants may still be alive after the root exits.
  local -A clients=(); local -a pending=("$client" "${!audited[@]}")
  while ((${#pending[@]})); do
    pid=${pending[0]}; pending=("${pending[@]:1}")
    is_owned "$pid" || continue
    [[ ! -v clients[$pid] ]] || continue
    collect_tree "$pid"
    clients[$pid]=1
    while read -r child; do [[ -z $child ]] || pending+=("$child"); done < <(ps -o pid= --ppid "$pid")
  done
  for pid in "${!clients[@]}"; do
    if ! audit_endpoint_file "/proc/$pid/environ"; then
      # A vanished descendant has no endpoints left to audit; a live mismatch fails.
      is_owned "$pid" || continue
      printf 'endpoint-mismatch pid=%s\n' "$pid" >> "$art/isolation.txt"; return 1
    fi
    if [[ ${audited[$pid]:-} != "${owned[$pid]}" ]]; then
      printf 'client=%s start=%s endpoints=private\n' "$pid" "${owned[$pid]}" >> "$art/isolation.txt"
      audited[$pid]=${owned[$pid]}
    fi
  done
}
if [[ ${LISPTERM_VTTEST_FUNCTIONS_ONLY:-} == 1 ]]; then return 0; fi
backend=${1:-}
[[ $backend == x11 || $backend == wayland ]] || { echo 'Usage: tests/vttest-gate.sh x11|wayland' >&2; exit 2; }
# LISPTERM_VTTEST_SCALE=1|2 selects Wayland buffer/output scale; X11 accepts 1 only.
scale=${LISPTERM_VTTEST_SCALE:-1}
fixture_scale "$backend" "$scale" || { echo "Scale must be 1 (X11) or 1|2 (Wayland)" >&2; exit 2; }
export LISPTERM_VTTEST_SCALE=$scale
mode=${LISPTERM_VTTEST_MODE:-record}
[[ $mode == record || $mode == accept ]] || { echo 'Mode must be record or accept' >&2; exit 2; }
export LISPTERM_VTTEST_MODE=$mode
export PATH="$root/.build/test-tools/bin:$PATH"
export LISPTERM_SBCL=${LISPTERM_SBCL:-$root/.build/deps/bin/sbcl}
# Validate the actual catalog/helper before fixtures, upstream or native startup.
# This dependency-free process never loads bootstrap.lisp or the application.
"$root/scripts/sbcl" --non-interactive --load tests/vttest-protocol.lisp \
  --load tests/vttest-selection.lisp \
  --eval '(handler-case (vttest-selection:select-cases vttest-check:*case-specs* :exact-labels (sb-ext:posix-getenv "LISPTERM_VTTEST_EXACT_LABELS") :filter (sb-ext:posix-getenv "LISPTERM_VTTEST_FILTER")) (vttest-selection:invalid-selection (c) (format *error-output* "~a~%" c) (sb-ext:exit :code 2)))'
selector_sha=$(sha256sum tests/vttest-selection.lisp | cut -d' ' -f1)
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
fixture_lock || { echo "Invalid inherited GUI lock" >&2; exit 1; }
art=$(mktemp -d "$root/.build/evidence/vttest-${backend}-scale${scale}-v3.XXXXXX")
export LISPTERM_VTTEST_ARTIFACTS="$art/" LISPTERM_VTTEST_BACKEND=$backend LISPTERM_VTTEST_BASH=$BASH
export LISPTERM_VTTEST_TITLE="Lispterm-vttest-${art##*.}-$$"
# The ordinary root is always the actual mktemp result, never the caller hint.
if [[ ${LISPTERM_VTTEST_ON_PHASE:-0} == 1 ]]; then mkdir "$art/on-phase"; fi
client='' server='' keyboard_holder='' runtime=''
cleanup() {
  local code=$? pid token
  trap - EXIT INT TERM ERR
  set +e
  # The compositor helper was launched by this fixture. Its exact private config
  # is a second identity check when startup failed before its PID was adopted.
  if [[ -f $art/compositor/compositor.pid ]]; then
    pid=$(cat "$art/compositor/compositor.pid")
    if [[ $pid =~ ^[0-9]+$ && -r $art/compositor/compositor.start && $(process_token "$pid") == "$(cat "$art/compositor/compositor.start")" ]] && grep -zFxq -- "$art/compositor/sway.conf" "/proc/$pid/cmdline"; then remember "$pid"; fi
  fi
  : > "$art/process-tree-final.txt"
  for pid in "${!owned[@]}"; do
    is_owned "$pid" && ps -p "$pid" -o pid,ppid,pgid,sid,lstart,args >> "$art/process-tree-final.txt"
  done
  local stopped=1
  stop_owned || { code=1; stopped=0; }
  if [[ $stopped == 1 && -f $art/compositor/runtime-path ]]; then
    runtime=$(cat "$art/compositor/runtime-path")
    if private_runtime "$runtime"; then rm -rf -- "$runtime"; fi
  fi
  if [[ $stopped == 1 && $backend == x11 ]] && private_runtime "$runtime"; then rm -rf -- "$runtime"; fi
  fixture_latest_pointer "$root/.build/evidence/vttest-${backend}-latest" || code=1
  printf '%s\n' "$code" > "$art/harness.exit"
  # Hash all data and metadata after cleanup, with relative filenames.
  if ! (cd "$art"; find . -type f ! -name provenance.sha256 -print0 | sort -z | xargs -0 sha256sum > provenance.sha256); then
    code=1; printf '%s\n' "$code" > "$art/harness.exit"
  fi
  echo "vttest $mode exit=$code: $art"
  exit "$code"
}
trap cleanup EXIT
trap 'fixture_error "$?" "${BASH_SOURCE[0]}" "$LINENO" "${FUNCNAME[0]:-main}"' ERR
trap 'exit 130' INT
trap 'exit 143' TERM
cp tests/vttest-gate.sh tests/vttest-gate.lisp tests/vttest-protocol.lisp tests/vttest-selection.lisp tests/verify.sh scripts/deps.nix "$art/"
# Hashing in cleanup covers this actual launch dependency as well as its copy.
if [[ $backend == wayland ]]; then cp tests/wayland-gate.sh "$art/"; fi
# Optional area-driver receipt binds this new mktemp capture to its sole launch.
if [[ -n ${LISPTERM_VTTEST_AREA_REQUEST:-} ]]; then
  [[ $LISPTERM_VTTEST_AREA_REQUEST == /* && -f $LISPTERM_VTTEST_AREA_REQUEST && ! -L $LISPTERM_VTTEST_AREA_REQUEST ]]
  [[ ${LISPTERM_VTTEST_AREA_RECEIPT:-} == /* && ! -e $LISPTERM_VTTEST_AREA_RECEIPT ]]
  cp "$LISPTERM_VTTEST_AREA_REQUEST" "$art/area-request.sexp"
  (set -o noclobber; printf '%s\n' "$art" > "$LISPTERM_VTTEST_AREA_RECEIPT")
  cp tests/vttest-area.lisp tests/vttest-cursor-driver.lisp tests/vttest-cursor-oracle.lisp "$art/"
fi
[[ $(sha256sum "$art/vttest-selection.lisp" | cut -d' ' -f1) == "$selector_sha" ]] || { echo 'Selector changed after preflight' >&2; exit 1; }
if [[ ${LISPTERM_VTTEST_ON_PHASE:-0} == 1 ]]; then
  cp tests/vttest-on-phase.lisp tests/vttest-on-phase-tests.lisp "$art/on-phase/"
fi
{
  printf 'recorder-version=3\nmode=%s\nfilter=%s\nbackend=%s\ntitle=%s\n' "$mode" "${LISPTERM_VTTEST_FILTER:-all}" "$backend" "$LISPTERM_VTTEST_TITLE"
  printf 'exact-labels=%s\nselector-sha256=%s\nselector-snapshot=%s\n' "${LISPTERM_VTTEST_EXACT_LABELS:-}" "$selector_sha" "$art/vttest-selection.lisp"
  printf 'fixture-scale=%s\n' "$scale"
  printf 'on-phase-supplements=%s\n' "${LISPTERM_VTTEST_ON_PHASE:-0}"
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
# Protected transaction sources are stat/hash-only, never copied into diffs.
git diff --binary HEAD -- . ':!tests/capture-transaction.lisp' ':!tests/capture-transaction-tests.lisp' ':!tests/vt100-native-geometry.lisp' ':!tests/vt100-native-geometry.sh' > "$art/working-tree.diff"
# Include all actual source files, including untracked task files, not just HEAD.
find src engine tests scripts -type f ! -path 'tests/vt100-native-geometry.lisp' ! -path 'tests/vt100-native-geometry.sh' ! -path 'tests/capture-transaction.lisp' ! -path 'tests/capture-transaction-tests.lisp' -print0 | sort -z | xargs -0 sha256sum > "$art/source.sha256"
printf '%s\n' 'Unrelated native-geometry and protected transaction sources excluded from diff and source inputs.' > "$art/source-exclusions.txt"
for tool in Xvfb xdotool import sway swaymsg wtype grim jq timeout ps; do
  command -v "$tool" >> "$art/dependencies.txt" 2>/dev/null || true
done
clear_session_endpoints
if [[ $backend == x11 ]]; then
  runtime=$(mktemp -d /tmp/lispterm-x11.XXXXXX)
  chmod 700 "$runtime"
  export XDG_RUNTIME_DIR=$runtime
  unset WAYLAND_DISPLAY SWAYSOCK DISPLAY XAUTHORITY DBUS_SESSION_BUS_ADDRESS DBUS_SESSION_BUS_PID DBUS_SESSION_BUS_WINDOWID
  Xvfb -displayfd 3 -screen 0 1280x800x24 -nolisten tcp 3> "$art/display-number" > "$art/xvfb.log" 2>&1 & server=$!
  remember "$server"
  for ((i=0;i<100;i++)); do
    is_owned "$server" || { echo 'Owned Xvfb failed' >&2; exit 1; }
    [[ -s $art/display-number ]] && break; sleep .05
  done
  number=$(cat "$art/display-number"); [[ $number =~ ^[0-9]+$ ]]
  export DISPLAY=:$number
  fixture_isolation > "$art/isolation.txt" || exit 1
  timeout 10 xdpyinfo -display "$DISPLAY" > "$art/x-display.txt"
  printf 'DISPLAY=%s\nserver=%s\nstart=%s\n' "$DISPLAY" "$server" "${owned[$server]}" > "$art/transport.txt"
else
  timeout 15 tests/wayland-gate.sh start "$art/compositor" "$scale"
  source "$art/compositor/env.sh"
  server=$(cat "$art/compositor/compositor.pid"); remember "$server"
  [[ -z ${DISPLAY+x} ]]
  fixture_isolation > "$art/isolation.txt" || exit 1
  # Initial size only. Subsequent capture rectangles follow actual native geometry.
  for rule in 'floating enable' 'move position 0 0'; do
    timeout 5 "$LISPTERM_SWAYMSG" "for_window [title=\"^$LISPTERM_VTTEST_TITLE\$\"] $rule" >> "$art/window-rule.json"
  done
  printf 'DISPLAY=unset\nWAYLAND_DISPLAY=%s\n' "$WAYLAND_DISPLAY" > "$art/transport.txt"
  "$LISPTERM_WTYPE" -s "$(( (${LISPTERM_VTTEST_TIMEOUT:-1800} + 60) * 1000 ))" > "$art/keyboard-holder.log" 2>&1 & keyboard_holder=$!
  remember "$keyboard_holder"; sleep .3; is_owned "$keyboard_holder"
fi
fixture_isolation > "$art/isolation.txt" || { echo "Endpoint isolation failed" >&2; exit 1; }
unset CL_SOURCE_REGISTRY ASDF_OUTPUT_TRANSLATIONS
"$root/scripts/sbcl" --script tests/vttest-gate.lisp < /dev/null > "$art/client.log" 2>&1 & client=$!
remember "$client"
audit_client_tree || { echo "Client endpoint audit failed" >&2; exit 1; }
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
# Separate optional exposure channel. Ordinary requests, images and counters are
# untouched. Every attempt has unique retained files, including failed images.
supplement_capture() {
  local dest=$art/on-phase name ew eh requested_scale extra window owner node
  local width height x y start finish rc=0 before_window before_geometry output
  read -r name ew eh requested_scale extra < "$dest/capture-request"
  [[ $name =~ ^(x11-screen-pause14|wayland-screen-pause14|x11-bug-d-choice0)-attempt0[1-6]$ ]]
  [[ $name == "$backend-"* && $requested_scale == "$scale" && -z $extra ]]
  [[ $ew =~ ^[0-9]+$ && $eh =~ ^[0-9]+$ ]]
  [[ ! -e $dest/$name.png && ! -e $dest/$name.external.sexp && ! -e $dest/$name.ack ]]
  fixture_isolation > "$dest/$name.isolation-before.txt"
  window_identity
  before_window=$window
  if [[ $backend == x11 ]]; then
    timeout 5 xdotool getwindowgeometry --shell "$window" > "$dest/$name.geometry-before.txt"
    width=$(sed -n 's/^WIDTH=//p' "$dest/$name.geometry-before.txt")
    height=$(sed -n 's/^HEIGHT=//p' "$dest/$name.geometry-before.txt")
    local ow oh
    x=$(sed -n 's/^X=//p' "$dest/$name.geometry-before.txt")
    y=$(sed -n 's/^Y=//p' "$dest/$name.geometry-before.txt")
    timeout 5 xdotool getdisplaygeometry > "$dest/$name.output-before.txt"
    read -r ow oh < "$dest/$name.output-before.txt"
    [[ $x -ge 0 && $y -ge 0 && $((x + width)) -le $ow && $((y + height)) -le $oh ]]
    output="x11-$DISPLAY"
  else
    cp "$art/current-tree.json" "$dest/$name.tree-before.json"
    timeout 5 "$LISPTERM_SWAYMSG" -t get_outputs > "$dest/$name.outputs-before.json"
    # A single private output supplies an unambiguous bridge from the protocol's
    # entered/sync_output object to the external compositor output name.
    jq -e 'length == 1 and .[0].active == true' "$dest/$name.outputs-before.json" > /dev/null
    output=$(jq -r '.[0].name' "$dest/$name.outputs-before.json")
    [[ $output =~ ^HEADLESS-[0-9]+$ ]]
    read -r x y width height < <(jq -r '[.rect.x,.rect.y,.rect.width,.rect.height] | @tsv' <<< "$node")
    jq -e --argjson x "$x" --argjson y "$y" --argjson w "$width" --argjson h "$height" \
      '.[0] | $x >= .rect.x and $y >= .rect.y and ($x+$w) <= (.rect.x+.rect.width) and ($y+$h) <= (.rect.y+.rect.height)' "$dest/$name.outputs-before.json" > /dev/null
  fi
  [[ $width == "$ew" && $height == "$eh" ]]
  read -r start _ < /proc/uptime
  if [[ $backend == x11 ]]; then
    timeout 10 import -display "$DISPLAY" -window "$window" "$dest/$name.png" > "$dest/$name.capture.log" 2>&1 || rc=$?
  else
    timeout 10 "$LISPTERM_GRIM" -g "$x,$y ${width}x$height" "$dest/$name.png" > "$dest/$name.capture.log" 2>&1 || rc=$?
  fi
  read -r finish _ < /proc/uptime
  local png=nil stable=t iw=0 ih=0
  if [[ -f $dest/$name.png ]]; then
    if timeout 5 identify -format '%w %h\n' "$dest/$name.png" > "$dest/$name.image-size.txt" 2> "$dest/$name.identify.log"; then
      read -r iw ih < "$dest/$name.image-size.txt"
      if [[ $iw == "$((scale * width))" && $ih == "$((scale * height))" ]]; then png=t; fi
    fi
  fi
  window_identity
  [[ $window == "$before_window" ]] || stable=nil
  if [[ $backend == x11 ]]; then
    timeout 5 xdotool getwindowgeometry --shell "$window" > "$dest/$name.geometry-after.txt"
    cmp -s "$dest/$name.geometry-before.txt" "$dest/$name.geometry-after.txt" || stable=nil
    timeout 5 xdotool getdisplaygeometry > "$dest/$name.output-after.txt"
    cmp -s "$dest/$name.output-before.txt" "$dest/$name.output-after.txt" || stable=nil
  else
    cp "$art/current-tree.json" "$dest/$name.tree-after.json"
    # Full tree includes unrelated compositor state; compare the owned node rect.
    before_geometry=$(jq -c --argjson id "$before_window" '.. | objects | select(.id? == $id) | {id,pid,name,rect}' "$dest/$name.tree-before.json")
    [[ $before_geometry == "$(jq -c '{id,pid,name,rect}' <<< "$node")" ]] || stable=nil
    timeout 5 "$LISPTERM_SWAYMSG" -t get_outputs > "$dest/$name.outputs-after.json"
    cmp -s "$dest/$name.outputs-before.json" "$dest/$name.outputs-after.json" || stable=nil
  fi
  fixture_isolation > "$dest/$name.isolation-after.txt"
  # /proc/uptime has two fractional digits. Ratios retain its exact 10ms ticks.
  printf '(:start %s/100 :end %s/100 :exit %s :png %s :identity-stable %s :client %s :window %s :output "%s" :width %s :height %s :scale %s :pixel-width %s :pixel-height %s :clock :linux-proc-uptime :resolution 1/100 :identity-basis :unique-owned-window-of-single-host)\n' \
    "${start/./}" "${finish/./}" "$rc" "$png" "$stable" "$client" "$window" "$output" "$width" "$height" "$scale" "$iw" "$ih" > "$dest/$name.external.sexp"
  mv "$dest/capture-request" "$dest/$name.request.txt"
  touch "$dest/$name.ack"
}
end=$((SECONDS + ${LISPTERM_VTTEST_TIMEOUT:-1800}))
while is_owned "$client"; do
  watchdog_check "$end"
  collect_tree "$client"
  audit_client_tree allow-exit || { fixture_error 1 "${BASH_SOURCE[0]}" "$LINENO" audit_client_tree; exit 1; }
  if [[ ${LISPTERM_VTTEST_ON_PHASE:-0} == 1 && -f $art/on-phase/capture-request ]]; then
    supplement_capture
  fi
  if [[ -f $art/capture-request ]]; then
    read -r name expected_width expected_height requested_scale extra < "$art/capture-request"
    [[ $name =~ ^[0-9]+-[a-z0-9_-]+$ && $expected_width =~ ^[0-9]+$ && $expected_height =~ ^[0-9]+$ ]]
    [[ $requested_scale == "$scale" && -z $extra && $expected_width -gt 0 && $expected_height -gt 0 ]]
    window_identity
    if [[ $backend == x11 ]]; then
      timeout 5 xdotool getwindowgeometry --shell "$window" > "$art/$name.geometry.txt"
      width=$(sed -n 's/^WIDTH=//p' "$art/$name.geometry.txt")
      height=$(sed -n 's/^HEIGHT=//p' "$art/$name.geometry.txt")
      output_x=0; output_y=0
      read -r output_width output_height < <(timeout 5 xdotool getdisplaygeometry)
      x=$(sed -n 's/^X=//p' "$art/$name.geometry.txt")
      y=$(sed -n 's/^Y=//p' "$art/$name.geometry.txt")
      [[ $x -ge 0 && $y -ge 0 && $((x + width)) -le $output_width && $((y + height)) -le $output_height ]]
      timeout 10 import -display "$DISPLAY" -window "$window" "$art/$name.png"
    else
      cp "$art/current-tree.json" "$art/$name.tree.json"
      jq '{id,pid,name,rect,window_rect,geometry}' <<< "$node" > "$art/$name.geometry.json"
      read -r x y width height < <(jq -r '[.rect.x,.rect.y,.rect.width,.rect.height] | @tsv' <<< "$node")
      jq -e --argjson x "$x" --argjson y "$y" --argjson w "$width" --argjson h "$height" 'any(.[]; $x >= .rect.x and $y >= .rect.y and ($x+$w) <= (.rect.x+.rect.width) and ($y+$h) <= (.rect.y+.rect.height))' "$art/isolation-outputs.json" > /dev/null
      read -r output_x output_y output_width output_height < <(jq -r --argjson x "$x" --argjson y "$y" --argjson w "$width" --argjson h "$height" '.[] | select($x >= .rect.x and $y >= .rect.y and ($x+$w) <= (.rect.x+.rect.width) and ($y+$h) <= (.rect.y+.rect.height)) | [.rect.x,.rect.y,.rect.width,.rect.height] | @tsv' "$art/isolation-outputs.json")
      timeout 10 "$LISPTERM_GRIM" -g "$x,$y ${width}x$height" "$art/$name.png"
    fi
    printf 'expected=%sx%s actual=%sx%s window=%s fixture-scale=%s\n' "$expected_width" "$expected_height" "$width" "$height" "$window" "$scale" > "$art/$name.dimensions.txt"
    if [[ $mode == accept ]]; then
      [[ $width == "$expected_width" && $height == "$expected_height" ]] || { echo 'Capture geometry mismatch' >&2; exit 1; }
    fi
    timeout 5 identify -format '%w %h\n' "$art/$name.png" > "$art/$name.image-size.txt"
    read -r image_width image_height < "$art/$name.image-size.txt"
    [[ $image_width == "$((scale * width))" && $image_height == "$((scale * height))" ]]
    printf '(:recorder-version 3 :logical-width %s :logical-height %s :scale %s :pixel-width %s :pixel-height %s :output-width %s :output-height %s :x %s :y %s :output-x %s :output-y %s)\n' \
      "$width" "$height" "$scale" "$image_width" "$image_height" "$output_width" "$output_height" "$x" "$y" "$output_x" "$output_y" > "$art/$name.external.sexp"
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
fixture_wait_client; client=''
[[ -f $art/finished.sexp ]]
check_mode=measure; [[ $mode != accept ]] || check_mode=accept
scripts/sbcl --script tests/vttest-protocol.lisp --vttest-check "$check_mode" "$art" > "$art/offline-check.log"
echo "Recorder v3 $mode complete (mechanical checks only, visual acceptance separate)"
