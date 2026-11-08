#!/usr/bin/env bash
# C5 disposable native renderer fixture. This does not replace pinned 20-case vttest.
set -Eeuo pipefail
cd "$(dirname "$0")/.."
root=$PWD
# Register each future-view command separately: commas end for_window registration.
native_window_rules() {
 local title=$1 reply=$2 rule
 [[ $title =~ ^[[:alnum:]_-]+$ ]] || return 1
 for rule in 'floating enable' 'border none' 'move position 0 0'; do
  timeout 5 "$LISPTERM_SWAYMSG" "for_window [title=\"^$title\$\"] $rule" |
   tee -a "$reply" |
   jq -es 'length == 1 and (.[0] | type == "array" and length == 1 and
     .[0].success == true and .[0].parse_error != true)' > /dev/null || return 1
 done
}
# Identity is re-read for every action and capture, never inferred from a title alone.
window_identity() {
 if [[ $backend == x11 ]]; then
  mapfile -t windows < <(timeout 5 xdotool search --name "^$LISPTERM_NATIVE_TITLE\$" || true)
  [[ ${#windows[@]} == 1 ]] || return 1
  window=${windows[0]}
  owner=$(timeout 5 xdotool getwindowpid "$window" 2>/dev/null || true)
  [[ -z $owner || $owner == "$client" ]]
 else
  timeout 5 "$LISPTERM_SWAYMSG" -t get_tree > "$art/current-tree.json" || return 1
  node=$(jq -ces --arg t "$LISPTERM_NATIVE_TITLE" --argjson p "$client" '
   if length != 1 then error("tree count") else .[0] end |
   [..|objects|select(.name?==$t and .pid?==$p and (.type?=="floating_con" or .type?=="con"))] |
   if length==1 then .[0] else error("owned window not unique") end |
   select(.id|type=="number" and .>0 and floor==.)' "$art/current-tree.json") || return 1
  window=$(jq -r .id <<< "$node") || return 1
 fi
}
# Exactly one command, one JSON document, one successful reply. Retain failures too.
native_action_ipc() {
 local command=$1 prefix=$2
 printf '%s\n' "$command" > "$prefix.command" || return 1
 timeout 5 "$LISPTERM_SWAYMSG" "$command" > "$prefix.reply.json" || return 1
 jq -es 'length==1 and (.[0]|type=="array" and length==1 and
  (.[0]|type=="object" and .success==true and (has("error")|not) and
   ((has("parse_error")|not) or .parse_error==false)))' "$prefix.reply.json" > /dev/null
}
native_action_outputs() {
 timeout 5 "$LISPTERM_SWAYMSG" -t get_outputs > "$1" || return 1
 jq -es 'length==1 and (.[0]|type=="array")' "$1" > /dev/null || return 1
 fixture_outputs "$1"
}
# Integer logical rectangles must fit exactly one validated private output.
native_action_fit() {
 local outputs=$1 x=$2 y=$3 w=$4 h=$5
 jq -e --argjson x "$x" --argjson y "$y" --argjson w "$w" --argjson h "$h" '
  ([$x,$y,$w,$h]|all(type=="number" and floor==.)) and $w>0 and $h>0 and
  ([.[]|select($x>=.rect.x and $y>=.rect.y and
    ($x+$w)<=(.rect.x+.rect.width) and ($y+$h)<=(.rect.y+.rect.height))]|length==1)' "$outputs" > /dev/null
}
native_resize_wayland() {
 local w=$1 h=$2 prefix=$3 target=$window ax ay aw ah px py pw ph
 is_owned "$client" && is_owned "$server" || return 1
 [[ $(jq -r .type <<< "$node") == floating_con ]] || return 1
 cp "$art/current-tree.json" "$prefix.before.tree.json" || return 1
 native_action_outputs "$prefix.outputs.json" || return 1
 read -r ax ay aw ah < <(jq -er '[.rect.x,.rect.y,.rect.width,.rect.height]|@tsv' <<< "$node") || return 1
 native_action_fit "$prefix.outputs.json" "$ax" "$ay" "$aw" "$ah" || return 1
 # Preparation may retain the safe anchor from before a mode transition.
 ax=${4:-$ax}; ay=${5:-$ay}
 native_action_fit "$prefix.outputs.json" "$ax" "$ay" "$w" "$h" || return 1
 is_owned "$client" && is_owned "$server" || return 1
 native_action_ipc "[con_id=$target] resize set width $w px height $h px" "$prefix.resize" || return 1
 window_identity || return 1
 cp "$art/current-tree.json" "$prefix.resized.tree.json" || return 1
 [[ $window == "$target" && $(jq -r .type <<< "$node") == floating_con ]] || return 1
 native_action_outputs "$prefix.resized.outputs.json" || return 1
 cmp -s <(jq -Sc . "$prefix.outputs.json") <(jq -Sc . "$prefix.resized.outputs.json") || return 1
 is_owned "$client" && is_owned "$server" || return 1
 native_action_ipc "[con_id=$target] move position $ax $ay" "$prefix.anchor" || return 1
 window_identity || return 1
 cp "$art/current-tree.json" "$prefix.anchored.tree.json" || return 1
 [[ $window == "$target" && $(jq -r .type <<< "$node") == floating_con ]] || return 1
 is_owned "$client" && is_owned "$server" || return 1
 read -r px py pw ph < <(jq -er '[.rect.x,.rect.y,.rect.width,.rect.height]|@tsv' <<< "$node") || return 1
 [[ $px == "$ax" && $py == "$ay" ]] || return 1
 native_action_outputs "$prefix.after.outputs.json" || return 1
 native_action_fit "$prefix.after.outputs.json" "$px" "$py" "$pw" "$ph" || return 1
 cmp -s <(jq -Sc . "$prefix.outputs.json") <(jq -Sc . "$prefix.after.outputs.json") || return 1
 is_owned "$client" && is_owned "$server"
}
# Separate scoped operations: validate transport, reply, identity and ownership
# after each command. An IPC ACK does not establish owner configure dispatch.
native_mode_wayland() {
 local prefix=$1 target=$window command index=0
 shift
 is_owned "$client" && is_owned "$server" || return 1
 cp "$art/current-tree.json" "$prefix.before.tree.json" || return 1
 native_action_outputs "$prefix.outputs.json" || return 1
 for command in "$@"; do
  index=$((index+1))
  is_owned "$client" && is_owned "$server" || return 1
  native_action_ipc "[con_id=$target] $command" "$prefix.step$index" || return 1
  window_identity || return 1
  cp "$art/current-tree.json" "$prefix.step$index.tree.json" || return 1
  [[ $window == "$target" ]] || return 1
  case $command in
   'floating enable'|'border none'|'move position 0 0') [[ $(jq -r .type <<< "$node") == floating_con ]] || return 1;;
   'floating disable'|'fullscreen enable') [[ $(jq -r .type <<< "$node") == con ]] || return 1;;
  esac
  is_owned "$client" && is_owned "$server" || return 1
  native_action_outputs "$prefix.step$index.outputs.json" || return 1
  cmp -s <(jq -Sc . "$prefix.outputs.json") <(jq -Sc . "$prefix.step$index.outputs.json") || return 1
 done
 is_owned "$client" && is_owned "$server"
}
native_prepare_retry() {
 local w=$1 h=$2 prefix=$3 target=$window ax ay aw ah
 is_owned "$client" && is_owned "$server" || return 1
 cp "$art/current-tree.json" "$prefix.failed.tree.json" || return 1
 native_action_outputs "$prefix.failed.outputs.json" || return 1
 read -r ax ay aw ah < <(jq -er '[.rect.x,.rect.y,.rect.width,.rect.height]|@tsv' <<< "$node") || return 1
 native_action_fit "$prefix.failed.outputs.json" "$ax" "$ay" "$aw" "$ah" || return 1
 native_action_fit "$prefix.failed.outputs.json" "$ax" "$ay" "$w" "$h" || return 1
 native_mode_wayland "$prefix.floating" 'fullscreen disable' 'floating enable' 'border none' || return 1
 [[ $window == "$target" ]] || return 1
 cmp -s <(jq -Sc . "$prefix.failed.outputs.json") <(jq -Sc . "$prefix.floating.outputs.json") || return 1
 cmp -s <(jq -Sc . "$prefix.failed.outputs.json") <(jq -Sc . "$prefix.floating.step3.outputs.json") || return 1
 native_resize_wayland "$w" "$h" "$prefix.restore" "$ax" "$ay" || return 1
 cmp -s <(jq -Sc . "$prefix.failed.outputs.json") <(jq -Sc . "$prefix.restore.outputs.json") || return 1
 cmp -s <(jq -Sc . "$prefix.failed.outputs.json") <(jq -Sc . "$prefix.restore.after.outputs.json") || return 1
 is_owned "$client" && is_owned "$server"
}
# Reserve an action ordinal once. A failed handoff cannot be retried as a fresh
# action, and producer serials never determine this independent 1/9 sequence.
native_action_ordinal=0
native_action_failed=0
native_action_copy() (
 local source=$1 destination=$2 byte count=0 status
 local LC_ALL=C
 [[ ! -L $source && -f $source && -r $source ]] || exit 1
 [[ ! -L $art/record-read.stderr && ! -s $art/record-read.stderr ]] || exit 1
 [[ ! -e $destination && ! -L $destination ]] || exit 1
 set -C
 exec 3> "$destination" || exit 1
 # Keep malformed/partial bytes too. At most 4098 bytes are retained; overflow
 # is failure, the original is kept, and this prefix is never a complete copy.
 while ((count < 4098)); do
  byte=
  if IFS= read -r -d '' -n 1 byte; then status=0; else status=$?; fi
  if ((status != 0)); then
   [[ $status == 1 && ! -s $art/record-read.stderr ]]
   exit $?
  fi
  if [[ -n $byte ]]; then printf '%s' "$byte" >&3; else printf '\0' >&3; fi || exit 1
  count=$((count+1))
 done < "$source" 2>> "$art/record-read.stderr"
 exit 1
)
# Enumerate the source-defined suffix graph, never names discovered in R.
native_action_resize_leaves() {
 local base=$1 suffix
 for suffix in before.tree.json outputs.json resize.command resize.reply.json resized.tree.json resized.outputs.json anchor.command anchor.reply.json anchored.tree.json after.outputs.json; do
  action_leaves+=("$base.$suffix")
 done
}
native_action_mode_leaves() {
 local base=$1 steps=$2 index
 action_leaves+=("$base.before.tree.json" "$base.outputs.json")
 for ((index=1;index<=steps;index++)); do
  action_leaves+=("$base.step$index.command" "$base.step$index.reply.json" "$base.step$index.tree.json" "$base.step$index.outputs.json")
 done
}
native_action_context() {
 is_owned "$client" && is_owned "$server" &&
 [[ $XDG_RUNTIME_DIR == "$capture_runtime" && ${DISPLAY:-} == "$capture_display" && ${SWAYSOCK:-} == "$capture_swaysock" && ${WAYLAND_DISPLAY:-} == "$capture_wayland" ]]
}
external_action() {
 local action w h extra prefix stage ordinal expected leaf target
 local -a sequence action_leaves=()
 [[ $native_action_failed == 0 && $native_action_ordinal =~ ^[0-9]$ ]] || return 1
 native_action_failed=1
 case $mode in
  normal) sequence=(resize);;
  refusal) [[ $backend == wayland ]] || return 1
   sequence=(resize tiled prepare-retry tiled floating resize tiled floating resize);;
  *) return 1;;
 esac
 ordinal=$((native_action_ordinal+1))
 ((ordinal <= ${#sequence[@]})) || return 1
 native_action_ordinal=$ordinal
 expected=${sequence[ordinal-1]}
 printf -v prefix '%s/action-%02d' "$art" "$ordinal"
 printf -v stage '%s/action-request-s%02d.staged' "$art" "$ordinal"
 native_action_copy "$art/action-request" "$prefix.request.raw" || return 1
 [[ ! -L $stage && -f $stage && $stage -ef $art/action-request ]] || return 1
 cmp -s "$prefix.request.raw" "$stage" || return 1
 acq_read_record "$prefix.request.raw" 4096 || return 1
 IFS=' ' read -r action w h extra <<< "$acq_record"
 [[ -z $extra && $action == "$expected" ]] || return 1
 case $action in
  resize|prepare-retry)
   [[ $w =~ ^[1-9][0-9]{0,4}$ && $h =~ ^[1-9][0-9]{0,4}$ && $w -le 32768 && $h -le 32768 && $acq_record == "$action $w $h" ]] || return 1;;
  tiled|floating) [[ -z $w && -z $h && $acq_record == "$action" ]] || return 1;;
  *) return 1;;
 esac
 [[ $backend == x11 || $backend == wayland ]] || return 1
 action_leaves=("$prefix.ack.staged" "$prefix.ack.publish.stdout" "$prefix.ack.publish.stderr")
 case $action in
  resize) if [[ $backend == x11 ]]; then action_leaves+=("$prefix.json")
   else native_action_resize_leaves "$prefix"; fi;;
  tiled) native_action_mode_leaves "$prefix" 2;;
  floating) native_action_mode_leaves "$prefix" 4;;
  prepare-retry)
   action_leaves+=("$prefix.failed.tree.json" "$prefix.failed.outputs.json")
   native_action_mode_leaves "$prefix.floating" 3
   native_action_resize_leaves "$prefix.restore";;
 esac
 # -L also detects dangling links. No pre-existing leaf is adopted or replaced.
 for leaf in "$art/action-ack" "$art/action-ack.tmp" "$prefix.ack.raw" "${action_leaves[@]}"; do
  [[ ! -e $leaf && ! -L $leaf ]] || return 1
 done
 native_action_context && window_identity || return 1
 [[ $window =~ ^[1-9][0-9]{0,11}$ && ( ${capture_action_window:-NIL} == NIL || $window == "$capture_action_window" ) ]] || return 1
 target=$window
 case $action in
  resize)
   if [[ $backend == x11 ]]; then
    (set -C; timeout 5 xdotool windowsize --sync "$window" "$w" "$h" > "$prefix.json") || return 1
    window_identity && [[ $window == "$target" ]] || return 1
   else native_resize_wayland "$w" "$h" "$prefix" || return 1; fi;;
  tiled) native_mode_wayland "$prefix" 'floating disable' 'fullscreen enable' || return 1;;
  floating) native_mode_wayland "$prefix" 'fullscreen disable' 'floating enable' 'border none' 'move position 0 0' || return 1;;
  prepare-retry) native_prepare_retry "$w" "$h" "$prefix" || return 1;;
 esac
 native_action_context && [[ $window == "$target" ]] || return 1
 # Recheck the same retained request after native operations, before removal.
 [[ ! -L $stage && -f $stage && ! -L $art/action-request && $stage -ef $art/action-request ]] || return 1
 cmp -s "$prefix.request.raw" "$stage" && cmp -s "$stage" "$art/action-request" || return 1
 rm -- "$art/action-request" || return 1
 (
  set -C
  printf '%s %s\n' "$action" "$window" > "$prefix.ack.staged" || exit 1
  command timeout --signal=KILL 1 ln -T -- "$prefix.ack.staged" "$art/action-ack" > "$prefix.ack.publish.stdout" 2> "$prefix.ack.publish.stderr" || exit 1
 ) || return 1
 # Successful ln is required, not merely a staged ACK. The owner may already
 # have consumed the alias; if present, it must still be that exact hardlink.
 [[ ! -s $prefix.ack.publish.stdout && ! -s $prefix.ack.publish.stderr ]] || return 1
 if [[ -e $art/action-ack || -L $art/action-ack ]]; then
  [[ ! -L $art/action-ack && -f $art/action-ack && $prefix.ack.staged -ef $art/action-ack ]] || return 1
 fi
 native_action_failed=0
}
# Acquisition protocol v1. These helpers are sourceable without native dispatch.
# Read one bounded ASCII record including its required LF and physical EOF.
# -n, unlike -N, honors the empty delimiter: NUL is an empty successful read.
# Never send bytes through command substitution, which would discard NUL/LF.
acq_read_record() {
 local path=$1 maximum=${2:-4096} byte count=0 ended=0 status errors
 local LC_ALL=C
 # Bash returns 1 for both EOF and I/O errors. Retain stderr and fail closed
 # after any read diagnostic, including an error immediately after the LF.
 [[ -n ${art:-} && -d $art ]] || return 1
 errors=$art/record-read.stderr
 [[ ! -L $errors && ! -s $errors ]] || return 1
 acq_record=
 [[ -f $path && -r $path && $maximum =~ ^[1-9][0-9]{0,3}$ ]] || return 1
 while :; do
  byte=
  if IFS= read -r -d '' -n 1 byte; then status=0; else status=$?; fi
  if ((status != 0)); then
   ((status == 1 && ended == 1)) && [[ -z $byte && ! -s $errors ]]
   return $?
  fi
  count=$((count+1))
  ((count <= maximum+1 && ended == 0)) || return 1
  if [[ $byte == $'\n' ]]; then ended=1
  elif [[ -n $byte && ( $byte == $'\t' || $byte == [\ -~] ) ]]; then
   ((count <= maximum)) || return 1
   acq_record+=$byte
  else return 1; fi
 done < "$path" 2>> "$errors"
}
# Source only fixture helpers for offline parser/IPC/action controls.
[[ ${LISPTERM_NATIVE_FUNCTIONS_ONLY:-0} != 1 ]] || return 0
help() {
 cat <<'HELP'
Usage: tests/verify.sh native-geometry self-test
       tests/verify.sh native-geometry x11|wayland [--scale 1|2] [--font-size 14|16|18] [--mode normal|refusal] [--timeout 30..600]
Defaults: scale1, font16, normal, 180 seconds. X11 accepts scale1/normal only.
Native execution is explicit; self-test cannot start a server or client.
Each run retains a NEW .build/evidence/native-geometry-BACKEND-scaleN-fontN-MODE.*
font selector chooses the actual Terminus OTB strike; runtime dimensions use its
reported metrics. Run each backend, Wayland scales1/2, and a second font strike.
Refusal: private Wayland only, tiled/maximized request132 must reach :refused or
:timed-out within15s with owner alive. That checkpoint is expected-refusal, NEVER
ordinary success. Restore saved target while floating, await an owner-observed
unconstrained configure/engine/PTY barrier, then retry once (same id/new epoch).
External preparation is not recovery. Tiled failure/cancel-adopt,
ordinary resize recovery and new DECCOLM are checked. Any other outcome fails.
Single inherited FD9 requires exact held-lock proof. Endpoint failure has no
fallback. Only remembered PID/start-token processes and private runtime dirs are
cleaned. Failed runs are retained. Native execution and PNG review are separate.
Examples:
 tests/verify.sh native-geometry x11
 tests/verify.sh native-geometry wayland --scale 2 --font-size 14
 tests/verify.sh native-geometry wayland --mode refusal
HELP
}
[[ ${1:-} != --help && ${1:-} != -h ]] || { help; exit 0; }
if [[ ${1:-} == self-test ]]; then
 [[ $# == 1 ]] || exit 2
 exec scripts/sbcl --script tests/vt100-native-geometry.lisp --self-test
fi
backend=${1:-}; shift "$(( $# > 0 ? 1 : 0 ))"
scale=1; font_size=16; mode=normal; duration=180
while (($#)); do
 [[ $# -ge 2 ]] || { help >&2; exit 2; }
 case $1 in --scale) scale=$2;; --font-size) font_size=$2;; --mode) mode=$2;; --timeout) duration=$2;; *) help >&2; exit 2;; esac
 shift 2
done
# Documented 122 interface: this early return defines helpers, with no launch,
# lock, trap or artifact action. The source changes cwd, so source absolute path.
LISPTERM_VTTEST_FUNCTIONS_ONLY=1 source "$root/tests/vttest-gate.sh"
fixture_scale "$backend" "$scale" || { help >&2; exit 2; }
[[ $font_size == 14 || $font_size == 16 || $font_size == 18 ]] || exit 2
[[ $mode == normal || ( $mode == refusal && $backend == wayland ) ]] || exit 2
[[ $duration =~ ^[0-9]+$ && $duration -ge 30 && $duration -le 600 ]] || exit 2
export PATH="$root/.build/test-tools/bin:$PATH"
export LISPTERM_SBCL=${LISPTERM_SBCL:-$root/.build/deps/bin/sbcl}
fonts=(/nix/store/*-terminus-font-*/share/fonts/terminus/ter-u${font_size}n.otb)
[[ -f ${fonts[0]} ]] || { echo 'Requested font strike unavailable' >&2; exit 1; }
export LISPTERM_FONT=${fonts[0]}
mkdir -p .build/evidence
fixture_lock || { echo 'Invalid inherited GUI lock' >&2; exit 1; }
art=$(mktemp -d "$root/.build/evidence/native-geometry-${backend}-scale${scale}-font${font_size}-${mode}.XXXXXX")
export LISPTERM_NATIVE_ARTIFACTS="$art/" LISPTERM_NATIVE_SCALE=$scale LISPTERM_NATIVE_BACKEND=$backend LISPTERM_NATIVE_MODE=$mode
export LISPTERM_NATIVE_TITLE="Lispterm-native-${art##*.}-$$"
client='' server='' runtime=''
cleanup() {
 local code=$? pid
 trap - EXIT INT TERM ERR
 set +e
 if [[ -f $art/compositor/compositor.pid && -f $art/compositor/compositor.start ]]; then
  pid=$(cat "$art/compositor/compositor.pid")
  if [[ $(process_token "$pid") == "$(cat "$art/compositor/compositor.start")" ]] && grep -zFxq -- "$art/compositor/sway.conf" "/proc/$pid/cmdline"; then remember "$pid"; fi
 fi
 if stop_owned; then
  if [[ -f $art/compositor/runtime-path ]]; then runtime=$(cat "$art/compositor/runtime-path"); fi
  if private_runtime "$runtime"; then rm -rf -- "$runtime"; fi
 else code=1; fi
 printf '%s\n' "$code" > "$art/harness.exit"
 (cd "$art"; find . -type f ! -name provenance.sha256 -print0 | sort -z | xargs -0 sha256sum > provenance.sha256)
 echo "native-geometry exit=$code: $art"
 exit "$code"
}
trap cleanup EXIT
trap 'fixture_error "$?" "${BASH_SOURCE[0]}" "$LINENO" "${FUNCNAME[0]:-main}"' ERR
trap 'exit 130' INT
trap 'exit 143' TERM
printf 'backend=%s scale=%s font-size=%s mode=%s timeout=%s\n' "$backend" "$scale" "$font_size" "$mode" "$duration" > "$art/fixture.txt"
printf 'command=' >> "$art/fixture.txt"; printf '%q ' "$0" "$backend" --scale "$scale" --font-size "$font_size" --mode "$mode" --timeout "$duration" >> "$art/fixture.txt"; echo >> "$art/fixture.txt"
git rev-parse HEAD > "$art/run-head.txt"
sha256sum "$LISPTERM_FONT" tests/vt100-native-geometry.* tests/vttest-protocol.lisp tests/vttest-gate.sh > "$art/source.sha256"
clear_session_endpoints
if [[ $backend == x11 ]]; then
 runtime=$(mktemp -d /tmp/lispterm-x11.XXXXXX); chmod 700 "$runtime"; export XDG_RUNTIME_DIR=$runtime
 Xvfb -displayfd 3 -screen 0 2560x1600x24 -nolisten tcp 3> "$art/display-number" > "$art/xvfb.log" 2>&1 & server=$!; remember "$server"
 for ((i=0;i<100;i++)); do is_owned "$server" || exit 1; [[ -s $art/display-number ]] && break; sleep .05; done
 number=$(cat "$art/display-number"); [[ $number =~ ^[0-9]+$ ]]; export DISPLAY=:$number
else
 timeout 15 tests/wayland-gate.sh start "$art/compositor" "$scale"
 source "$art/compositor/env.sh"
 server=$(cat "$art/compositor/compositor.pid"); remember "$server"
 native_window_rules "$LISPTERM_NATIVE_TITLE" "$art/window-rule.json"
fi
fixture_isolation > "$art/isolation.txt"
unset CL_SOURCE_REGISTRY ASDF_OUTPUT_TRANSLATIONS
scripts/sbcl --script tests/vt100-native-geometry.lisp --native < /dev/null > "$art/client.log" 2>&1 & client=$!; remember "$client"
audit_client_tree
capture_runtime=$XDG_RUNTIME_DIR; capture_display=${DISPLAY:-}; capture_swaysock=${SWAYSOCK:-}; capture_wayland=${WAYLAND_DISPLAY:-}
capture_action_window=NIL
capture_native() {
 local name ew eh ds cw extra
 read -r name ew eh ds cw extra < "$art/capture-request"
 [[ $name =~ ^[0-9]+-[a-z0-9_-]+$ && $ew =~ ^[0-9]+$ && $eh =~ ^[0-9]+$ && $cw =~ ^[0-9]+$ && $ds == "$scale" && -z $extra ]]
 window_identity
 if [[ $backend == x11 ]]; then
  timeout 5 xdotool getwindowgeometry --shell "$window" > "$art/$name.geometry.txt"
  width=$(sed -n 's/^WIDTH=//p' "$art/$name.geometry.txt"); height=$(sed -n 's/^HEIGHT=//p' "$art/$name.geometry.txt")
  x=$(sed -n 's/^X=//p' "$art/$name.geometry.txt"); y=$(sed -n 's/^Y=//p' "$art/$name.geometry.txt")
  ox=0; oy=0; read -r ow oh < <(timeout 5 xdotool getdisplaygeometry)
  timeout 10 import -display "$DISPLAY" -window "$window" "$art/$name.png"
 else
  cp "$art/current-tree.json" "$art/$name.tree.json"
  read -r x y width height < <(jq -r '[.rect.x,.rect.y,.rect.width,.rect.height]|@tsv' <<< "$node")
  timeout 5 "$LISPTERM_SWAYMSG" -t get_outputs > "$art/$name.outputs.json"
  fixture_outputs "$art/$name.outputs.json"
  read -r ox oy ow oh < <(jq -er --argjson x "$x" --argjson y "$y" --argjson w "$width" --argjson h "$height" '[.[]|select($x>=.rect.x and $y>=.rect.y and ($x+$w)<=(.rect.x+.rect.width) and ($y+$h)<=(.rect.y+.rect.height))]|if length==1 then .[0]|[.rect.x,.rect.y,.rect.width,.rect.height]|@tsv else error("output fit") end' "$art/$name.outputs.json")
  timeout 10 "$LISPTERM_GRIM" -g "$x,$y ${width}x$height" "$art/$name.png"
 fi
 [[ $width == "$ew" && $height == "$eh" && $ow -ge $((132*cw)) && $x -ge $ox && $y -ge $oy && $((x+width)) -le $((ox+ow)) && $((y+height)) -le $((oy+oh)) ]]
 read -r pw ph < <(timeout 5 identify -format '%w %h\n' "$art/$name.png")
 [[ $pw == "$((scale*width))" && $ph == "$((scale*height))" ]]
 printf '(:logical-width %s :logical-height %s :scale %s :pixel-width %s :pixel-height %s :output-width %s :output-height %s :x %s :y %s :output-x %s :output-y %s :window %s)\n' "$width" "$height" "$scale" "$pw" "$ph" "$ow" "$oh" "$x" "$y" "$ox" "$oy" "$window" > "$art/$name.external.sexp"
 rm "$art/capture-request"; printf '%s\n' "$name" > "$art/capture-ack.tmp"; mv "$art/capture-ack.tmp" "$art/capture-ack"
}
end=$((SECONDS+duration))
while is_owned "$client"; do
 watchdog_check "$end"; fixture_isolation >> "$art/isolation-checks.txt"
 audit_client_tree allow-exit || { fixture_error 1 "${BASH_SOURCE[0]}" "$LINENO" audit_client_tree; exit 1; }
 if [[ -e $art/action-request || -L $art/action-request ]]; then external_action; capture_action_window=$window; fi
 if [[ -f $art/capture-request ]]; then capture_native; fi
 sleep .03
done
fixture_wait_client
[[ -f $art/completed.sexp ]]
