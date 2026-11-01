#!/usr/bin/env bash
# C5 disposable native renderer fixture. This does not replace pinned 20-case vttest.
set -euo pipefail
cd "$(dirname "$0")/.."
root=$PWD
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
ordinary success. Then floating retry (new epoch), tiled failure/cancel-adopt,
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
 trap - EXIT INT TERM
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
trap 'exit 130' INT
trap 'exit 143' TERM
printf 'backend=%s scale=%s font-size=%s mode=%s timeout=%s\n' "$backend" "$scale" "$font_size" "$mode" "$duration" > "$art/fixture.txt"
printf 'command=' >> "$art/fixture.txt"; printf '%q ' "$0" "$backend" --scale "$scale" --font-size "$font_size" --mode "$mode" --timeout "$duration" >> "$art/fixture.txt"; echo >> "$art/fixture.txt"
git rev-parse HEAD > "$art/run-head.txt"
sha256sum "$LISPTERM_FONT" tests/vt100-native-geometry.* tests/vttest-protocol.lisp > "$art/source.sha256"
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
 timeout 5 "$LISPTERM_SWAYMSG" "for_window [title=\"^$LISPTERM_NATIVE_TITLE\$\"] floating enable, border none, move position 0 0" > "$art/window-rule.json"
fi
fixture_isolation > "$art/isolation.txt"
unset CL_SOURCE_REGISTRY ASDF_OUTPUT_TRANSLATIONS
scripts/sbcl --script tests/vt100-native-geometry.lisp --native < /dev/null > "$art/client.log" 2>&1 & client=$!; remember "$client"
audit_client_tree
window_identity() {
 if [[ $backend == x11 ]]; then
  mapfile -t windows < <(timeout 5 xdotool search --name "^$LISPTERM_NATIVE_TITLE\$" || true)
  [[ ${#windows[@]} == 1 ]]; window=${windows[0]}
  owner=$(timeout 5 xdotool getwindowpid "$window" 2>/dev/null || true)
  [[ -z $owner || $owner == "$client" ]]
 else
  timeout 5 "$LISPTERM_SWAYMSG" -t get_tree > "$art/current-tree.json"
  node=$(jq -ce --arg t "$LISPTERM_NATIVE_TITLE" --argjson p "$client" '[..|objects|select(.name?==$t and .pid?==$p and (.type?=="floating_con" or .type?=="con"))]|if length==1 then .[0] else error("owned window not unique") end' "$art/current-tree.json")
  window=$(jq -r .id <<< "$node")
 fi
}
external_action() {
 local action w h extra
 read -r action w h extra < "$art/action-request"
 [[ -z $extra ]]; window_identity
 case $action in
  resize)
   [[ $w =~ ^[0-9]+$ && $h =~ ^[0-9]+$ && $w -gt 0 && $h -gt 0 ]]
   if [[ $backend == x11 ]]; then timeout 5 xdotool windowsize --sync "$window" "$w" "$h";
   else timeout 5 "$LISPTERM_SWAYMSG" "[con_id=$window] resize set width $w px height $h px"; fi;;
  tiled) [[ $backend == wayland ]]; timeout 5 "$LISPTERM_SWAYMSG" "[con_id=$window] floating disable, fullscreen enable";;
  floating) [[ $backend == wayland ]]; timeout 5 "$LISPTERM_SWAYMSG" "[con_id=$window] fullscreen disable, floating enable, border none, move position 0 0";;
  *) return 1;;
 esac > "$art/action-$(date +%s%N).json"
 rm "$art/action-request"; printf '%s\n' "$action $window" > "$art/action-ack.tmp"; mv "$art/action-ack.tmp" "$art/action-ack"
}
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
 watchdog_check "$end"; fixture_isolation >> "$art/isolation-checks.txt"; audit_client_tree
 if [[ -f $art/action-request ]]; then external_action; fi
 if [[ -f $art/capture-request ]]; then capture_native; fi
 sleep .03
done
wait "$client"
[[ -f $art/completed.sexp ]]
