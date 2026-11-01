#!/usr/bin/env bash
# Offline and disposable process-tree fixtures only. No display/terminal launch.
set -euo pipefail
cd "$(dirname "$0")/.."
root=$PWD
export PATH="$root/.build/test-tools/bin:$PATH"
# Each invocation retains its own logs and fixtures under this workspace directory.
# The ARTIFACTS line reports the generated absolute directory; no override is needed.
mkdir -p "$root/.build/evidence"
art=$(mktemp -d "$root/.build/evidence/vttest-harness.XXXXXX")
printf 'ARTIFACTS %s\n' "$art"
export LISPTERM_VTTEST_FUNCTIONS_ONLY=1
source tests/vttest-gate.sh
unset LISPTERM_VTTEST_FUNCTIONS_ONLY
# Pure/mock isolation tests, with a real disposable lock but no GUI processes.
fixture_scale x11 1; fixture_scale wayland 1; fixture_scale wayland 2
for spec in 'x11 2' 'wayland 0' 'wayland 3' 'wayland 02' 'wayland 1.0' 'unknown 1'; do
  read -r b s <<< "$spec"
  if fixture_scale "$b" "$s"; then echo "Invalid scale accepted: $spec" >&2; exit 1; fi
done
(
  clear_session_endpoints
  export XDG_RUNTIME_DIR=/tmp/lispterm-mock WAYLAND_DISPLAY=wayland-1 SWAYSOCK=/tmp/lispterm-mock/ipc
  backend=wayland
  printf 'XDG_RUNTIME_DIR=%s\0WAYLAND_DISPLAY=%s\0SWAYSOCK=%s\0' "$XDG_RUNTIME_DIR" "$WAYLAND_DISPLAY" "$SWAYSOCK" > "$art/mock-env"
  audit_endpoint_file "$art/mock-env"
  for bad in 'DISPLAY=:0' 'XAUTHORITY=/user/auth' 'DBUS_SESSION_BUS_ADDRESS=unix:/user/bus' 'XDG_RUNTIME_DIR=/run/user/1000' 'WAYLAND_DISPLAY=wayland-0' 'SWAYSOCK=/user/ipc'; do
    cp "$art/mock-env" "$art/mock-bad-env"; printf '%s\0' "$bad" >> "$art/mock-bad-env"
    if audit_endpoint_file "$art/mock-bad-env"; then echo "Endpoint accepted: ${bad%%=*}" >&2; exit 1; fi
  done
  backend=x11; export DISPLAY=:123
  unset WAYLAND_DISPLAY SWAYSOCK
  printf 'XDG_RUNTIME_DIR=%s\0DISPLAY=%s\0' "$XDG_RUNTIME_DIR" "$DISPLAY" > "$art/mock-env"
  audit_endpoint_file "$art/mock-env"
  printf 'DISPLAY=:0\0' >> "$art/mock-env"
  if audit_endpoint_file "$art/mock-env"; then exit 1; fi
)
mkdir -p "$art/mock-fd"
ln -s 'socket:[123]' "$art/mock-fd/4"
printf 'Num RefCount Protocol Flags Type St Inode Path\n000: 2 0 10000 1 01 123 /private/socket\n001: 2 0 10000 1 01 456 /user/socket\n' > "$art/mock-unix"
socket_inode_match 0 /private/socket "$art/mock-fd" "$art/mock-unix" > "$art/mock-inode-match"
if socket_inode_match 0 /user/socket "$art/mock-fd" "$art/mock-unix"; then exit 1; fi
if socket_inode_match 0 /missing/socket "$art/mock-fd" "$art/mock-unix"; then exit 1; fi
# A matching pathname without an owned socket inode is not proof.
rm "$art/mock-fd/4"; ln -s /private/socket "$art/mock-fd/4"
if socket_inode_match 0 /private/socket "$art/mock-fd" "$art/mock-unix"; then exit 1; fi
(
  scale=2
  printf '[{"name":"HEADLESS-1","active":true,"scale":2,"current_mode":{"width":2560,"height":1600}}]\n' > "$art/mock-outputs"
  fixture_outputs "$art/mock-outputs"
  for bad in '.[0].name="DP-1"' '.[0].scale=1' '.[0].current_mode.width=1280' '.[0].active=false'; do
    jq "$bad" "$art/mock-outputs" > "$art/mock-bad-outputs"
    if fixture_outputs "$art/mock-bad-outputs"; then exit 1; fi
  done
)
(
  # Never contend with the live GUI lock in non-GUI tests.
  root=$art/mock-lock-root; mkdir -p "$root/.build"
  export LISPTERM_GUI_LOCK_HELD=1
  exec 9>&-
  if fixture_lock 2>/dev/null; then echo 'Marker-only lock accepted' >&2; exit 1; fi
  exec 9> "$root/.build/vttest-gui.lock"
  if fixture_lock; then echo 'Unlocked FD9 accepted' >&2; exit 1; fi
  unset LISPTERM_GUI_LOCK_HELD
  fixture_lock
  (fixture_lock) # Real inherited held FD, no nested flock.
  LISPTERM_VTTEST_FUNCTIONS_ONLY=1 LISPTERM_GUI_LOCK_HELD=1 bash -c 'source tests/vttest-gate.sh; root=$1; fixture_lock' bash "$root"
  if flock -n "$root/.build/vttest-gui.lock" true; then echo 'Lock was not held' >&2; exit 1; fi
)
printf 'PASS scale validation, endpoint poisoning, socket-inode join, headless outputs, inherited FD9 proof\n' > "$art/isolation-tests.log"
# Exercise the production registration helper without compositor or client launches.
(
  LISPTERM_NATIVE_FUNCTIONS_ONLY=1 source tests/vt100-native-geometry.sh
  LISPTERM_SWAYMSG=mock-swaymsg
  title=Lispterm-native-rule-control-133
  rule_checks=0
  timeout() {
    local number rule
    [[ $# == 3 && $1 == 5 && $2 == mock-swaymsg ]] || return 90
    number=$(( $(wc -l < "$art/rule-calls") + 1 ))
    case $number in 1) rule='floating enable';; 2) rule='border none';; 3) rule='move position 0 0';; *) return 91;; esac
    [[ $3 == "for_window [title=\"^$title\$\"] $rule" ]] || return 92
    printf '%s\n' "$3" >> "$art/rule-calls"
    if [[ $number != "$fail_at" ]]; then printf '[{"success":true}]\n'; return; fi
    case $reply_kind in
      exit) printf '[{"success":true}]\n'; return 2;;
      empty) return 0;;
      malformed) printf '{broken\n';;
      false) printf '[{"success":false,"parse_error":true}]\n';;
      mixed) printf '[{"success":true},{"success":false}]\n';;
      extra) printf '[{"success":false}]\n[{"success":true}]\n';;
      parse) printf '[{"success":true,"parse_error":true}]\n';;
    esac
  }
  : > "$art/rule-calls"
  fail_at=0 reply_kind=success
  native_window_rules "$title" "$art/rule-success.json"
  [[ $(wc -l < "$art/rule-calls") == 3 ]]
  jq -es 'length == 3 and all(.[]; length == 1 and .[0].success == true)' "$art/rule-success.json" > /dev/null
  ((rule_checks+=1))
  for fail_at in 1 2 3; do
    for reply_kind in exit empty malformed false mixed extra parse; do
      : > "$art/rule-calls"
      if native_window_rules "$title" "$art/rule-$fail_at-$reply_kind.json" 2> "$art/rule-$fail_at-$reply_kind.stderr"; then
        echo "Registration accepted $reply_kind at $fail_at" >&2; exit 1
      fi
      [[ $(wc -l < "$art/rule-calls") == "$fail_at" ]]
      ((rule_checks+=1))
    done
  done
  export -f native_window_rules timeout
  export art title LISPTERM_SWAYMSG fail_at reply_kind
  reply_kind=exit
  for fail_at in 1 2 3; do
    : > "$art/rule-calls"
    code=0
    bash -c 'set -euo pipefail; native_window_rules "$title" "$art/rule-errexit-$fail_at.json"; touch "$art/rule-continued"' || code=$?
    [[ $code == 1 && ! -e $art/rule-continued && $(wc -l < "$art/rule-calls") == "$fail_at" ]]
    ((rule_checks+=1))
  done
  for bad_title in '' 'foreign.*' 'owned"' 'owned; border none' 'owned, move position 0 0' 'owned]' 'owned\\' $'owned\nforeign'; do
    : > "$art/rule-calls"
    if native_window_rules "$bad_title" "$art/rule-invalid.json"; then exit 1; fi
    [[ ! -s $art/rule-calls && ! -e $art/rule-invalid.json ]]
    ((rule_checks+=1))
  done
  printf 'PASS %s registration controls: exact selector per action, ordered policies, IPC/reply failure stops, invalid selector rejection\n' "$rule_checks" > "$art/window-rule-tests.log"
)
export LISPTERM_SBCL=${LISPTERM_SBCL:-$root/.build/deps/bin/sbcl}
export LISPTERM_HARNESS_TEST_ARTIFACTS="$art/fixtures-$(date +%s%N)/"
export LISPTERM_VTTEST_ARTIFACTS="$LISPTERM_HARNESS_TEST_ARTIFACTS"
scripts/sbcl --script tests/vttest-protocol-tests.lisp > "$art/protocol-tests.log" 2>&1
scripts/sbcl --script tests/vttest-recorder-tests.lisp > "$art/recorder-tests.log" 2>&1
bash -n tests/vttest-gate.sh tests/vttest-harness-tests.sh tests/verify.sh tests/wayland-gate.sh
# Retain a negative-control process; it is ours, but deliberately not in the
# production cleanup ownership set. No existing process is inspected or signalled.
bash -c 'trap "" TERM; while :; do sleep 10; done' & fixture_root=$!
remember "$fixture_root"
bash -c 'sleep 30' & sentinel=$!
sentinel_token=$(process_token "$sentinel")
cleanup_fixture() {
  set +e
  stop_owned
  if [[ $(process_token "$sentinel" 2>/dev/null) == "$sentinel_token" ]]; then kill "$sentinel"; fi
  wait "$sentinel" 2>/dev/null
}
trap cleanup_fixture EXIT
sleep .15
collect_tree "$fixture_root"
printf 'root=%s owned=%s sentinel=%s\n' "$fixture_root" "${!owned[*]}" "$sentinel" > "$art/fixture-processes.txt"
(( ${#owned[@]} >= 2 ))
# A simulated stale PID identity must not be signalled. Registering an incorrect
# start token exercises the exact production guard without PID-reuse tricks.
owned[$sentinel]=not-the-start-token
stop_owned
[[ $(process_token "$sentinel") == "$sentinel_token" ]]
if watchdog_check "$SECONDS"; then echo 'Expired watchdog succeeded' >&2; exit 1; else [[ $? == 124 ]]; fi
watchdog_check "$((SECONDS + 10))"
for pid in "${!owned[@]}"; do [[ $pid == "$sentinel" ]] || ! is_owned "$pid"; done
printf 'PASS bounded TERM/KILL owned descendants\nPASS unowned/stale-token sentinel untouched\nPASS watchdog returns124 at deadline\n' > "$art/cleanup-tests.log"
# Dispose only of our negative control, after its survival was established.
owned=(); remember "$sentinel"; stop_owned
wait "$fixture_root" 2>/dev/null || true
wait "$sentinel" 2>/dev/null || true
trap - EXIT
for dir in vttest-x11.soyWA3 vttest-wayland.4pz8fm vttest-wayland.P7So6b; do
  [[ -d .build/evidence/$dir ]] || continue
  tests/verify.sh vttest-check measure ".build/evidence/$dir" > "$art/$dir.measure.log"
  if tests/verify.sh vttest-check accept ".build/evidence/$dir" > "$art/$dir.reject.log"; then
    echo 'Legacy baseline incorrectly accepted' >&2; exit 1
  fi
done
git diff --check
cat "$art/protocol-tests.log" "$art/recorder-tests.log" "$art/cleanup-tests.log" "$art/isolation-tests.log"
echo 'Offline/static/disposable harness checks passed; no GUI run.'
