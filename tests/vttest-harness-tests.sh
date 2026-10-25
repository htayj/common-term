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
export LISPTERM_SBCL=${LISPTERM_SBCL:-$root/.build/deps/bin/sbcl}
export LISPTERM_HARNESS_TEST_ARTIFACTS="$art/fixtures-$(date +%s%N)/"
export LISPTERM_VTTEST_ARTIFACTS="$LISPTERM_HARNESS_TEST_ARTIFACTS"
scripts/sbcl --script tests/vttest-protocol-tests.lisp > "$art/protocol-tests.log" 2>&1
scripts/sbcl --script tests/vttest-recorder-tests.lisp > "$art/recorder-tests.log" 2>&1
bash -n tests/vttest-gate.sh tests/vttest-harness-tests.sh tests/verify.sh
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
cat "$art/protocol-tests.log" "$art/recorder-tests.log" "$art/cleanup-tests.log"
echo 'Offline/static/disposable harness checks passed; no GUI run.'
