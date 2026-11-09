#!/usr/bin/env bash
# Non-GUI behavioral checks of the gate's direct-child wait, reap, log and
# receipt functions, sourced from tests/vttest-gate.sh with FUNCTIONS_ONLY.
# Only this script's own short-lived children are signalled or waited.
# VTTEST_GATE_SOURCE may name another copy of the gate, e.g. a pre-fix revision.
LISPTERM_VTTEST_FUNCTIONS_ONLY=1 source "${VTTEST_GATE_SOURCE:-$(dirname "$0")/vttest-gate.sh}"
set +e
tmp=$(mktemp -d "${TMPDIR:-/tmp}/gate-cleanup-wait.XXXXXX") && tmp=$(realpath -e -- "$tmp") || exit 2
trap 'rm -rf -- "$tmp"' EXIT
failures=0 n=0
check() { # description, then a command that must succeed
  local what=$1; shift; n=$((n+1))
  if "$@"; then printf 'ok %s - %s\n' "$n" "$what"
  else printf 'not ok %s - %s\n' "$n" "$what"; failures=$((failures+1)); fi
}
same() { [[ $1 == "$2" ]] || { printf '#   expected [%s]\n#   observed [%s]\n' "$2" "$1"; return 1; }; }
status_is() { local want=$1; shift; "$@"; same "$?" "$want"; }
log_is() { same "$(cat "$art/cleanup.log")" "$1"; }
receipt_has() { grep -qxF -- "$1" "$art/cleanup-receipt.sexp" || { sed 's/^/#   /' "$art/cleanup-receipt.sexp"; return 1; }; }
# Fresh gate state and a valid launch binding for each scenario.
scenario() {
  owned=() audited=() roles=() direct_children=() reaped=() identity_conflicts=()
  adoption_failures=() identity_failures=()
  art=$tmp/$1/recorder; mkdir -p -- "$art"
  printf '(:request %s)\n' "$1" > "$art/area-request.sexp"
  area_request_sha=$(sha256sum "$art/area-request.sexp"); area_request_sha=${area_request_sha%% *}
  backend=x11 runtime='' gate_pid=$$ gate_start=$(process_token $$)
  LISPTERM_VTTEST_TITLE=Lispterm-vttest-cleanup-wait-$$
  echo "# scenario $1"
}
# Start an owned direct fixture child: role, exit status, lifetime.
spawn() {
  ( sleep "$3"; exit "$2" ) & child=$!
  token=$(process_token "$child") && remember_role "$1" "$child" direct
}
# Poll without waiting until the fixture is no longer live.
await_exit() {
  local i
  for ((i=0;i<250;i++)); do [[ $(owned_state "$1") == live ]] || return 0; sleep .02; done
  return 1
}
# Each scenario owns one PID, so its receipt entry line also closes :owned.
entry() { printf '  (:role "%s" :pid %s :start "%s" :state :%s :observed :%s :direct t :wait-status %s))' "$@"; }
# Count real wait builtin calls made by the gate functions.
waits=0; wait() { waits=$((waits+1)); builtin wait "$@"; }

scenario ordinary
spawn client 7 0.2; c=$child t=$token
check 'fixture adopted as direct client' same "${roles[$c]}:${direct_children[$c]}:${owned[$c]}" "client:1:$t"
check 'fixture exits before any wait' await_exit "$c"
waits=0; reap_direct_children
check 'reap waited the child once' same "$waits" 1
check 'reap saved the actual exit status' same "${reaped[$c]:-none}" 7
check 'cached wait reused, not synthetic 127' status_is 7 wait_direct_child "$c"
check 'cached wait made no new wait' same "$waits" 1
check 'wait log succeeds' status_is 0 log_direct_child_waits
check 'exact WAIT line' log_is "WAIT role=client pid=$c start=$t status=7 result=waited"
check 'receipt verifies cleanup' status_is 0 fixture_cleanup_receipt 0
check 'receipt state stopped' receipt_has ' :state :stopped :stop-owned :all-stopped :runtime :none'
check 'receipt entry' receipt_has "$(entry client "$c" "$t" stopped absent 7)"
check 'receipt is exclusive' status_is 2 fixture_cleanup_receipt 0

scenario status127
spawn client 127 0.1; c=$child t=$token
check 'fixture exits before any wait' await_exit "$c"
reap_direct_children
check 'real 127 saved' same "${reaped[$c]:-none}" 127
check '127 fails the wait log' status_is 1 log_direct_child_waits
check 'WAIT line unknown' log_is "WAIT role=client pid=$c start=$t status=127 result=unknown"
check '127 refuses the receipt' status_is 1 fixture_cleanup_receipt 0
check 'receipt state unknown' receipt_has ' :state :unknown :stop-owned :all-stopped :runtime :none'
check 'receipt entry' receipt_has "$(entry client "$c" "$t" unknown absent 127)"

scenario missing
sleep 5 & c=$! t=$(process_token "$c"); remember_role helper "$c" direct
reap_direct_children
check 'live child not waited' same "${reaped[$c]:-none}" none
check 'missing wait fails the log' status_is 1 log_direct_child_waits
check 'WAIT line unknown' log_is "WAIT role=helper pid=$c start=$t status=none result=unknown"
check 'live child fails the receipt' status_is 1 fixture_cleanup_receipt 0
check 'receipt state failed' receipt_has ' :state :failed :stop-owned :all-stopped :runtime :none'
check 'receipt entry' receipt_has "$(entry helper "$c" "$t" failed live nil)"
is_owned "$c" && kill -TERM "$c"; wait "$c" 2>/dev/null
check 'own fixture stopped' same "$(owned_state "$c")" absent

scenario unknown
# A non-numeric simulated identity: owned_state reports unknown without /proc.
owned[sim]=1 roles[sim]=simulated direct_children[sim]=1
check 'simulated state unknown' same "$(owned_state sim)" unknown
reap_direct_children
check 'unknown not waited' same "${reaped[sim]:-none}" none
check 'unknown fails the log' status_is 1 log_direct_child_waits
check 'WAIT line unknown' log_is 'WAIT role=simulated pid=sim start=1 status=none result=unknown'
check 'unknown refuses the receipt' status_is 1 fixture_cleanup_receipt 0
check 'receipt entry' receipt_has "$(entry simulated sim 1 unknown unknown nil)"

scenario mismatch
spawn client 5 0.1; c=$child t=$token
remember "$c" "$((t + 1))"
check 'conflict recorded' same "${identity_conflicts[$c]:-}" 1
waits=0; check 'mismatch refused' status_is 127 wait_direct_child "$c"
check 'mismatch made no wait' same "$waits" 0
check 'fixture exits' await_exit "$c"
reap_direct_children; check 'mismatch reap made no wait' same "$waits" 0
check 'mismatch never reaped' same "${reaped[$c]:-none}" none
check 'mismatch fails the log' status_is 1 log_direct_child_waits
check 'WAIT line unknown' log_is "WAIT role=client pid=$c start=$t status=none result=unknown"
check 'mismatch refuses the receipt' status_is 1 fixture_cleanup_receipt 0
check 'receipt entry' receipt_has "$(entry client "$c" "$t" unknown absent nil)"
check 'identity failure listed' grep -qF ":pid $c :start \"$t\" :observed-start \"$((t + 1))\" :reason :pid-reused" "$art/cleanup-receipt.sexp"
builtin wait "$c"; check 'gate left the real status unconsumed' same "$?" 5

scenario logfail
spawn client 0 0.1; c=$child t=$token
check 'fixture exits' await_exit "$c"
reap_direct_children
check 'zero status cached' same "${reaped[$c]:-none}" 0
# A directory in place of the log makes the append fail, even for root.
mkdir -- "$art/cleanup.log"
check 'unwritable WAIT log fails' status_is 1 log_direct_child_waits 2>/dev/null

echo "# $((n - failures))/$n passed"
((failures == 0))
