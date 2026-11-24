#!/usr/bin/env bash
# Exercise the launcher against an owned, authenticated Xvfb display.
set -euo pipefail
cd "$(dirname "$0")/.."
exec </dev/null
unset DISPLAY XAUTHORITY WAYLAND_DISPLAY WAYLAND_SOCKET SWAYSOCK XDG_RUNTIME_DIR
export LC_ALL=C.UTF-8
for tool in Xvfb xdotool timeout xauth mcookie; do
 command -v "$tool" >/dev/null || { echo "x11-poll prerequisite: missing $tool" >&2; exit 1; }
done
umask 077
mkdir -p .build/x11-poll
art=$(mktemp -d "$PWD/.build/x11-poll/run.XXXXXX")
printf 'X11 poll artifacts: %s\n' "$art"
server= terminal= stopped=0
fail() { echo "$*; see $art" >&2; exit 1; }
cleanup() {
 if [[ -n $terminal ]]; then
  if ((stopped)); then kill -CONT "$terminal" 2>/dev/null || :; fi
  kill -KILL "$terminal" 2>/dev/null || :
  wait "$terminal" 2>/dev/null || :
 fi
 if [[ -n $server ]]; then
  kill -TERM "$server" 2>/dev/null || :
  wait "$server" 2>/dev/null || :
 fi
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM
# Skip both filesystem and abstract sockets. If another server wins the
# race to bind this number, fail without touching its process or files.
number=
for n in {100..199}; do
 socket=/tmp/.X11-unix/X$n
 [[ ! -e $socket && ! -L $socket && ! -e /tmp/.X$n-lock && ! -L /tmp/.X$n-lock ]] || continue
 if awk -v path="$socket" '$8 == path || $8 == "@" path { found=1 } END { exit !found }' /proc/net/unix; then continue; fi
 number=$n
 break
done
[[ -n $number ]] || fail 'No free high X11 display number'
printf '%s\n' "$number" > "$art/display-number"
export DISPLAY=:$number XAUTHORITY=$art/Xauthority
: > "$XAUTHORITY"
# Pass the cookie on stdin and exclude the authority file from transcripts.
printf 'add %s MIT-MAGIC-COOKIE-1 %s\n' "$DISPLAY" "$(mcookie)" | xauth -q -f "$XAUTHORITY"
Xvfb "$DISPLAY" -screen 0 1024x768x24 -nolisten tcp -auth "$XAUTHORITY" -noreset \
 > "$art/xvfb.log" 2>&1 &
server=$!
printf '%s\n' "$server" > "$art/xvfb.pid"
lock=/tmp/.X$number-lock
ready=0
for ((i=0; i<200; i++)); do
 kill -0 "$server" 2>/dev/null || fail 'Private Xvfb exited during startup'
 if [[ -S $socket && -r $lock && $(tr -d '[:space:]' < "$lock") == "$server" ]] &&
    grep -zFxq -- "$DISPLAY" "/proc/$server/cmdline" &&
    grep -zFxq -- "$XAUTHORITY" "/proc/$server/cmdline" &&
    timeout 1s xdotool getdisplaygeometry > "$art/xvfb.geometry" 2>/dev/null; then
  ready=1
  break
 fi
 sleep .02
done
((ready)) || fail 'Owned, authenticated Xvfb did not become ready'
bin/common-term --backend x11 --renderer cpu --class T -- bash -c '
 stty -echo
 printf "%s\n" "$$" > "$1/child.pid"
 while IFS= read -r line; do
  printf "%s\n" "$line"
  printf "%s\n" "$line" >> "$1/echoed"
 done
' child "$art" > "$art/terminal.stdout" 2> "$art/terminal.stderr" &
terminal=$!
printf '%s\n' "$terminal" > "$art/terminal.pid"
window=$(timeout 30s xdotool search --sync --onlyvisible --limit 1 --class '^T$') || fail 'Class T window not mapped'
xd() { timeout 5s xdotool "$@"; }
xd windowfocus "$window"
check_echo() {
 local token=$1
 xd type --clearmodifiers --delay 10 "$token"
 xd key --clearmodifiers Return
 for ((i=0; i<250; i++)); do
  grep -Fxq "$token" "$art/echoed" 2>/dev/null && return
  kill -0 "$terminal" 2>/dev/null || fail "Terminal exited before $token"
  sleep .02
 done
 fail "Child did not echo $token"
}
check_echo before-pause
# Signal only the launcher we own. Its scripts exec into the SBCL process.
# The unit test deterministically covers an overlong in-flight poll. This
# test stops an idle, fully running terminal on a real X connection.
stopped=1
kill -STOP "$terminal"
for ((i=0; i<100; i++)); do
 if grep -Eq '^State:[[:space:]]+T' "/proc/$terminal/status"; then break; fi
 sleep .01
done
grep -Eq '^State:[[:space:]]+T' "/proc/$terminal/status" || fail 'Terminal did not stop'
sleep 2
kill -CONT "$terminal"
stopped=0
xd getwindowname "$window" > "$art/window-after-pause"
check_echo after-pause
mapfile -t echoed < "$art/echoed"
[[ ${#echoed[@]} == 2 && ${echoed[0]} == before-pause && ${echoed[1]} == after-pause ]] || fail 'Unexpected child input/echo sequence'
printf '%s\n' 'PASS: class T window and input echo survived SIGSTOP/SIGCONT'
# Induce real EOF or a socket error, then require prompt launcher exit.
kill -TERM "$server"
wait "$server" || :
server=
for ((i=0; i<250; i++)); do
 kill -0 "$terminal" 2>/dev/null || break
 sleep .02
done
! kill -0 "$terminal" 2>/dev/null || fail 'Launcher did not exit promptly after Xvfb stopped'
status=0
wait "$terminal" || status=$?
terminal=
printf '%s\n' "$status" > "$art/terminal.status"
[[ $status == 1 ]] || fail "Expected launcher display-error exit 1, got $status"
grep -E '^common-term: display error: .*Display: ' "$art/terminal.stderr" || fail 'Missing launcher display-error diagnostic'
printf '%s\n' 'PASS: real X11 transport loss ended the launcher'
