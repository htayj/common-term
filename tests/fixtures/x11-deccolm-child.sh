#!/usr/bin/env bash
set -euo pipefail
root=$PWD
winsize() {
 local output
 output=$("$root/scripts/sbcl" --script "$root/tests/fixtures/tty-winsize.lisp") || return
 # Some SBCL wrappers print a startup banner even in script mode.
 printf '%s\n' "${output##*$'\n'}"
}
cd "$1"
# Sample before changing tty modes or issuing the first terminal query.
winsize > initial-winsize.tmp
mv -- initial-winsize.tmp initial-winsize
stty raw -echo

terminal_barrier() {
 printf '\033[5n'
 local reply
 IFS= read -r -N 4 -t 5 reply
 [[ $reply == $'\033[0n' ]]
}

write_probe() {
  # A full dd record is written to the PTY in one write, even if the pipe splits it.
  printf '%s' "$1" | dd bs="${#1}" count=1 iflag=fullblock status=none
}

# Commands and replies use files, not the terminal byte stream. In particular,
# size queries must not wait for this terminal to service its own input/output.
terminal_barrier
: > ready
for ((id=1;;id++)); do
 while [[ ! -f command.$id ]]; do sleep 0.005; done
 IFS= read -r command < "command.$id"
 case $command in
  set) printf '\033[?3h'; result=sent ;;
  reset) printf '\033[?3l'; result=sent ;;
  size) result=$(winsize) ;;
   set-cpr) write_probe $'\033[?3h\033[6n'; result=sent ;;
   reset-cpr) write_probe $'\033[?3l\033[6n'; result=sent ;;
   refusal-cpr) write_probe $'\033[?3h\033[1;100H\033[6n'; result=sent ;;
   cpr-ready)
    # Bash read with timeout zero checks readability without consuming any bytes.
    if IFS= read -r -t 0; then result=readable; else result=pending; fi
    ;;
   sample-cpr)
    result=pending
    if IFS= read -r -t 0; then
     IFS= read -r -d R -t 2 cursor
     size=$(winsize)
     [[ $cursor =~ ^$'\033['([0-9]+\;[0-9]+)$ ]]
     result="${BASH_REMATCH[1]} $size"
    fi
    ;;
  quit) exit 0 ;;
  *) exit 2 ;;
 esac
 printf '%s\n' "$result" > reply.tmp
 mv -- reply.tmp "reply.$id"
done
