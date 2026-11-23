#!/usr/bin/env bash
set -euo pipefail
cd "$1"
stty raw -echo

terminal_barrier() {
 printf '\033[5n'
 local reply
 IFS= read -r -N 4 -t 5 reply
 [[ $reply == $'\033[0n' ]]
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
  size) result=$(stty size) ;;
   refusal-cursor)
    # An 80-column engine clamps this position to column 80; a wide engine does not.
    printf '\033[1;100H\033[6n'
    IFS= read -r -d R -t 5 cursor
    result=${cursor#$'\033['}
    ;;
  quit) exit 0 ;;
  *) exit 2 ;;
 esac
 printf '%s\n' "$result" > reply.tmp
 mv -- reply.tmp "reply.$id"
done
