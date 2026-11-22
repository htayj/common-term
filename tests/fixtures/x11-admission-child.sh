#!/usr/bin/env bash
set -euo pipefail
stty raw -echo
if [[ $1 == bracketed ]]; then printf '\033[?2004h'; fi
# Consume a terminal status reply before publishing readiness. The reply proves
# the terminal has processed everything through the mode-setting sequence.
printf '\033[5n'
reply=$(dd bs=1 count=4 status=none)
[[ $reply == $'\033[0n' ]]
exec 3> "$2"
printf 'ready\n' > "$3"
exec cat >&3
