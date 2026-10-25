#!/usr/bin/env bash
# Run after the final local signed commit. This script never edits Git config.
set -euo pipefail
cd "$(dirname "$0")/.."
git diff --check
[[ -z $(git status --porcelain --untracked-files=all) ]] || {
 echo 'Git gate requires a clean committed tree' >&2; exit 1;
}
while read -r name url direction; do
 url=${url,,}
 if [[ $url =~ (^|[/:@])github\.com([/:]|$) &&
       ! $url =~ ^(git@github\.com:|ssh://([^/@]+@)?github\.com(:[0-9]+)?/) ]]; then
  echo 'GitHub remotes must use SSH' >&2; exit 1
 fi
done < <(git remote -v)
fingerprint=D0BEDCD5B63A4040FBC5BC00A6398F53FCEC1C10
verifier=${LISPTERM_GPG:-/home/taylor/.local/share/agent-signing/gpg}
if [[ -x $verifier ]]; then
 status=$(git -c "gpg.program=$verifier" verify-commit --raw HEAD 2>&1)
else
 status=$(git verify-commit --raw HEAD 2>&1)
fi
printf '%s\n' "$status" | awk -v fingerprint="$fingerprint" '
 $1 == "[GNUPG:]" && $2 == "VALIDSIG" && ($3 == fingerprint || $NF == fingerprint) { valid=1 }
 END { exit !valid }' || { echo 'HEAD signature does not match the required fingerprint' >&2; exit 1; }
printf 'GIT clean tree, verified signature %s, SSH GitHub remotes PASS\n' "$fingerprint"
