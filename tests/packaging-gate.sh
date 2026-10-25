#!/usr/bin/env bash
# Load the delivered runtime without user init, Quicklisp, or previous caches.
set -euo pipefail
cd "$(dirname "$0")/.."
root=$PWD
runtime=$(realpath "${LISPTERM_RUNTIME:-$root/.build/deps}")
[[ -x $runtime/bin/sbcl ]] || { echo 'Run scripts/bootstrap-deps first' >&2; exit 1; }
[[ -f $runtime/share/fonts/terminus/ter-u16n.otb && -f $runtime/lib/libxkbcommon.so.0 ]]
mkdir -p .build/evidence
sandbox=$(mktemp -d "$root/.build/packaging.XXXXXX")
trap 'rm -rf "$sandbox"' EXIT
mkdir -p "$sandbox"/{home,config,data,state,cache}
# Exercise only deliverable source inputs, without repository metadata or caches.
mkdir -p "$sandbox/checkout"
cp -R lispterm.asd src engine vendor scripts "$sandbox/checkout/"
[[ ! -e $sandbox/checkout/.git && ! -e $sandbox/checkout/.build ]]
# This retains only the executable search path; no inherited Lisp or user config.
(
 cd "$sandbox/checkout"
env -i PATH="$PATH" HOME="$sandbox/home" XDG_CONFIG_HOME="$sandbox/config" \
 XDG_DATA_HOME="$sandbox/data" XDG_STATE_HOME="$sandbox/state" XDG_CACHE_HOME="$sandbox/cache" \
 LISPTERM_RUNTIME="$runtime" LISPTERM_SBCL="$runtime/bin/sbcl" \
 "$sandbox/checkout/scripts/sbcl" --non-interactive --load "$sandbox/checkout/scripts/bootstrap.lisp" \
 --eval '(asdf:load-system "lispterm/launcher")' \
 --eval '(assert (probe-file (uiop:getenv "LISPTERM_FONT")))' \
 --eval '(assert (lispterm.display::load-font nil 16))' \
 --eval '(assert (lispterm.display.wayland::ensure-xkb-library))' \
 --eval '(assert (find-package "LISPTERM"))' \
 --eval '(assert (zerop (hash-table-count lispterm::*hosts*)))' \
 --eval '(format t "PACKAGING clean source-only runtime, font parser, xkb load, inert ASDF load PASS~%")'
) > .build/evidence/packaging.log 2>&1 || { cat .build/evidence/packaging.log; exit 1; }
tail -8 .build/evidence/packaging.log
for file in scripts/* tests/*.sh bin/lispterm; do
 [[ -f $file ]] || continue
 if [[ $file == *.sh || $(head -n 1 "$file") == '#!/usr/bin/env bash' ]]; then
  bash -n "$file"
 fi
done
git diff --check
