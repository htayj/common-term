#!/usr/bin/env bash
# Closed keyboard adapter CLI. No gate environment overrides are inherited.
set -euo pipefail
if (( $# != 3 )); then echo 'Usage: keyboard-native.sh x11|wayland RUN MANIFEST' >&2; exit 2; fi
backend=$1 run=$2 manifest=$3
[[ $backend == x11 || $backend == wayland ]] || exit 2
[[ $run == /* && $run != / && $run != */ && -d $run && ! -L $run ]] || exit 2
[[ $(realpath -e -- "$run") == "$run" && $(stat -c %u -- "$run") == "$(id -u)" ]] || exit 2
perm=$(stat -c %a -- "$run")
(( (8#$perm & 8#022) == 0 )) || exit 2
[[ $manifest == "$run/MANIFEST.sexp" && -f $manifest && ! -L $manifest ]] || exit 2
root=$(cd -- "$(dirname -- "$0")/.." && pwd -P)
cd -- "$root"
exec /run/current-system/sw/bin/env -i PATH=/run/current-system/sw/bin:/usr/bin:/bin \
  "$root/scripts/sbcl" --non-interactive --load tests/vttest-area.lisp \
  --load tests/vttest-keyboard-launch.lisp --load tests/vttest-keyboard-native-main.lisp \
  --eval '(let ((args (cdr sb-ext:*posix-argv*))) (sb-ext:exit :code (apply (function vttest-keyboard-driver::native-main) args)))' \
  --end-toplevel-options "$backend" "$run" "$manifest"
