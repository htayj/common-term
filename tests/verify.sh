#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
root=$PWD
# The X11 harness invokes packaged sbcl directly; avoid inherited wrapper state.
unset CL_SOURCE_REGISTRY ASDF_OUTPUT_TRANSLATIONS
export LISPTERM_SBCL=${LISPTERM_SBCL:-$root/.build/deps/bin/sbcl}
if [[ -d $root/.build/test-tools/bin ]]; then
 export PATH="$root/.build/test-tools/bin:$PATH"
 export FASTFETCH=${FASTFETCH:-$root/.build/test-tools/bin/fastfetch}
fi
gate=${1:-}; shift "$(( $# > 0 ? 1 : 0 ))"
case $gate in
 deps) exec scripts/sbcl --non-interactive --load scripts/bootstrap.lisp \
  --eval '(dolist (name (quote ("sb-bsd-sockets" "babel" "cffi" "bordeaux-threads" "alexandria" "fiveam" "zpng" "cl-weave" "xclhb" "struct+"))) (asdf:load-system name) (format t "DEPENDENCY ~A OK~%" name))' ;;
 unit|self-check)
  scripts/sbcl --script tests/run-engine-tests.lisp
  scripts/sbcl --script tests/run-pty-tests.lisp
  scripts/sbcl --script tests/run-display-tests.lisp
  scripts/sbcl --script tests/run-host-tests.lisp
  scripts/sbcl --script tests/run-vt100-input-tests.lisp
  scripts/sbcl --script tests/run-vt100-rendition-tests.lisp
  scripts/sbcl --script tests/run-vt100-geometry-tests.lisp
  exec scripts/sbcl --script tests/run-host-input-tests.lisp ;;
 pty)
  scripts/sbcl --script tests/run-pty-tests.lisp
  exec scripts/sbcl --script tests/run-pty-close-tests.lisp ;;
 display)
  scripts/sbcl --script tests/run-display-tests.lisp
  scripts/sbcl --script tests/run-xkb-tests.lisp
  exec scripts/sbcl --script tests/run-x11-close-tests.lisp ;;
 host) exec scripts/sbcl --script tests/run-host-tests.lisp ;;
 geometry) exec scripts/sbcl --script tests/run-vt100-geometry-tests.lisp ;;
 native-geometry) exec tests/vt100-native-geometry.sh "$@" ;;
 pty-spike)
  log=$(mktemp)
  trap 'rm -f "$log"' EXIT
  scripts/sbcl --script tests/spike/pty-spike.lisp "$log"
  cat "$log"
  grep -q ':OWN-FDS :OK T' "$log" ;;
 x11) exec tests/x11-gate.sh "$@" ;;
 wayland) exec tests/wayland-gate.sh "$@" ;;
 same-heap|active-image) exec tests/autolith-gate.sh "${1:-wayland}" ;;
 recovery)
  tests/host-native.sh x11
  exec tests/host-native.sh wayland ;;
 integration)
  scripts/autolith-runtime --script tests/run-runtime-adoption-tests.lisp
  exec tests/checkpoint-gate.sh "$@" ;;
 fastfetch-x11) exec tests/fastfetch-gate.sh x11 ;;
 fastfetch-wayland) exec tests/fastfetch-gate.sh wayland ;;
 vttest-x11) exec tests/vttest-gate.sh x11 ;;
 vttest-wayland) exec tests/vttest-gate.sh wayland ;;
 vttest-check) exec scripts/sbcl --script tests/vttest-protocol.lisp --vttest-check "$@" ;;
 vttest-harness) exec tests/vttest-harness-tests.sh ;;
 packaging) exec tests/packaging-gate.sh "$@" ;;
 git) exec tests/git-gate.sh "$@" ;;
 *) echo "Unknown verification gate: ${gate:-<none>}" >&2; exit 2 ;;
esac
