#!/usr/bin/env bash
# Load the delivered runtime without user init, Quicklisp, or previous caches.
set -euo pipefail
cd "$(dirname "$0")/.."
root=$PWD
runtime=$(realpath "${COMTERM_RUNTIME:-${LISPTERM_RUNTIME:-$root/.build/deps}}")
[[ -x $runtime/bin/sbcl ]] || { echo 'Run scripts/bootstrap-deps first' >&2; exit 1; }
[[ -f $runtime/share/fonts/terminus/ter-u16n.otb && -f $runtime/lib/libxkbcommon.so.0 ]]
[[ -f $runtime/lib/libEGL.so.1 && -f $runtime/lib/libGLESv2.so.2 && -f $runtime/lib/libgbm.so.1 ]]
[[ -f $runtime/share/glvnd/egl_vendor.d/50_mesa.json && -d $runtime/lib/dri && -f $runtime/lib/gbm/dri_gbm.so ]]
mkdir -p .build/evidence
sandbox=$(mktemp -d "$root/.build/packaging.XXXXXX")
trap 'rm -rf "$sandbox"' EXIT
mkdir -p "$sandbox"/{home,config,data,state,cache}
# Exercise only deliverable source inputs, without repository metadata or caches.
mkdir -p "$sandbox/checkout"
cp -R lispterm.asd common-term.asd src engine vendor scripts bin "$sandbox/checkout/"
[[ ! -e $sandbox/checkout/.git && ! -e $sandbox/checkout/.build ]]
# This retains only the executable search path; no inherited Lisp or user config.
(
 cd "$sandbox/checkout"
env -i PATH="$PATH" HOME="$sandbox/home" XDG_CONFIG_HOME="$sandbox/config" \
 XDG_DATA_HOME="$sandbox/data" XDG_STATE_HOME="$sandbox/state" XDG_CACHE_HOME="$sandbox/cache" \
 COMTERM_RUNTIME="$runtime" COMTERM_SBCL="$runtime/bin/sbcl" \
 LISPTERM_RUNTIME=/nonexistent/legacy-runtime LISPTERM_SBCL=/nonexistent/legacy-sbcl \
 "$sandbox/checkout/scripts/sbcl" --non-interactive --load "$sandbox/checkout/scripts/bootstrap.lisp" \
 --eval '(asdf:load-system "common-term/launcher")' \
 --eval '(asdf:load-system "common-term/gpu")' \
 --eval '(assert (null lispterm.gpu::*native-libraries*))' \
 --eval '(assert (null lispterm.gpu::*live-contexts*))' \
 --eval '(assert (lispterm.gpu:ensure-native-libraries))' \
 --eval '(assert (null lispterm.gpu::*live-contexts*))' \
 --eval '(assert (probe-file (uiop:getenv "LISPTERM_FONT")))' \
 --eval '(assert (lispterm.display::load-font nil 16))' \
 --eval '(assert (lispterm.display.wayland::ensure-xkb-library))' \
 --eval '(assert (eq (find-package "LISPTERM") (find-package "COMMON-TERM")))' \
 --eval '(assert (eq (find-package "LISPTERM") (find-package "COMTERM")))' \
 --eval '(dolist (suffix (list "ENGINE.COMPAT" "ENGINE" "TRACE" "PTY" "DISPLAY" "DISPLAY.X11" "DISPLAY.WAYLAND" "GPU")) (assert (eq (find-package (concatenate (quote string) "LISPTERM." suffix)) (find-package (concatenate (quote string) "COMMON-TERM." suffix)))))' \
 --eval '(assert (zerop (hash-table-count lispterm::*hosts*)))' \
 --eval '(format t "PACKAGING clean source-only runtime, font parser, xkb load, inert ASDF load PASS~%")'
) > .build/evidence/packaging.log 2>&1 || { cat .build/evidence/packaging.log; exit 1; }
tail -8 .build/evidence/packaging.log
# Exercise all delivered entry points from the same source-only clean home.
for launcher in common-term comterm lispterm; do
 env -i PATH="$PATH" HOME="$sandbox/home" XDG_CONFIG_HOME="$sandbox/config" \
  XDG_DATA_HOME="$sandbox/data" XDG_STATE_HOME="$sandbox/state" XDG_CACHE_HOME="$sandbox/cache" \
  COMTERM_RUNTIME="$runtime" COMTERM_SBCL="$runtime/bin/sbcl" \
  "$sandbox/checkout/bin/$launcher" --help >"$sandbox/$launcher.out" 2>"$sandbox/$launcher.err"
done
bash tests/rebrand-env-tests.sh "$sandbox/checkout" "$runtime" "$sandbox"
echo 'PACKAGING public and legacy launchers, package aliases and environment precedence PASS'
for file in scripts/* tests/*.sh bin/lispterm bin/common-term bin/comterm; do
 [[ -f $file ]] || continue
 if [[ $file == *.sh || $(head -n 1 "$file") == '#!/usr/bin/env bash' ]]; then
  bash -n "$file"
 fi
done
git diff --check
