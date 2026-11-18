#!/usr/bin/env bash
# Called by packaging-gate with its owned source-only checkout and sandbox.
set -euo pipefail
checkout=$1 runtime=$2 sandbox=$3
cat >"$sandbox/sbcl-probe" <<'PROBE'
#!/usr/bin/env bash
set -euo pipefail
[[ $1 == --no-userinit && $2 == --no-sysinit ]]
[[ $LISPTERM_GPU_RUNTIME == "$EXPECTED_RUNTIME" ]]
[[ $LISPTERM_FONT == "$EXPECTED_FONT" ]]
[[ $LISPTERM_XKBCOMMON_LIBRARY == "$EXPECTED_XKB" ]]
[[ ! ${CL_SOURCE_REGISTRY+x} && ! ${ASDF_OUTPUT_TRANSLATIONS+x} ]]
PROBE
chmod +x "$sandbox/sbcl-probe"
# Conflicting values prove precedence; absent and empty public values prove fallback.
for wrapper in scripts/sbcl bin/common-term bin/comterm bin/lispterm; do
 env -i PATH="$PATH" HOME="$sandbox/home" AUTOLITH_ASDF_CACHE="$sandbox/al-cache" \
  COMTERM_RUNTIME="$runtime" LISPTERM_RUNTIME=/nonexistent/legacy-runtime \
  COMTERM_SBCL="$sandbox/sbcl-probe" LISPTERM_SBCL=/nonexistent/legacy-sbcl \
  COMTERM_FONT=new-font LISPTERM_FONT=old-font \
  COMTERM_XKBCOMMON_LIBRARY=new-xkb LISPTERM_XKBCOMMON_LIBRARY=old-xkb \
  CL_SOURCE_REGISTRY=invalid ASDF_OUTPUT_TRANSLATIONS=invalid \
  EXPECTED_RUNTIME="$runtime" EXPECTED_FONT=new-font EXPECTED_XKB=new-xkb \
  "$checkout/$wrapper" --help
 env -i PATH="$PATH" HOME="$sandbox/home" AUTOLITH_ASDF_CACHE="$sandbox/al-cache" \
  LISPTERM_RUNTIME="$runtime" LISPTERM_SBCL="$sandbox/sbcl-probe" \
  LISPTERM_FONT=old-font LISPTERM_XKBCOMMON_LIBRARY=old-xkb \
  EXPECTED_RUNTIME="$runtime" EXPECTED_FONT=old-font EXPECTED_XKB=old-xkb \
  "$checkout/$wrapper" --help
 env -i PATH="$PATH" HOME="$sandbox/home" AUTOLITH_ASDF_CACHE="$sandbox/al-cache" \
  COMTERM_RUNTIME= COMTERM_SBCL= COMTERM_FONT= COMTERM_XKBCOMMON_LIBRARY= \
  LISPTERM_RUNTIME="$runtime" LISPTERM_SBCL="$sandbox/sbcl-probe" \
  LISPTERM_FONT=old-font LISPTERM_XKBCOMMON_LIBRARY=old-xkb \
  EXPECTED_RUNTIME="$runtime" EXPECTED_FONT=old-font EXPECTED_XKB=old-xkb \
  "$checkout/$wrapper" --help
done
# The Autolith wrapper keeps its pinned Lisp closure; only display resources vary.
for naming in public legacy empty-public; do
 if [[ $naming == public ]]; then
  settings=(COMTERM_RUNTIME="$runtime" LISPTERM_RUNTIME=/nonexistent/legacy-runtime
   COMTERM_FONT=new-font LISPTERM_FONT=old-font
   COMTERM_XKBCOMMON_LIBRARY=new-xkb LISPTERM_XKBCOMMON_LIBRARY=old-xkb
   EXPECTED_FONT=new-font EXPECTED_XKB=new-xkb)
 elif [[ $naming == empty-public ]]; then
  settings=(COMTERM_RUNTIME= COMTERM_FONT= COMTERM_XKBCOMMON_LIBRARY=
   LISPTERM_RUNTIME="$runtime" LISPTERM_FONT=old-font
   LISPTERM_XKBCOMMON_LIBRARY=old-xkb EXPECTED_FONT=old-font EXPECTED_XKB=old-xkb)
 else
  settings=(LISPTERM_RUNTIME="$runtime" LISPTERM_FONT=old-font
   LISPTERM_XKBCOMMON_LIBRARY=old-xkb EXPECTED_FONT=old-font EXPECTED_XKB=old-xkb)
 fi
 env -i PATH="$PATH" HOME="$sandbox/home" AUTOLITH_ASDF_CACHE="$sandbox/al-cache" \
  COMTERM_SBCL=/nonexistent/ordinary-sbcl LISPTERM_SBCL=/nonexistent/legacy-sbcl \
  EXPECTED_RUNTIME="$runtime" "${settings[@]}" \
  "$checkout/scripts/autolith-runtime" --non-interactive \
  --eval '(require :asdf)' \
  --eval '(dolist (pair (list (cons "LISPTERM_FONT" "EXPECTED_FONT") (cons "LISPTERM_XKBCOMMON_LIBRARY" "EXPECTED_XKB") (cons "LISPTERM_GPU_RUNTIME" "EXPECTED_RUNTIME"))) (assert (equal (uiop:getenv (car pair)) (uiop:getenv (cdr pair)))))' \
  --eval '(assert (asdf:find-system "autolith" nil))'
done
# Exercise real font parsing and xkb loading both directly and through real wrappers.
# Conflicting invalid paths distinguish precedence from accidental default loading.
for entry in direct scripts/sbcl scripts/autolith-runtime; do
 for naming in public legacy empty-public; do
  font="$runtime/share/fonts/terminus/ter-u16n.otb"
  xkb="$runtime/lib/libxkbcommon.so.0"
  case $naming in
   public) settings=(COMTERM_FONT="$font" LISPTERM_FONT=/nonexistent/legacy-font
    COMTERM_XKBCOMMON_LIBRARY="$xkb" LISPTERM_XKBCOMMON_LIBRARY=/nonexistent/legacy-xkb) ;;
   legacy) settings=(LISPTERM_FONT="$font" LISPTERM_XKBCOMMON_LIBRARY="$xkb") ;;
   empty-public) settings=(COMTERM_FONT= COMTERM_XKBCOMMON_LIBRARY=
    LISPTERM_FONT="$font" LISPTERM_XKBCOMMON_LIBRARY="$xkb") ;;
  esac
  if [[ $entry == direct ]]; then
   command=("$runtime/bin/sbcl" --no-userinit --no-sysinit)
  else
   command=("$checkout/$entry")
  fi
  env -i PATH="$PATH" HOME="$sandbox/home" AUTOLITH_ASDF_CACHE="$sandbox/al-cache" \
   LISPTERM_RUNTIME="$runtime" LISPTERM_SBCL="$runtime/bin/sbcl" \
   EXPECTED_FONT="$font" EXPECTED_XKB="$xkb" "${settings[@]}" \
   "${command[@]}" --non-interactive --load "$checkout/scripts/bootstrap.lisp" \
   --eval '(asdf:load-system "common-term/display-wayland")' \
   --eval '(let ((font (common-term.display::load-font nil 16))) (assert (equal (truename (common-term.display::bitmap-font-path font)) (truename (uiop:getenv "EXPECTED_FONT")))))' \
   --eval '(let ((library (common-term.display.wayland::ensure-xkb-library))) (assert (equal (truename (cffi:foreign-library-pathname library)) (truename (uiop:getenv "EXPECTED_XKB")))))'
  printf 'Real environment control: %s %s PASS\n' "$entry" "$naming"
 done
done
echo 'Environment aliases: precedence, legacy fallback, empty public fallback and real Lisp entry points PASS'
