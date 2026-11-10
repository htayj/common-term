#!/usr/bin/env bash
# Narrow root/helper/CWD prefix probe, NOT transitive or native confinement.
# Requires independent source-time review and immutable prepared exact copies.
# No files/receipts written. No coverage of direct wrappers or relative sourcing.
set -uo pipefail
[[ $# == 7 ]] || { printf 'usage: ROOT RUN MANIFEST LAUNCHER screen|double baseline|candidate normal|misplaced\n' >&2; exit 2; }
fixture=$1; fixture_run=$2; fixture_manifest=$3; launcher=$4
family=$5; generation=$6; placement=$7
control=${BASH_SOURCE[0]}
fail() { printf 'control refusal: %s\n' "$*" >&2; exit 90; }
for path in "$fixture" "$fixture_run" "$fixture_manifest"; do
    [[ $path == /* && $(readlink -f -- "$path") == "$path" && ! -L $path ]] || fail 'noncanonical input'
done
[[ -d $fixture && $fixture != /home/taylor/projects/lispterm && -d $fixture_run && -O $fixture_run && -f $fixture_manifest ]] || fail 'fixture type/ownership'
[[ $fixture_run == "$fixture/run" && $fixture_manifest == "$fixture_run/MANIFEST.sexp" ]] || fail 'fixture layout'
case $family:$generation in
screen:candidate)
    hash=75e537184913bb8a00f0e01fc3b56832d5e540b6e5b8211bc024e0aa2027d5a8
    footer=408; cdline=251; rootcheck=236
    safe=' 231 233 234 235 236 237 238 240 241 244 245 246 247 248 '
    skip=' 242 243 249 250 '; function_name=screen_native_main ;;
screen:baseline)
    hash=72063612230ffa6c4bb8568320bf9de1957853f8964524b2217b8b51750df5e4
    footer=404; cdline=247; rootcheck=0
    safe=' 231 232 233 235 236 239 240 241 242 243 246 '
    skip=' 237 238 244 245 '; function_name=screen_native_main ;;
double:candidate)
    hash=c203bb44b7abce82b7c044e06df2b2ecc1ba7d14c0be82d5bd2eee6bec4d90fe
    footer=427; cdline=246; rootcheck=232
    safe=' 227 229 230 231 232 233 234 236 237 239 240 241 242 243 '
    skip=' 238 244 245 '; function_name=double_size_native_main ;;
double:baseline)
    hash=320f54ff3bf8909f4dfceeab43e5b894a57a1a0175eac0b6fb05b1c478e96073
    footer=423; cdline=242; rootcheck=0
    safe=' 227 228 229 231 232 234 235 236 237 238 241 '
    skip=' 233 239 240 '; function_name=double_size_native_main ;;
*) fail 'unknown reviewed input' ;;
esac
if [[ $family == screen ]]; then filename=vttest-screen-native.sh
else filename=vttest-double-size-native.sh; fi
case $placement in
normal) canonical=$fixture/tests/$filename ;;
misplaced) [[ $generation == candidate ]] || fail 'baseline has no placement refusal'; canonical=$fixture/misplaced/$filename ;;
*) fail 'unknown placement' ;;
esac
[[ $launcher == /* && -f $launcher && $(readlink -f -- "$launcher") == "$canonical" ]] || fail 'launcher containment'
# Read launcher bytes only. Never read manifest contents or any helper/module.
actual=$(sha256sum -- "$canonical") || fail 'launcher digest failed'
[[ ${actual%% *} == "$hash" ]] || fail 'unreviewed launcher bytes'
printf 'input=%s generation=%s placement=%s sha256=%s\n' "$launcher" "$generation" "$placement" "$hash"
# extdebug skips commands on handler return 1; functrace covers source/functions
# and root-resolution substitutions. No xtrace. Worker status is checked below.
if (
    cd -- "$fixture_run" || exit 90
    phase=source; crossed_cd=0; refused=0
    finish() { trap - EXIT DEBUG; printf 'stop=%s root=%s helper=%s cwd=%s\n' "$1" "${root-unset}" "${contract_helper-unset}" "$PWD"; exit "$1"; }
    guard() {
        local origin=$1 line=$2 command_text=$3
        [[ $origin == "$control" ]] && return 0
        [[ $origin == "$launcher" ]] || finish 90
        if [[ $phase == source ]]; then
            # Exact-hash-bound top-level set, empty associative declaration and
            # false direct-execution condition only; no function-body execution.
            case $line in 3|11) return 0;; esac
            [[ $line == "$footer" ]] && return 0
            finish 90
        fi
        # Bash emits an inherited DEBUG event at function entry before its body.
        if [[ ( $family == screen && $line == 230 || $family == double && $line == 226 ) &&
              $command_text == '"$function_name" x11 "$fixture_run" "$fixture_manifest"' ]]; then return 0; fi
        if (( crossed_cd )); then
            [[ $root == "$fixture" && $contract_helper == "$fixture/tests/${filename%.sh}-contract.lisp" && $PWD == "$fixture" ]] || finish 81
            finish 80
        fi
        if [[ $line == "$cdline" ]]; then
            printf 'observed root=%s helper=%s cwd-before=%s\n' "$root" "$contract_helper" "$PWD"
            # Fail baseline before changing into its original-workspace root.
            [[ $root == "$fixture" && $contract_helper == "$fixture/tests/${filename%.sh}-contract.lisp" ]] || finish 81
            [[ $command_text == 'cd "$root"' ]] || finish 90
            crossed_cd=1; return 0
        fi
        if [[ $line == "$rootcheck" && $command_text == 'return 2' ]]; then refused=1; fi
        # Each list is finite and tied to exact reviewed bytes, not command names.
        # Skip mkdir, every redirection and EXIT-trap installation before cd.
        [[ $skip == *" $line "* ]] && return 1
        [[ $safe == *" $line "* ]] && return 0
        finish 90
    }
    shopt -s extdebug
    set -T
    # Source enables -e; disable it per DEBUG event so return 1 skips, not exits.
    trap 'set +e; guard "${BASH_SOURCE[0]}" "$LINENO" "$BASH_COMMAND"' DEBUG
    source "$launcher"
    phase=main
    LISPTERM_VTTEST_SCALE=1
    "$function_name" x11 "$fixture_run" "$fixture_manifest"
    main_status=$?
    trap - EXIT DEBUG
    printf 'main-return=%s refused=%s cwd=%s\n' "$main_status" "$refused" "$PWD"
    [[ $placement == misplaced && $main_status == 2 && $refused == 1 && $crossed_cd == 0 && $PWD == "$fixture_run" ]] && exit 82
    exit 90
); then worker_status=0; else worker_status=$?; fi
printf 'worker-status=%s\n' "$worker_status"
case $placement:$worker_status in
normal:80|misplaced:82) printf 'PASS narrow root-binding control\n'; exit 0 ;;
*) printf 'FAIL narrow root-binding control\n'; exit 1 ;;
esac
