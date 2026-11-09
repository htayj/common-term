#!/usr/bin/env bash
# Owned headless screen adapter. Exact visible-screen acceptance with no capture retries.
set -Eeuo pipefail
charsets_token() {
    local row rest; [[ $1 =~ ^[0-9]+$ && -r /proc/$1/stat ]] || return 1
    IFS= read -r row < "/proc/$1/stat" || return 1
    rest=${row##*) }; local -a fields; read -ra fields <<< "$rest"
    [[ ${fields[0]} != Z ]] || return 1
    printf '%s\n' "${fields[19]}"
}
declare -A charsets_owned=()
charsets_remember() { local t; t=$(charsets_token "$1") || return 1; charsets_owned[$1]=$t; printf '%s %s\n' "$1" "$t" >> "$native/owned.tsv"; }
charsets_is_owned() { [[ ${charsets_owned[$1]+yes} && $(charsets_token "$1" 2>/dev/null) == "${charsets_owned[$1]}" ]]; }
charsets_collect() {
    local p=$1 child; charsets_is_owned "$p" || return 0
    if [[ -r /proc/$p/task/$p/children ]]; then
        local children; children=$(cat "/proc/$p/task/$p/children") || return 0
        for child in $children; do charsets_remember "$child" || continue; charsets_collect "$child"; done
    fi
}
declare -A charsets_signal_exits=()
charsets_signal_owned() {
    local p=$1 signal=$2 result
    charsets_is_owned "$p" || return 0
    if kill -"$signal" "$p" 2>/dev/null; then result=0; else result=$?; fi
    [[ $profile != vt52 ]] || charsets_signal_exits["$p,$signal"]=$result
    return 0
}
charsets_stop() {
    local p i live
    for p in "${!charsets_owned[@]}"; do charsets_collect "$p"; done
    for p in "${!charsets_owned[@]}"; do charsets_signal_owned "$p" TERM; done
    for ((i=0;i<50;i++)); do
        live=0; for p in "${!charsets_owned[@]}"; do charsets_is_owned "$p" && live=1; done
        ((live)) || break; sleep .04
    done
    for p in "${!charsets_owned[@]}"; do charsets_signal_owned "$p" KILL; done
    for ((i=0;i<50;i++)); do
        live=0; for p in "${!charsets_owned[@]}"; do charsets_is_owned "$p" && live=1; done
        ((live)) || break; sleep .04
    done
    ((live == 0))
}
charsets_cleanup_row() {
    local p=$1 wait_exit=$2 token=NIL waited=NIL
    if [[ $p =~ ^[0-9]+$ ]]; then
        [[ ! ${charsets_owned[$p]+yes} ]] || token=\"${charsets_owned[$p]}\"
    else p=NIL; fi
    [[ ! $wait_exit =~ ^[0-9]+$ ]] || waited=T
    printf '(:pid %s :start-token %s :term-exit %s :kill-exit %s :wait-exit %s :waited %s)' "$p" "$token" "${charsets_signal_exits["$p,TERM"]-NIL}" "${charsets_signal_exits["$p,KILL"]-NIL}" "$wait_exit" "$waited"
}
charsets_cleanup_receipt() {
    local client_row server_row text
    client_row=$(charsets_cleanup_row "$client" "$client_exit") || return 1
    server_row=$(charsets_cleanup_row "$server" "$server_exit") || return 1
    printf -v text '(:version 1 :client %s :server %s)' "$client_row" "$server_row"
    # Complete ASCII form plus LF, measured before exclusive publication.
    [[ $(( ${#text} + 1 )) -le 8192 ]] || return 1
    (set -o noclobber; printf '%s\n' "$text" > "$native/owned-cleanup.sexp")
}
charsets_socket_owned() {
    local fd link inode
    for fd in /proc/"$1"/fd/*; do
        link=$(readlink "$fd") || continue
        [[ $link =~ ^socket:\[([0-9]+)\]$ ]] || continue
        inode=${BASH_REMATCH[1]}
        if awk -v n="$inode" -v p="$2" '$7==n && $8==p {found=1; print} END {exit !found}' /proc/net/unix; then return 0; fi
    done
    return 1
}
charsets_wait_file() {
    local path=$1 i
    for ((i=0;i<1500;i++)); do
        [[ ! -e $recorder/abort.sexp && ! -e $recorder/ABORT ]] || return 1
        [[ ! -e $recorder/end.sexp && ! -e $recorder/END ]] || return 1
        [[ ! -e $path ]] || return 0
        charsets_collect "$client"
        charsets_is_owned "$client" || return 1
        charsets_is_owned "$server" || return 1
        sleep .02
    done
    return 1
}
charsets_read_request() {
    local path=$1 expected=$2
    read -r nn window width height scale target ack extra < "$path" || return 1
    [[ $expected == "$capture_pause" && $nn =~ ^0?[12]$ && $((10#$nn)) == "$capture_pause" && $window =~ ^[0-9]+$ && $width == 640 && $height == 384 && $scale == "$run_scale" && $target =~ ^[0-9]+$ && -z ${extra:-} ]] || return 1
    if [[ $profile == vt52 ]]; then
        [[ $nn == 2 && $window =~ ^(0|[1-9][0-9]*)$ && $target =~ ^(0|[1-9][0-9]*)$ ]] || return 1
        [[ $ack == -1 || $ack =~ ^(0|[1-9][0-9]*)$ ]] || return 1
        cmp -s -- "$path" <(printf '%s %s %s %s %s %s %s\n' "$nn" "$window" "$width" "$height" "$scale" "$target" "$ack") || return 1
    fi
    if [[ $backend == x11 ]]; then [[ $ack == -1 ]]; else [[ $ack =~ ^[0-9]+$ ]]; fi
}
# Register each deferred map command separately. IPC commas end for_window registration.
charsets_placement_rule() {
    local action
    case $1 in floating) action='floating enable';; origin) action='move position 0 0';; *) return 1;; esac
    printf 'for_window [app_id="^org[.]autolith[.]lispterm$" title="^%s$"] %s\n' "$owned_title" "$action"
}
charsets_rule_success() { "$jq" -e 'type=="array" and length==1 and all(.[]; .success==true and .parse_error!=true and .error==null)' "$1" >/dev/null; }
charsets_register_placement() {
    local part rule e
    for part in floating origin; do
        rule=$(charsets_placement_rule "$part") || return 1
        printf '%q ' "$swaymsg" "$rule" > "$native/window-rule-$part.command"; printf '\n' >> "$native/window-rule-$part.command"
        e=0; "$swaymsg" "$rule" > "$native/window-rule-$part.stdout" 2> "$native/window-rule-$part.stderr" || e=$?
        printf '%s\n' "$e" > "$native/window-rule-$part.exit"
        [[ $e == 0 ]] && charsets_rule_success "$native/window-rule-$part.stdout" || return 1
    done
}
charsets_output_valid() {
    "$jq" -e --argjson s "$run_scale" 'length==1 and .[0].name=="HEADLESS-1" and .[0].scale==$s and .[0].active==true and .[0].rect=={"x":0,"y":0,"width":1280,"height":800}' "$1" >/dev/null
}
charsets_owned_node() {
    "$jq" -ce --argjson pid "$2" --arg title "$owned_title" '[.. | objects | select(.pid?==$pid and .type?=="floating_con")] | if length==1 and .[0].app_id=="org.autolith.lispterm" and .[0].name==$title then .[0] else error("ambiguous/missing/foreign owned surface") end' "$1"
}
charsets_rect_valid() {
    "$jq" -e --argjson width "$2" --argjson height "$3" '
      .rect as $r | .window_rect as $w |
      ($r.x|type)=="number" and ($r.y|type)=="number" and ($r.x|floor)==$r.x and ($r.y|floor)==$r.y and
      $r.x>=0 and $r.y>=0 and $r.width==$width and $r.height==$height and
      $w.width==$width and $w.height==$height and
      ($r.x+$r.width)<=1280 and ($r.y+$r.height)<=800' "$1" >/dev/null
}
charsets_initial_rect_valid() {
    charsets_rect_valid "$1" 640 384 && "$jq" -e '.rect.x==0 and .rect.y==0' "$1" >/dev/null
}
charsets_full_region() {
    local actual; actual=$("$jq" -r '"\(.rect.x),\(.rect.y) \(.rect.width)x\(.rect.height)"' "$1") || return 1
    [[ $2 == "$actual" ]]
}
charsets_receipt() {
    local path=$1 count=$2 exitcode=$3 custody=$4 surface=NIL
    [[ $backend != wayland ]] || surface=$window
    # ACK and protocol surface ID bind the recorder witness, not independent native observations.
    (set -o noclobber; printf '(:version 3 :backend :%s :pause %s :window-id %s :native-window-id %s :surface-id %s :width %s :height %s :scale %s :capture-count %s :owned T :capture-exit %s :png "pause-%02d-actual.png" :ack-output %s :ack-basis :recorder-request-binding :custody %s)\n' "$backend" "$n" "$native_window" "$native_window" "$surface" "$width" "$height" "$run_scale" "$count" "$exitcode" "$n" "$ack" "$custody" > "$path.tmp") || return 1
    ln -T -- "$path.tmp" "$path" || return 1
    rm -- "$path.tmp"
}
# One shared parser is used here and by the manifest summary. Each call retains
# exact argv, separate streams and its actual process exit before any import.
charsets_contract() {
    local operation=$1 e=0
    local sbcl=/nix/store/rnyqmmrpp09r6rn7avh77bjkdaaxs0cm-sbcl-with-packages/bin/sbcl
    local -a command=("$sbcl" --noinform --no-userinit --no-sysinit --non-interactive
        --load "$contract_helper" --eval "($contract_package:main)")
    printf 'LISPTERM_CHARSETS_OPERATION=%q LISPTERM_CHARSETS_ROOT=%q LISPTERM_CHARSETS_BACKEND=%q ' "$operation" "$run" "$backend" > "$native/$operation.command"
    local key
    for key in PID TOKEN START_NS DEADLINE_NS PAUSE STAGE FINALIZATION CLIENT_EXIT STATUS ATTEMPTED REACHED CAPTURED BUILD STARTED CLEANUP REASON; do
        local variable=LISPTERM_CHARSETS_$key
        [[ ! -v $variable ]] || printf '%s=%q ' "$variable" "${!variable}" >> "$native/$operation.command"
    done
    printf '%q ' "${command[@]}" >> "$native/$operation.command"; printf '\n' >> "$native/$operation.command"
    LISPTERM_CHARSETS_OPERATION=$operation LISPTERM_CHARSETS_ROOT=$run LISPTERM_CHARSETS_BACKEND=$backend \
        "${command[@]}" </dev/null > "$native/$operation.stdout" 2> "$native/$operation.stderr" || e=$?
    printf '%s\n' "$e" > "$native/$operation.exit"
    return "$e"
}
charsets_import_recorder() { charsets_contract import; }
charsets_finalize() {
    [[ $finalized == NIL ]] || return 0
    finalized=T
    # A single fixed CLOCK_MONOTONIC phase. No requests, queries or captures here.
    if [[ -z $client || ! ${charsets_owned[$client]+yes} ]]; then
        finalization=:not-started; return 0
    fi
    # Start once before publication; await and import reuse exactly this clock.
    charsets_contract start || { finalization=:invalid-result; return 1; }
    local start; start=$(cat "$native/start.stdout")
    [[ $start =~ ^[0-9]+$ ]] || { finalization=:invalid-result; return 1; }
    export LISPTERM_CHARSETS_START_NS=$start LISPTERM_CHARSETS_DEADLINE_NS=$((start + 15000000000))
    if [[ ${native_primary_exit:-0} != 0 && ${native_primary_exit:-0} != 3 && ! -e $recorder/abort.sexp && ! -e $recorder/ABORT && ! -e $recorder/end.sexp ]]; then
        export LISPTERM_CHARSETS_PAUSE=${n:-0} LISPTERM_CHARSETS_REASON=$reason LISPTERM_CHARSETS_STAGE=:$native_stage
        charsets_contract notify || printf 'native failure publication failed\n' >> "$native/finalization-errors.txt"
    fi
    charsets_collect "$client"
    export LISPTERM_CHARSETS_PID=$client LISPTERM_CHARSETS_TOKEN=${charsets_owned[$client]}
    if charsets_contract await; then :
    else
        local await_exit=$?
        finalization=:invalid-result
        [[ $profile != vt52 || $await_exit != 124 ]] || finalization=:timeout
        printf 'await failed\n' >> "$native/finalization-errors.txt"; return 1
    fi
    local state; state=$(cat "$native/await.stdout")
    if [[ $state == :TIMEOUT ]]; then finalization=:timeout; return 1; fi
    [[ $state == :EXITED ]] || { finalization=:invalid-result; return 1; }
    # Only wait on our original shell child, after its token has gone/zombied.
    client_exit=0; wait "$client" || client_exit=$?
    printf '%s\n' "$client_exit" > "$native/client.exit"
    if [[ ! -f $recorder/RESULT.sexp ]]; then finalization=:exited-without-result; return 1; fi
    if charsets_import_recorder; then finalization=:complete
    else
        finalization=:invalid-result
        [[ $(cat "$native/import.exit") != 124 ]] || finalization=:timeout
        return 1
    fi
    [[ $client_exit == 0 || $client_exit == 1 || $client_exit == 3 ]]
}
charsets_cleanup() {
    local e=$? p; trap - EXIT; set +e
    native_primary_exit=$e
    [[ $e == 0 || $e == 3 ]] || e=1
    # Preserve the primary reason; secondary failures have separate receipts.
    printf '%s\n' "$reason" > "$native/primary-reason.txt"
    charsets_finalize || { e=1; printf '%s\n' "$finalization" >> "$native/finalization-errors.txt"; }
    [[ ! -e $recorder/abort.sexp && ! -e $recorder/ABORT ]] || e=1
    if [[ $started == T ]]; then
        cleanup=:pass; charsets_stop || cleanup=:fail
        for p in "${endpoints[@]}"; do
            if [[ -e $p || -S $p ]]; then printf 'owned-survivor %s\n' "$p" >> "$native/cleanup.log"; cleanup=:fail
            else printf 'owned-absent %s\n' "$p" >> "$native/cleanup.log"; fi
        done
    fi
    if [[ -n $client && ${charsets_owned[$client]+yes} && $client_exit == NIL ]] && ! charsets_is_owned "$client"; then
        client_exit=0; wait "$client" || client_exit=$?
        printf '%s\n' "$client_exit" > "$native/client.exit"
    fi
    if [[ $profile == vt52 ]]; then
        server_exit=NIL
        if [[ -n $server && ${charsets_owned[$server]+yes} ]] && ! charsets_is_owned "$server"; then
            server_exit=0; wait "$server" || server_exit=$?
            printf '%s\n' "$server_exit" > "$native/server.exit"
        fi
        if [[ $started == T && ( $client_exit == NIL || $server_exit == NIL ) ]]; then cleanup=:fail; e=1; fi
        charsets_cleanup_receipt || { cleanup=:fail; e=1; }
    fi
    if [[ -n ${binary:-} && -f $binary ]]; then
        sha256sum -- "$binary" > "$native/binary-post.sha256"
        sha256sum -c "$run/build/binary.sha256" > "$native/binary-post.check" || { e=1; printf 'binary-changed\n' >> "$native/finalization-errors.txt"; }
    fi
    if [[ -n ${runtime:-} ]]; then
        rmdir -- "$runtime" 2>> "$native/cleanup.log" || { e=1; cleanup=:fail; }
        [[ ! -e $runtime ]] || { e=1; cleanup=:fail; }
    fi
    [[ $cleanup != :fail ]] || e=1
    if [[ -f $native/execution-inputs.sha256 ]]; then
        sha256sum -c "$native/execution-inputs.sha256" > "$native/execution-inputs-post.check" || { e=1; printf 'execution-source-changed\n' >> "$native/finalization-errors.txt"; }
    fi
    if [[ $profile == vt52 ]]; then
        sha256sum -- "$manifest" > "$native/manifest-post.sha256" || e=1
        cmp -s -- "$native/manifest-pre.sha256" "$native/manifest-post.sha256" || { e=1; printf 'manifest-changed\n' >> "$native/finalization-errors.txt"; }
    fi
    local recorder_status=:UNVERIFIED
    [[ $finalization != :complete ]] || recorder_status=$(cat "$native/import.stdout")
    [[ $recorder_status != :FAIL ]] || e=1
    if [[ ( $e == 0 || $e == 3 ) && $finalization == :complete && $recorder_status == :PASS && $client_exit == 0 && $attempted == 1 && $reached == 1 && $captured == 1 && $cleanup == :pass && $build_status == :pass && ! -e $recorder/abort.sexp && ! -e $recorder/ABORT ]]; then
        e=0
    elif [[ $e == 0 ]]; then
        e=1
    fi
    local status=:fail; [[ $e == 3 ]] && status=:unverified; [[ $e == 0 ]] && status=:pass
    export LISPTERM_CHARSETS_FINALIZATION=$finalization LISPTERM_CHARSETS_CLIENT_EXIT=$client_exit
    export LISPTERM_CHARSETS_STATUS=$status LISPTERM_CHARSETS_ATTEMPTED=$attempted LISPTERM_CHARSETS_REACHED=$reached LISPTERM_CHARSETS_CAPTURED=$captured
    export LISPTERM_CHARSETS_BUILD=$build_status LISPTERM_CHARSETS_STARTED=$started LISPTERM_CHARSETS_CLEANUP=$cleanup LISPTERM_CHARSETS_REASON=$reason
    charsets_contract publish || { e=1; printf 'publish/import failure\n' >> "$native/finalization-errors.txt"; }
    # Publication can conservatively downgrade but never upgrade the shell outcome.
    if [[ $e == 0 ]]; then
        [[ -f $native/publish.stdout && $(cat "$native/publish.stdout") == :PASS && -f $run/$run_receipt ]] || e=1
        [[ $profile != vt52 || -f $run/vt52-check.sexp ]] || e=1
    else
        [[ ! -f $native/publish.stdout || $(cat "$native/publish.stdout") != :FAIL ]] || e=1
    fi
    exit "$e"
}
charsets_native_main() {
    charsets_profile_args "$@" || return 2
    backend=$1; run=${2%/}/; manifest=$3; native=$run/native; recorder=$run/recorder
    local canonical_run canonical_manifest
    canonical_run=$(readlink -f -- "$2") || return 2
    canonical_manifest=$(readlink -f -- "$manifest") || return 2
    [[ $run == "$canonical_run/" && $manifest == "$canonical_manifest" ]] || return 2
    capture_pause=1; owned_title=Lispterm-charsets-owned-run; run_receipt=CHARSETS-RUN.sexp
    contract_package=vttest-charsets-native-contract
    if [[ $profile == vt52 ]]; then
        capture_pause=2; owned_title=Lispterm-vt52-owned-run; run_receipt=VT52-RUN.sexp
        contract_package=vttest-vt52-native-contract
    fi
    [[ $manifest == "${run}MANIFEST.sexp" ]] || return 2
    # Selected scale arrives explicitly through env -i; X11 (Xvfb) is scale 1 only.
    run_scale=${LISPTERM_VTTEST_SCALE-}
    [[ $run_scale == 1 || $run_scale == 2 ]] || return 2
    [[ $backend == wayland || $run_scale == 1 ]] || return 2
    mkdir -m700 -- "$native" || return 1
    started=NIL; cleanup=:not-applicable; build_status=:unverified
    attempted=0; reached=0; captured=0; reason=preparation; binary=; server=; client=
    endpoints=(); finalized=NIL; finalization=:not-started; client_exit=NIL
    n=0; native_stage=placement
    local script_path root
    script_path=$(readlink -f -- "${BASH_SOURCE[0]}") || return 1
    root=${script_path%/tests/vttest-charsets-native.sh}
    [[ $root != "$script_path" && -d $root ]] || return 1
    contract_helper=$root/tests/vttest-$profile-native-contract.lisp
    : > "$native/stage-events.sexp"; : > "$native/unowned-endpoints.txt"
    printf '%s\n' "$run_scale" > "$native/scale.txt"
    trap charsets_cleanup EXIT
    # All project execution is relative to this prepared copy of the shell.
    cd "$root"
    # Bind inspected project inputs, actual raster/font decoder, font and launcher sources.
    # The release owner must separately seal the remaining external runtime closure.
    local bash=/nix/store/kwyi4bq5s2qgwbhz6cs4gjy57c2zdxws-bash-interactive-5.3p20/bin/bash
    local sbcl=/nix/store/rnyqmmrpp09r6rn7avh77bjkdaaxs0cm-sbcl-with-packages/bin/sbcl
    local archive=$root/.build/evidence/cursor-x11-first-screen-20261106-a/vttest-20251205.tgz
    local decoder=/nix/store/h74lg8wfwndnf5glr171kmcshlffi7l2-imagemagick-7.1.2-29/bin/magick
    local archive_input=$archive
    local -a profile_inputs=()
    if [[ $profile == vt52 ]]; then
        archive_input=${archive#"$root/"}
        profile_inputs=(tests/vttest-protocol.lisp tests/vttest-screen-native-contract.lisp scripts/deps.nix
            tests/vttest-double-size-driver.lisp
            tests/vttest-double-size-native-contract.lisp
            tests/vttest-double-size-manifest.lisp
            tests/vttest-keyboard-driver.lisp
            tests/vttest-keyboard-native-contract.lisp
            tests/vttest-keyboard-artifacts.lisp
            tests/vttest-keyboard-timing.lisp
            tests/vttest-keyboard-manifest.lisp
            tests/vttest-keyboard-cleanup.lisp
            tests/vttest-keyboard-native-summary.lisp
            tests/vttest-keyboard-prefix.lisp
            tests/vttest-keyboard-cleanup-schema.lisp
            tests/vttest-keyboard-cleanup-status.lisp
            tests/vttest-screen-blink-{native,selection,timing,contract}.lisp
            tests/vttest-vt52-{oracle,pixels,sha256,admission,native-check,png-adapter,capture-adapter,navigation}.lisp
            tests/vttest-vt52-{driver,manifest}.lisp tests/vttest-vt52-native-{io,root,lifecycle,contract,provenance,clock,navigation}.lisp)
        sha256sum -- "$manifest" > "$native/manifest-pre.sha256" || return 1
    fi
    # Created before execution, verified after cleanup; no distinct source precheck.
    sha256sum -- "$manifest" tests/verify.sh tests/vttest-area.lisp tests/vttest-cursor-driver.lisp tests/vttest-screen-driver.lisp tests/vttest-screen-manifest.lisp tests/vttest-charsets-driver.lisp tests/vttest-charsets-manifest.lisp tests/vttest-screen-build.sh tests/vttest-charsets-native.sh tests/vttest-charsets-native-contract.lisp tests/vttest-charsets-recorder.lisp tests/vttest-screen-frame.lisp tests/vttest-screen-oracle.lisp tests/vttest-charsets-oracle.lisp tests/vttest-charsets-pixels.lisp tests/vttest-cursor-oracle.lisp tests/vttest-screen-wire.lisp tests/vttest-selection.lisp scripts/sbcl scripts/bootstrap.lisp lispterm.asd src/package.lisp src/conditions.lisp src/engine/compat.lisp src/engine/adapter.lisp \
        engine/nerimux/src/package-types.lisp \
        engine/nerimux/src/package.lisp \
        engine/nerimux/src/cell.lisp \
        engine/nerimux/src/screen-data.lisp \
        engine/nerimux/src/screen.lisp \
        engine/nerimux/src/screen-metadata.lisp \
        engine/nerimux/src/screen-resize.lisp \
        engine/nerimux/src/screen-logic.lisp \
        engine/nerimux/src/scroll.lisp \
        engine/nerimux/src/erase.lisp \
        engine/nerimux/src/edit.lisp \
        engine/nerimux/src/cursor.lisp \
        engine/nerimux/src/char-write-definitions.lisp \
        engine/nerimux/src/char-write-cells.lisp \
        engine/nerimux/src/char-write.lisp \
        engine/nerimux/src/modes-alt-screen.lisp \
        engine/nerimux/src/modes-dec-pm-definitions.lisp \
        engine/nerimux/src/modes-cursor-save.lisp \
        engine/nerimux/src/modes-reset.lisp \
        engine/nerimux/src/modes-charset-definitions.lisp \
        engine/nerimux/src/modes-charset.lisp \
        engine/nerimux/src/modes-ansi-sm-rm-definitions.lisp \
        engine/nerimux/src/screen-projection.lisp \
        engine/nerimux/src/screen-osc-state.lisp \
        engine/nerimux/src/sgr-definitions.lisp \
        engine/nerimux/src/sgr-colors.lisp \
        engine/nerimux/src/sgr.lisp \
        engine/nerimux/src/sgr-report.lisp \
        engine/nerimux/src/csi-replies-definitions.lisp \
        engine/nerimux/src/csi-replies.lisp \
        engine/nerimux/src/csi-parameters.lisp \
        engine/nerimux/src/csi-dispatch.lisp \
        engine/nerimux/src/csi.lisp \
        engine/nerimux/src/csi-device-rules.lisp \
        engine/nerimux/src/csi-extended-rules.lisp \
        engine/nerimux/src/csi-compose.lisp \
        engine/nerimux/src/parser-dcs.lisp \
        engine/nerimux/src/parser-core.lisp \
        engine/nerimux/src/parser-csi.lisp \
        engine/nerimux/src/parser-utf8.lisp \
        engine/nerimux/src/parser.lisp \
        engine/nerimux/src/parser-vt52.lisp \
        engine/nerimux/src/parser-osc-clipboard.lisp \
        engine/nerimux/src/parser-osc-uri.lisp \
        engine/nerimux/src/parser-osc-color.lisp \
        engine/nerimux/src/parser-osc-dispatch.lisp \
        engine/nerimux/src/parser-osc.lisp \
        engine/nerimux/src/emulator.lisp \
        src/session.lisp src/renderer.lisp src/window.lisp src/host.lisp \
        src/pty/package.lisp \
        src/pty/ffi.lisp \
        src/pty/posix.lisp \
        src/pty/process.lisp \
        src/display/package.lisp \
        src/display/cells.lisp \
        src/display/font-otb.lisp \
        src/display/font.lisp \
        src/display/keys.lisp \
        src/display/protocol.lisp \
        src/display/framebuffer.lisp \
        src/display/x11.lisp \
        src/display/wayland-wire.lisp \
        src/display/wayland-protocol.lisp \
        src/display/wayland-shm.lisp \
        src/display/xkb.lisp \
        src/display/wayland.lisp \
        "${profile_inputs[@]}" "$LISPTERM_FONT" "$decoder" "$bash" "$sbcl" "$archive_input" > "$native/execution-inputs.sha256" || return 1
    reason=build
    local build_run=${run%/}
    printf '%q ' "$bash" --noprofile --norc "$root/tests/vttest-screen-build.sh" "$build_run" "$archive" > "$native/build.command"; printf '\n' >> "$native/build.command"
    local e=0
    "$bash" --noprofile --norc "$root/tests/vttest-screen-build.sh" "$build_run" "$archive" </dev/null > "$native/build.stdout" 2> "$native/build.stderr" || e=$?
    printf '%s\n' "$e" > "$native/build.exit"
    [[ $e == 0 ]] || { build_status=:fail; return 1; }
    build_status=:pass; IFS= read -r binary < "$run/build/binary.path"
    [[ $binary == "$build_run/build/source/vttest-20251205/vttest" && -x $binary && ! -L $binary ]] || return 1
    sha256sum -c "$run/build/binary.sha256" > "$native/binary-pre.check" || return 1
    sha256sum "$binary" > "$native/binary-pre.sha256"
    reason=transport
    runtime=$(mktemp -d /tmp/lt-charsets.XXXXXX); printf '%s\n' "$runtime" > "$native/runtime.path"
    export XDG_RUNTIME_DIR=$runtime TERM=vt100
    local xdotool=/nix/store/qln0b1qf02iif0kmkd6iqxg5fp5bhjyf-xdotool-4.20260303.1/bin/xdotool
    local magick=/nix/store/h74lg8wfwndnf5glr171kmcshlffi7l2-imagemagick-7.1.2-29/bin/magick
    local sway=/nix/store/9vhixvl9s6a6k7b70plgpvzd4z0nbg62-sway-unwrapped-1.12/bin/sway
    local swaymsg=${sway%/*}/swaymsg
    local grim=/nix/store/7wadxjci562iydhr98dn7lk9ha39ikm3-grim-1.5.0/bin/grim
    local jq=/nix/store/4b6f9wb1qak318gk7mxlfkhril6lxpcx-jq-1.8.2-bin/bin/jq
    local wtype=/nix/store/2ljrpd04hhwary2fir7183c4inyx1y9a-wtype-0.4/bin/wtype
    local xvfb=/nix/store/lkbb5ra3h85qssrbc8p1fffr3f1i65j2-xorg-server-21.1.24/bin/Xvfb
    local tool
    for tool in "$sbcl" "$xdotool" "$magick" "$sway" "$swaymsg" "$grim" "$jq" "$wtype" "$xvfb"; do
        [[ -x $tool && -f $tool ]] || return 1
        sha256sum "$tool" >> "$native/tools.sha256"
    done
    if [[ $backend == x11 ]]; then
        printf '%q ' "$xvfb" -displayfd 3 -screen 0 1280x800x24 -nolisten tcp > "$native/server.command"
        "$xvfb" -displayfd 3 -screen 0 1280x800x24 -nolisten tcp 3> "$native/display-number" </dev/null > "$native/server.stdout" 2> "$native/server.stderr" & server=$!
        started=T; charsets_remember "$server"
        local i number
        for ((i=0;i<100;i++)); do charsets_is_owned "$server" || return 1; [[ ! -s $native/display-number ]] || break; sleep .05; done
        number=$(cat "$native/display-number"); [[ $number =~ ^[0-9]+$ ]] || return 1
        export DISPLAY=:$number
        printf '/tmp/.X%s-lock\n' "$number" >> "$native/unowned-endpoints.txt"
        if charsets_socket_owned "$server" "/tmp/.X11-unix/X$number" > "$native/transport-ownership.txt"; then
            endpoints=("/tmp/.X11-unix/X$number")
        else
            charsets_socket_owned "$server" "@/tmp/.X11-unix/X$number" > "$native/transport-ownership.txt" || return 1
            # Abstract-socket custody is not custody of a filesystem pathname.
            printf '/tmp/.X11-unix/X%s\n' "$number" >> "$native/unowned-endpoints.txt"
        fi
    else
        local wp=$runtime/wayland-1 ip=$runtime/sway-ipc.$UID.9999999999.sock
        printf 'wayland=%s bytes=%s\nipc-upper-bound=%s bytes=%s\nrequire<=100 (108 including NUL kernel capacity)\n' "$wp" "${#wp}" "$ip" "${#ip}" > "$native/socket-precheck.txt"
        [[ ${#wp} -le 100 && ${#ip} -le 100 ]] || return 1
        printf '%s\n' 'xwayland disable' "output HEADLESS-1 mode $((1280*run_scale))x$((800*run_scale)) scale $run_scale" 'seat seat0 fallback true' 'font monospace 12' 'default_border none' 'focus_follows_mouse no' > "$native/sway.conf"
        printf 'WLR_BACKENDS=headless WLR_HEADLESS_OUTPUTS=1 WLR_RENDERER=pixman WLR_LIBINPUT_NO_DEVICES=1 %q --config %q\n' "$sway" "$native/sway.conf" > "$native/server.command"
        WLR_BACKENDS=headless WLR_HEADLESS_OUTPUTS=1 WLR_RENDERER=pixman WLR_LIBINPUT_NO_DEVICES=1 "$sway" --config "$native/sway.conf" </dev/null > "$native/server.stdout" 2> "$native/server.stderr" & server=$!
        started=T; charsets_remember "$server"
        export WAYLAND_DISPLAY=wayland-1 SWAYSOCK=$runtime/sway-ipc.$UID.$server.sock
        local i
        for ((i=0;i<100;i++)); do charsets_is_owned "$server" || return 1; [[ ! -S $SWAYSOCK || ! -S $runtime/wayland-1 ]] || break; sleep .05; done
        [[ -S $SWAYSOCK && -S $runtime/wayland-1 ]] || return 1
        charsets_socket_owned "$server" "$runtime/wayland-1" > "$native/transport-ownership.txt" || return 1
        endpoints=("$runtime/wayland-1")
        charsets_socket_owned "$server" "$SWAYSOCK" >> "$native/transport-ownership.txt" || return 1
        endpoints+=("$SWAYSOCK")
        "$swaymsg" -t get_outputs > "$native/outputs.json" 2> "$native/outputs.stderr"
        charsets_output_valid "$native/outputs.json" || return 1
        charsets_register_placement || return 1
        "$wtype" -s 120000 </dev/null > "$native/keyboard.stdout" 2> "$native/keyboard.stderr" & keyboard=$!; charsets_remember "$keyboard"
        sleep .3; charsets_is_owned "$keyboard" || return 1
    fi
    reason=recorder
    export LISPTERM_VTTEST_RUN_ROOT=$run LISPTERM_VTTEST_EXECUTABLE=$binary LISPTERM_VTTEST_BACKEND=$backend LISPTERM_VTTEST_SCALE=$run_scale
    printf '%q ' "$sbcl" --noinform --no-userinit --no-sysinit --script "$root/tests/vttest-charsets-recorder.lisp" > "$native/client.command"; printf '\n' >> "$native/client.command"
    "$sbcl" --noinform --no-userinit --no-sysinit --script "$root/tests/vttest-charsets-recorder.lisp" </dev/null > "$native/client.stdout" 2> "$native/client.stderr" & client=$!
    charsets_remember "$client"
    printf 'recorder-pid=%s token=%s executable=%s\n' "$client" "${charsets_owned[$client]}" "$binary" > "$native/launch.txt"
    local prefix nn window width height scale target ack extra native_window x y w h pid node
    reason=placement; native_stage=placement
    charsets_wait_file "$recorder/placement-request.sexp" || return 1
    if [[ $backend == wayland ]]; then
        # Recorder cannot reach a BEFORE until this observation is published.
        for ((i=0;i<400;i++)); do
            charsets_is_owned "$client" && charsets_is_owned "$server" || return 1
            "$swaymsg" -t get_tree > "$native/initial-tree.json" 2> "$native/initial-tree.stderr" || return 1
            if [[ $("$jq" --argjson pid "$client" '[.. | objects | select(.pid?==$pid and .type?=="floating_con")]|length' "$native/initial-tree.json") != 0 ]]; then
                node=$(charsets_owned_node "$native/initial-tree.json" "$client" 2> "$native/initial-node.stderr") || return 1
                break
            fi
            sleep .02
        done
        printf '%s\n' "$node" > "$native/initial-geometry.json"
        charsets_initial_rect_valid "$native/initial-geometry.json" || return 1
        "$swaymsg" -t get_outputs > "$native/initial-outputs.json" 2> "$native/initial-outputs.stderr" || return 1
        charsets_output_valid "$native/initial-outputs.json" || return 1
    fi
    (set -o noclobber; printf '(:observed T)\n' > "$recorder/placement-ready.sexp.tmp") || return 1
    ln -T -- "$recorder/placement-ready.sexp.tmp" "$recorder/placement-ready.sexp" || return 1
    rm -- "$recorder/placement-ready.sexp.tmp"
    printf '(:initial-placement-observed T)\n' > "$native/placement-observed.sexp"
    for ((n=capture_pause;n<=capture_pause;n++)); do
        printf -v prefix '%s/pause-%02d-' "$recorder" "$n"
        reason=request; native_stage=request
        if ! charsets_wait_file "${prefix}request"; then
            [[ ( -f $recorder/end.sexp || -f $recorder/END ) && ! -f $recorder/abort.sexp && ! -f $recorder/ABORT ]] || return 1
            break
        fi
        ((attempted+=1))
        printf '(%s :request)\n' "$n" >> "$native/stage-events.sexp"
        charsets_read_request "${prefix}request" "$n" || return 1
        ((reached+=1)); reason=geometry; native_stage=geometry
        if [[ $backend == x11 ]]; then
            native_window=$window
            charsets_is_owned "$client" && charsets_is_owned "$server" || return 1
            "$xdotool" getwindowname "$window" > "${prefix}title.txt" 2> "${prefix}title.stderr"
            [[ $profile != vt52 || $(cat "${prefix}title.txt") == "$owned_title" ]] || return 1
            printf 'recorder=%s token=%s window=%s basis=private-server-and-recorder-request\n' "$client" "${charsets_owned[$client]}" "$window" > "${prefix}ownership.txt"
            "$xdotool" getwindowgeometry --shell "$window" > "${prefix}geometry.txt" 2> "${prefix}geometry.stderr"
            w=$(sed -n 's/^WIDTH=//p' "${prefix}geometry.txt"); h=$(sed -n 's/^HEIGHT=//p' "${prefix}geometry.txt")
            [[ $w == "$width" && $h == "$height" ]] || return 1
        else
            charsets_is_owned "$client" && charsets_is_owned "$server" || return 1
            "$swaymsg" -t get_tree > "${prefix}tree.json" 2> "${prefix}tree.stderr"
            node=$(charsets_owned_node "${prefix}tree.json" "$client") || return 1
            printf '%s\n' "$node" > "${prefix}geometry.json"
            read -r native_window x y w h < <("$jq" -r '[.id,.rect.x,.rect.y,.rect.width,.rect.height]|@tsv' <<< "$node")
            charsets_rect_valid "${prefix}geometry.json" "$width" "$height" || return 1
            "$swaymsg" -t get_outputs > "${prefix}outputs.json" 2> "${prefix}outputs.stderr" || return 1
            charsets_output_valid "${prefix}outputs.json" || return 1
        fi
        charsets_receipt "${prefix}native-ready.sexp" 0 0 NIL
        reason=authorization; native_stage=authorization; charsets_wait_file "${prefix}go" || return 1
        reason=capture; native_stage=capture; e=0
        printf '(%s :invoked)\n' "$n" >> "$native/stage-events.sexp"
        if [[ $backend == x11 ]]; then
            printf '%q ' "$magick" import -window "$window" "${prefix}actual.png" > "${prefix}capture.command"
            "$magick" import -window "$window" "${prefix}actual.png" </dev/null > "${prefix}capture.stdout" 2> "${prefix}capture.stderr" || e=$?
        else
            charsets_full_region "${prefix}geometry.json" "$x,$y ${width}x$height" || return 1
            printf '%q ' "$grim" -g "$x,$y ${width}x$height" "${prefix}actual.png" > "${prefix}capture.command"
            "$grim" -g "$x,$y ${width}x$height" "${prefix}actual.png" </dev/null > "${prefix}capture.stdout" 2> "${prefix}capture.stderr" || e=$?
        fi
        printf '%s\n' "$e" > "${prefix}capture.exit"
        printf '(%s :completed %s)\n' "$n" "$e" >> "$native/stage-events.sexp"
        if [[ $e != 0 || ! -s ${prefix}actual.png ]]; then charsets_receipt "${prefix}native.sexp" 1 "$e" NIL; : > "${prefix}done"; return 1; fi
        # Physical raster proof on the unchanged PNG: logical size times selected scale.
        local -a identify=("$magick" identify -format '%w %h' "${prefix}actual.png")
        printf '%q ' "${identify[@]}" > "${prefix}dimensions.command"; printf '\n' >> "${prefix}dimensions.command"
        local de=0
        "${identify[@]}" </dev/null > "${prefix}dimensions.stdout" 2> "${prefix}dimensions.stderr" || de=$?
        printf '%s\n' "$de" > "${prefix}dimensions.exit"
        if [[ $de != 0 || $(cat "${prefix}dimensions.stdout") != "$((width*run_scale)) $((height*run_scale))" ]]; then
            charsets_receipt "${prefix}native.sexp" 1 0 NIL; : > "${prefix}done"; return 1
        fi
        ((captured+=1))
        sha256sum "${prefix}actual.png" > "${prefix}actual.sha256"
        charsets_receipt "${prefix}native.sexp" 1 0 T
        : > "${prefix}done"
    done
    reason=recorder-exit
    # EXIT owns the only finalization path, including normal completion.
    return 3
}
# Validate the closed CLI before allocation or native/build dispatch.
charsets_profile_args() {
    [[ $# == 3 || $# == 5 ]] || return 2
    profile=charsets
    if [[ $# == 5 ]]; then
        [[ $4 == --profile && ( $5 == vt52 || $5 == charsets ) ]] || return 2
        profile=$5
    fi
    [[ -z ${LISPTERM_VTTEST_PROFILE:-} || $LISPTERM_VTTEST_PROFILE == "$profile" ]] || return 2
    [[ ( $1 == x11 || $1 == wayland ) && $2 == /* && -d $2 && ! -L $2 && -O $2 && $3 == "${2%/}/MANIFEST.sexp" && -f $3 && ! -L $3 ]] || return 2
    local canonical_run canonical_manifest
    local readlink=/nix/store/cb987zkxvsv0490753ilrvn5yk7ylgxi-coreutils-9.12/bin/readlink
    canonical_run=$("$readlink" -f -- "$2") || return 2
    canonical_manifest=$("$readlink" -f -- "$3") || return 2
    [[ ${2%/}/ == "$canonical_run/" && $3 == "$canonical_manifest" ]] || return 2
}
if [[ ${BASH_SOURCE[0]} == "$0" ]]; then
    charsets_profile_args "$@" || exit 2
    [[ ${LISPTERM_VTTEST_SCALE-} == 1 || ${LISPTERM_VTTEST_SCALE-} == 2 ]] || exit 2
    [[ $1 == wayland || $LISPTERM_VTTEST_SCALE == 1 ]] || exit 2
    umask 077
    # Allocate only run-owned environment dirs before executing any native/build tool.
    for dir in home cache config data tmp; do mkdir -m700 -- "$2/$dir" || exit 1; done
    script=$(readlink -f -- "$0")
    bash=/nix/store/kwyi4bq5s2qgwbhz6cs4gjy57c2zdxws-bash-interactive-5.3p20/bin/bash
    env=/nix/store/cb987zkxvsv0490753ilrvn5yk7ylgxi-coreutils-9.12/bin/env
    command=("$env" -i PATH=/run/current-system/sw/bin "HOME=$2/home" "XDG_CACHE_HOME=$2/cache" "XDG_CONFIG_HOME=$2/config" "XDG_DATA_HOME=$2/data" "TMPDIR=$2/tmp" LANG=C.UTF-8 LC_ALL=C.UTF-8 "LISPTERM_VTTEST_PROFILE=$profile" "LISPTERM_VTTEST_SCALE=$LISPTERM_VTTEST_SCALE" LISPTERM_FONT=/nix/store/9qvd27vg12177prxcl778rdhxh4lk2mi-terminus-font-4.49.1/share/fonts/terminus/ter-u16n.otb LISPTERM_XKBCOMMON_LIBRARY=/nix/store/f7dllvig9i72z13kzxczwq7wy8a1jpgg-libxkbcommon-1.13.2/lib/libxkbcommon.so.0 "$bash" --noprofile --norc -c 'source "$1"; charsets_native_main "${@:2}"' charsets-native "$script" "$@")
    printf '%q ' "${command[@]}" > "$2/native-environment.command"; printf '\n' >> "$2/native-environment.command"
    for fdpath in /proc/$$/fd/*; do fd=${fdpath##*/}; if [[ $fd =~ ^[0-9]+$ && $fd -gt 2 && $fd -ne 255 ]]; then eval "exec $fd>&-"; fi; done
    exec "${command[@]}" </dev/null
fi
