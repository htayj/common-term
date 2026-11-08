#!/usr/bin/env bash
# Offline and disposable process-tree fixtures only. No display/terminal launch.
set -euo pipefail
cd "$(dirname "$0")/.."
root=$PWD
export PATH="$root/.build/test-tools/bin:$PATH"
# This entry point reads only the action definitions, never the native runner or
# the application system. The dispatcher is real; display operations are fakes.
geometry_action_contract() (
  export LISPTERM_ACTION_CONTRACT_ROOT=$1
  scripts/sbcl --script /dev/stdin <<'LISP'
(require :asdf)
(require :sb-posix)
(defpackage #:native-geometry (:use #:cl))
(in-package #:native-geometry)
;; Read only these complete source forms. No application package, bootstrap,
;; capture implementation, or top-level native entry point is loaded/evaluated.
(let ((wanted '("(defvar *art* " "(defvar *action-window* " "(defvar *action-ordinal* "
                "(defvar *preparation-deadline* " "(defvar *acquisition* "
                "(defun path " "(defun append-value " "(defun now " "(defun await "
                "(defun action-ack-p " "(defun action ")))
  (with-open-file (source "tests/vt100-native-geometry.lisp")
    (loop for line = (read-line source nil) while line
          when (some (lambda (prefix) (uiop:string-prefix-p prefix line)) wanted)
            do (eval (read (make-concatenated-stream
                            (make-string-input-stream (format nil "~a~%" line)) source)))
               (setf wanted (remove-if (lambda (prefix) (uiop:string-prefix-p prefix line)) wanted))))
  (assert (null wanted)))
(defparameter *checks* 0)
(defun check (value label)
  (assert value () "Action contract failed: ~a" label)
  (incf *checks*) (format t "PASS ~a~%" label))
(defun fails (thunk) (handler-case (progn (funcall thunk) nil) (error () t)))
(defparameter *dispatch*
"LISPTERM_NATIVE_FUNCTIONS_ONLY=1 source tests/vt100-native-geometry.sh
art=$1 mode=$2 native_action_ordinal=$3 fault=$4
native_action_failed=0 backend=wayland client=1 server=1 capture_action_window=5
export XDG_RUNTIME_DIR=$art/runtime
capture_runtime=${XDG_RUNTIME_DIR:-} capture_display=${DISPLAY:-}
capture_swaysock=${SWAYSOCK:-} capture_wayland=${WAYLAND_DISPLAY:-}
is_owned() { [[ $1 == 1 ]]; }
window_identity() { window=5; }
native_resize_wayland() { :; }
native_mode_wayland() { :; }
native_prepare_retry() { :; }
printf -v stage '%s/action-request-s%02d.staged' \"$art\" \"$((native_action_ordinal+1))\"
case $fault in
  missing) mv -- \"$stage\" \"$stage.removed\";;
  replaced) mv -- \"$stage\" \"$stage.original\"; cp -- \"$art/action-request\" \"$stage\";;
esac
if external_action; then
  [[ $fault == none && $native_action_failed == 0 && ! -e $art/action-request && -f $art/action-ack ]]
else
  [[ $fault != none && $native_action_failed == 1 && -f $art/action-request && ! -e $art/action-ack ]]
fi")
(defparameter *real-await* (symbol-function 'await))
(defvar *mode*) (defvar *fault*) (defvar *dispatch-code* nil)
(setf (symbol-function 'await)
      (lambda (thunk &optional seconds)
        (declare (ignore seconds))
        (let ((code (nth-value 2 (uiop:run-program
                                 (list "bash" "-c" *dispatch* "contract" (namestring *art*)
                                       *mode* (write-to-string (1- *action-ordinal*)) *fault*)
                                 :output *standard-output* :error-output *error-output*
                                 :ignore-error-status t))))
          (setf *dispatch-code* code)
          (assert (eql 0 code) () "Dispatcher contract subprocess exit ~s" code))
        (funcall *real-await* thunk .05)))
(defun contract-case (name thunk)
  (let ((*art* (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "~a/" name)
                                 (uiop:ensure-directory-pathname (uiop:getenv "LISPTERM_ACTION_CONTRACT_ROOT")))))
        (*action-ordinal* 0) (*action-window* nil) (*mode* "normal") (*fault* "none") (*dispatch-code* nil))
    (ensure-directories-exist (path "placeholder"))
    (funcall thunk)))
(contract-case "normal"
  (lambda () (action "resize" 800 416)
    (check (and (= 1 *action-ordinal*) (= 5 *action-window*)
                (probe-file (path "action-request-s01.staged"))
                (not (probe-file (path "action-ack")))) "real ordinal1 request, dispatcher and strict ACK")))
(contract-case "refusal"
  (lambda ()
    (setf *mode* "refusal")
    (loop for kind in '("resize" "tiled" "prepare-retry" "tiled" "floating" "resize" "tiled" "floating" "resize")
          for ordinal from 1
          do (if (member kind '("resize" "prepare-retry") :test #'equal)
                 (action kind 800 416) (action kind))
             (check (and (= ordinal *action-ordinal*) (= 5 *action-window*)
                         (probe-file (path (format nil "action-request-s~2,'0d.staged" ordinal))))
                    (format nil "real refusal sequence ordinal ~d" ordinal)))))
(dolist (fault '("missing" "replaced"))
  (contract-case fault
    (lambda () (setf *fault* fault)
      (check (fails (lambda () (action "resize" 800 416)))
             (format nil "~a stage fails closed without ACK" fault))
      (check (and (eql 0 *dispatch-code*) (= 1 *action-ordinal*) (null *action-window*)
                  (probe-file (path "action-request"))
                  (probe-file (path "action-01.request.raw"))) "failed transaction evidence retained"))))
(dolist (leaf '("action-request" "action-ack" "action-request-s01.staged"))
  (contract-case leaf
    (lambda ()
      (with-open-file (out (path leaf) :direction :output :if-exists :error)
        (write-line "prior evidence" out))
      (let ((inode (sb-posix:stat-ino (sb-posix:stat (path leaf)))))
        (check (fails (lambda () (action "resize" 800 416))) (format nil "~a collision rejected" leaf))
        (check (and (= inode (sb-posix:stat-ino (sb-posix:stat (path leaf))))
                    (string= "prior evidence" (with-open-file (in (path leaf)) (read-line in))))
               "collision preserves original inode and bytes")))))
;; Preserve a dangling destination too; the stale handshake check may reject it.
(contract-case "dangling-request"
  (lambda ()
    (sb-posix:symlink "absent" (namestring (path "action-request")))
    (check (fails (lambda () (action "resize" 800 416))) "dangling request destination rejected")
    (check (and (null *dispatch-code*)
                (string= "absent" (sb-posix:readlink (path "action-request"))))
           "dangling request preserved without dispatch")))
(format t "PASS ~d real producer/dispatcher contract checks; native operations mocked~%" *checks*)
LISP
)
if [[ ${1:-} == --geometry-action-contract ]]; then
  mkdir -p "$root/.build/evidence"
  contract_art=$(mktemp -d "$root/.build/evidence/geometry-action-contract.XXXXXX")
  printf 'ARTIFACTS %s\n' "$contract_art"
  geometry_action_contract "$contract_art"
  exit
fi
# Each invocation retains its own logs and fixtures under this workspace directory.
# The ARTIFACTS line reports the generated absolute directory; no override is needed.
mkdir -p "$root/.build/evidence"
art=$(mktemp -d "$root/.build/evidence/vttest-harness.XXXXXX")
printf 'ARTIFACTS %s\n' "$art"
geometry_action_contract "$art/action-contract" > "$art/action-contract.log" 2> "$art/action-contract.stderr"
export LISPTERM_VTTEST_FUNCTIONS_ONLY=1
source tests/vttest-gate.sh
unset LISPTERM_VTTEST_FUNCTIONS_ONLY
# Production pointer publication must never replace historical evidence.
(
  art=$art/pointer-controls; mkdir -p "$art"
  pointer=$art/latest
  fixture_latest_pointer "$pointer"
  [[ $(cat "$pointer") == "$art" ]]
  printf 'historical\n' > "$art/history"
  for kind in regular symlink dangling directory; do
    rm -rf -- "$pointer"
    case $kind in
      regular) cp "$art/history" "$pointer";;
      symlink) ln -s "$art/history" "$pointer";;
      dangling) ln -s "$art/absent" "$pointer";;
      directory) mkdir "$pointer";;
    esac
    before=$(stat -c '%f|%u|%g|%s|%Y|%Z|%i' "$pointer")
    fixture_latest_pointer "$pointer"
    [[ $(stat -c '%f|%u|%g|%s|%Y|%Z|%i' "$pointer") == "$before" ]]
    [[ $kind != regular ]] || cmp "$pointer" "$art/history"
    [[ $kind != dangling || ! -e $art/absent ]]
  done
  if fixture_latest_pointer "$art/missing-parent/latest"; then exit 1; fi
)
printf 'PASS latest create-once and historical regular/symlink/dangling/directory preservation; publication error rejects\n' > "$art/pointer-tests.log"
# Run the real native action dispatcher against a stateful, non-GUI Sway model.
(
  art=$art/placement-controls; mkdir -p "$art"
  LISPTERM_NATIVE_FUNCTIONS_ONLY=1 source "$root/tests/vt100-native-geometry.sh"
  clear_session_endpoints
  export XDG_RUNTIME_DIR=$art/runtime SWAYSOCK=$art/sway.sock WAYLAND_DISPLAY=wayland-fixture
  mkdir "$XDG_RUNTIME_DIR"
  capture_runtime=$XDG_RUNTIME_DIR; capture_display=${DISPLAY:-}
  capture_swaysock=$SWAYSOCK; capture_wayland=$WAYLAND_DISPLAY
  capture_action_window=5
  backend=wayland; client=$BASHPID; server=$client; remember "$client"
  export LISPTERM_NATIVE_TITLE=placement-control
  export LISPTERM_SWAYMSG=$art/swaymsg
  cat > "$LISPTERM_SWAYMSG" <<'MOCK'
#!/usr/bin/env bash
set -euo pipefail
base=${0%/*}
state=$base/state.json
fault=$(cat "$base/fault")
if [[ $* == '-t get_tree' ]]; then
  case $fault in
    tree-transport) exit 7;;
    tree-empty) exit 0;;
    tree-malformed) echo '{'; exit 0;;
    tree-multiple) cat "$state" "$state"; exit 0;;
  esac
  cat "$state"; exit 0
elif [[ $* == '-t get_outputs' ]]; then
  case $fault in
    outputs-transport) exit 7;;
    outputs-empty) exit 0;;
    outputs-malformed) echo '{'; exit 0;;
    outputs-multiple) cat "$base/outputs.json" "$base/outputs.json"; exit 0;;
  esac
  cat "$base/outputs.json"; exit 0
fi
printf '%s\n' "$*" >> "$base/operations"
read -ra words <<< "$1"
case ${words[1]:-} in
  fullscreen)
    kind=fullscreen-${words[2]}; cp "$state" "$state.tmp";;
  floating)
    kind=floating-${words[2]}
    if [[ ${words[2]} == enable ]]; then
      jq '.type="floating_con" | .rect={x:0,y:0,width:640,height:384}' "$state" > "$state.tmp"
    else jq '.type="con"' "$state" > "$state.tmp"; fi;;
  border) kind=border; cp "$state" "$state.tmp";;
  resize)
    kind=resize; w=${words[4]}; h=${words[7]}
    jq --argjson w "$w" --argjson h "$h" '
      .rect.x += ((.rect.width-$w)/2) | .rect.y += ((.rect.height-$h)/2) |
      .rect.width=$w | .rect.height=$h' "$state" > "$state.tmp";;
  move)
    kind=anchor; x=${words[3]}; y=${words[4]}
    jq --argjson x "$x" --argjson y "$y" '.rect.x=$x | .rect.y=$y' "$state" > "$state.tmp";;
  *) exit 90;;
esac
mv "$state.tmp" "$state"
if [[ $fault == title-$kind ]]; then
  jq '.name="foreign"' "$state" > "$state.tmp"; mv "$state.tmp" "$state"
fi
if [[ $fault == type-$kind ]]; then
  jq '.type="workspace"' "$state" > "$state.tmp"; mv "$state.tmp" "$state"
fi
if [[ $fault == shrink-$kind || $fault == output-$kind ]]; then
  jq --arg f "$fault" '.[] |= (if ($f|startswith("shrink")) then .rect.width=100 else .name="changed" end)' "$base/outputs.json" > "$base/outputs.tmp"
  mv "$base/outputs.tmp" "$base/outputs.json"
fi
if [[ $fault == stale-$kind ]]; then
  jq '.id+=1' "$state" > "$state.tmp"; mv "$state.tmp" "$state"
fi
if [[ $fault == wrong-$kind ]]; then
  jq '.pid+=1' "$state" > "$state.tmp"; mv "$state.tmp" "$state"
fi
if [[ $fault == duplicate-$kind ]]; then
  jq '[.,.]' "$state" > "$state.tmp"; mv "$state.tmp" "$state"
fi
if [[ $fault == oversized-$kind ]]; then
  jq '.rect.width=9999' "$state" > "$state.tmp"; mv "$state.tmp" "$state"
fi
if [[ $fault == unmoved-$kind ]]; then
  jq '.rect.y = -16' "$state" > "$state.tmp"; mv "$state.tmp" "$state"
fi
case $fault in
  transport-$kind) exit 7;;
  false-$kind) echo '[{"success":false}]';;
  error-$kind) echo '[{"success":true,"error":"rejected"}]';;
  null-$kind) echo '[{"success":null}]';;
  parse-$kind) echo '[{"success":true,"parse_error":true}]';;
  malformed-$kind) echo '{';;
  empty-$kind) :;;
  arrayempty-$kind) echo '[]';;
  mixed-$kind) echo '[{"success":true},{"success":false}]';;
  two-$kind) echo '[{"success":true},{"success":true}]';;
  parsetype-$kind) echo '[{"success":true,"parse_error":"false"}]';;
  multiple-$kind) printf '[{"success":true}]\n[{"success":true}]\n';;
  object-$kind) echo '{"success":true}';;
  *) echo '[{"success":true}]';;
esac
MOCK
  chmod +x "$LISPTERM_SWAYMSG"
  case_number=0
  action_reset() {
    local action=$1 previous
    local -a leaves
    ((case_number+=1))
    previous=$art/case-$case_number
    mkdir "$previous"
    # Retain each completed or failed transaction before reserving another slot.
    shopt -s nullglob
    leaves=("$art"/action-*)
    shopt -u nullglob
    ((${#leaves[@]} == 0)) || mv -- "${leaves[@]}" "$previous/"
    native_action_failed=0
    case $action in
      resize) mode=normal; native_action_ordinal=0;;
      prepare-retry) mode=refusal; native_action_ordinal=2;;
      tiled) mode=refusal; native_action_ordinal=1;;
      floating) mode=refusal; native_action_ordinal=4;;
      *) return 1;;
    esac
  }
  stage_request() {
    local stage
    printf -v stage '%s/action-request-s%02d.staged' "$art" "$((native_action_ordinal+1))"
    # Fault cases may replace the request bytes, but retain the producer's alias.
    printf "$@" > "$stage"
    [[ -e $art/action-request ]] || ln -- "$stage" "$art/action-request"
  }
  placement_reset() {
    local requested_action=${2:-resize}
    action_reset "$requested_action"
    : > "$art/operations"; printf '%s\n' "${1:-none}" > "$art/fault"
    jq -n --arg t "$LISPTERM_NATIVE_TITLE" --argjson p "$client" \
      --argjson w "$((132*cw))" --argjson h "$((24*ch))" \
      '{id:5,name:$t,pid:$p,type:"floating_con",rect:{x:0,y:0,width:$w,height:$h}}' > "$art/state.json"
    jq -n --argjson s "$scale" '[{name:"HEADLESS-1",active:true,scale:$s,
      current_mode:{width:(1280*$s),height:(800*$s)},rect:{x:0,y:0,width:1280,height:800}}]' > "$art/outputs.json"
    case $requested_action in
      resize) stage_request 'resize %s %s\n' "$((100*cw))" "$((26*ch))";;
      prepare-retry) stage_request 'prepare-retry %s %s\n' "$((132*cw))" "$(((800/ch)*ch))";;
      tiled|floating) stage_request '%s\n' "$requested_action";;
    esac
  }
  # The 8x16 case is saved139's 006->007 center shift; other strikes/scales
  # use logical font metrics, never scaled pixel dimensions in IPC commands.
  for scale in 1 2; do
    for metrics in '7 14' '8 16' '9 18'; do
      read -r cw ch <<< "$metrics"
      placement_reset
      external_action
      [[ $(cat "$art/action-ack") == 'resize 5' ]]
      printf '[con_id=5] resize set width %s px height %s px\n[con_id=5] move position 0 0\n' "$((100*cw))" "$((26*ch))" > "$art/expected-operations"
      cmp "$art/operations" "$art/expected-operations"
      jq -e --argjson w "$((100*cw))" --argjson h "$((26*ch))" \
        '.id==5 and .rect=={x:0,y:0,width:$w,height:$h}' "$art/state.json" > /dev/null
      cp "$art/state.json" "$art/success-scale${scale}-${cw}x${ch}.json"
      if [[ $cw == 8 && $scale == 1 ]]; then
        jq -es 'any(.[]; .rect=={x:128,y:-16,width:800,height:416})' "$art"/action-*.resized.tree.json > /dev/null
      fi
    done
  done
  scale=1; cw=8; ch=16
  for fault in stale-resize stale-anchor wrong-resize wrong-anchor duplicate-resize duplicate-anchor \
      oversized-anchor unmoved-anchor \
      transport-resize transport-anchor false-resize false-anchor parse-resize parse-anchor \
      malformed-resize malformed-anchor empty-resize empty-anchor arrayempty-resize arrayempty-anchor \
      mixed-resize mixed-anchor multiple-resize multiple-anchor object-resize object-anchor \
      two-resize two-anchor parsetype-resize parsetype-anchor \
      tree-transport tree-empty tree-malformed tree-multiple \
      outputs-transport outputs-empty outputs-malformed outputs-multiple; do
    placement_reset "$fault"
    if external_action; then echo "Accepted placement fault: $fault" >&2; exit 1; fi
    [[ ! -e $art/action-ack && -e $art/action-request ]]
    cp "$art/operations" "$art/fault-$fault.operations"
  done
  for fault in wrong-title wrong-pid duplicate unsafe-anchor oversized-request stale-owner missing-owner write-failure; do
    placement_reset; owner_token=${owned[$client]}
    case $fault in
      wrong-title) jq '.name="other"' "$art/state.json" > "$art/state.tmp"; mv "$art/state.tmp" "$art/state.json";;
      wrong-pid) jq '.pid+=1' "$art/state.json" > "$art/state.tmp"; mv "$art/state.tmp" "$art/state.json";;
      duplicate) jq '[.,.]' "$art/state.json" > "$art/state.tmp"; mv "$art/state.tmp" "$art/state.json";;
      unsafe-anchor) jq '.rect.y = -16' "$art/state.json" > "$art/state.tmp"; mv "$art/state.tmp" "$art/state.json";;
      oversized-request) stage_request 'resize 9999 416\n';;
      stale-owner) owned[$client]=stale;;
      missing-owner) unset 'owned[$client]';;
      write-failure) mkdir "$art/action-ack.tmp";;
    esac
    if external_action; then echo "Accepted placement fault: $fault" >&2; exit 1; fi
    [[ ! -e $art/action-ack ]]
    owned[$client]=$owner_token; unset 'identity_conflicts[$client]'
    # Keep the write-failure directory for action_reset's evidence archive.
  done
  # Positive nonzero anchor is retained; negative geometry and oversize fail fit.
  placement_reset
  jq '.rect.x=32 | .rect.y=48' "$art/state.json" > "$art/state.tmp"; mv "$art/state.tmp" "$art/state.json"
  external_action
  jq -e '.rect.x==32 and .rect.y==48' "$art/state.json" > /dev/null
  if native_action_fit "$art/outputs.json" 128 -16 1056 416; then exit 1; fi
  if native_action_fit "$art/outputs.json" 0 0 9999 416; then exit 1; fi
  if native_action_ipc '[con_id=5] move position 0 0' "$art/missing-parent/action"; then exit 1; fi
  printf 'PASS six metric/scale combinations, exact resize/anchor ordering, saved139 shift, nonzero anchor; 46 dispatcher rejection controls, IPC write failure; no ACK on failure\n' > "$art/tests.log"
  # The floating model restores80x24; setup must explicitly recover saved target.
  prep_reset() {
    placement_reset "${1:-none}" "${2:-prepare-retry}"
    jq '.type="con" | .rect={x:0,y:0,width:1280,height:800}' "$art/state.json" > "$art/state.tmp"
    mv "$art/state.tmp" "$art/state.json"
  }
  for scale in 1 2; do
    for metrics in '7 14' '8 16' '9 18'; do
      read -r cw ch <<< "$metrics"; prep_reset
      external_action
      [[ $(cat "$art/action-ack") == 'prepare-retry 5' ]]
      printf '[con_id=5] fullscreen disable\n[con_id=5] floating enable\n[con_id=5] border none\n[con_id=5] resize set width %s px height %s px\n[con_id=5] move position 0 0\n' "$((132*cw))" "$(((800/ch)*ch))" > "$art/prep-expected-operations"
      cmp "$art/operations" "$art/prep-expected-operations"
      jq -e --argjson w "$((132*cw))" --argjson h "$(((800/ch)*ch))" '.id==5 and .type=="floating_con" and .rect=={x:0,y:0,width:$w,height:$h}' "$art/state.json" > /dev/null
    done
  done
  scale=1; cw=8; ch=16; prep_checks=0
  for step in fullscreen-disable floating-enable border resize anchor; do
    for problem in transport false error null parse malformed empty arrayempty mixed two parsetype multiple object stale wrong title type duplicate shrink output; do
      fault=$problem-$step; prep_reset "$fault"
      if external_action; then echo "Accepted preparation fault: $fault" >&2; exit 1; fi
      [[ ! -e $art/action-ack && -e $art/action-request ]]
      cp "$art/operations" "$art/prep-fault-$fault.operations"
      ((prep_checks+=1))
    done
  done
  for fault in unsafe-anchor oversized-request stale-owner missing-owner write-failure; do
    prep_reset; owner_token=${owned[$client]}
    case $fault in
      unsafe-anchor) jq '.rect.y=-16' "$art/state.json" > "$art/state.tmp"; mv "$art/state.tmp" "$art/state.json";;
      oversized-request) stage_request 'prepare-retry 9999 800\n';;
      stale-owner) owned[$client]=stale;;
      missing-owner) unset 'owned[$client]';;
      write-failure) mkdir "$art/action-ack.tmp";;
    esac
    if external_action; then echo "Accepted preparation fault: $fault" >&2; exit 1; fi
    [[ ! -e $art/action-ack ]]
    if [[ $fault != write-failure ]]; then [[ ! -s $art/operations ]]; fi
    owned[$client]=$owner_token; unset 'identity_conflicts[$client]' # action_reset archives the write-failure directory too.
    ((prep_checks+=1))
  done
  prep_reset
  jq '.rect.x=32 | .rect.width=1200' "$art/state.json" > "$art/state.tmp"; mv "$art/state.tmp" "$art/state.json"
  external_action
  jq -e '.rect.x==32 and .rect.y==0 and .rect.width==1056 and .rect.height==800' "$art/state.json" > /dev/null
  mkdir "$art/reply-write.reply.json"
  before_write=$(wc -l < "$art/operations")
  if native_action_ipc '[con_id=5] move position 0 0' "$art/reply-write"; then exit 1; fi
  [[ $(wc -l < "$art/operations") == "$before_write" ]]
  for action_mode in tiled floating; do
    for problem in transport false mixed two multiple; do
      if [[ $action_mode == tiled ]]; then step=floating-disable; else step=fullscreen-disable; fi
      prep_reset "$problem-$step" "$action_mode"
      if external_action; then exit 1; fi
      [[ ! -e $art/action-ack && $(wc -l < "$art/operations") == 1 ]]
      ((prep_checks+=1))
    done
  done
  prep_reset none floating; external_action
  prep_reset none tiled; external_action
  printf 'PASS 6 preparation metric/scale combinations and %s failclosed dispatcher controls; target restore after modeled80x24; each scoped IPC validated, no ACK on failure\n' "$prep_checks" > "$art/preparation-tests.log"
)
# Deterministic non-GUI shutdown windows against the production shell helpers.
(
  art=$art/shutdown-controls; mkdir -p "$art"
  owned=(); audited=()
  trap 'stop_owned > /dev/null 2>&1' EXIT
  clear_session_endpoints
  backend=x11; export DISPLAY=:136 XDG_RUNTIME_DIR=/tmp/lispterm-mock136
  for status in 0 7; do
    bash -c 'while [[ ! -f $1 ]]; do sleep .01; done; exit "$2"' bash "$art/release$status" "$status" & client=$!
    remember "$client"; is_owned "$client"; audit_client_tree
    # Reproduce exit precisely between the loop condition and the audit call.
    is_owned "$client"; touch "$art/release$status"
    wait "$client" || [[ $? == "$status" ]]
    if audit_client_tree; then echo 'Startup accepted absent root' >&2; exit 1; fi
    audit_client_tree allow-exit
    if fixture_wait_client; then actual=0; else actual=$?; fi
    [[ $actual == "$status" ]]
  done
  # No ownership record is not an observed shutdown, even for an absent PID.
  unset 'owned[$client]'
  if audit_client_tree allow-exit; then exit 1; fi
  sleep 60 & client=$!; remember "$client"
  token=${owned[$client]}; owned[$client]=stale
  if audit_client_tree allow-exit; then echo 'Stale live root accepted' >&2; exit 1; fi
  owned[$client]=$token
  audit_client_tree allow-exit
  # A live endpoint mismatch still fails, including a previously audited orphan.
  DBUS_SESSION_BUS_ADDRESS=unix:/invalid/control sleep 60 & poisoned=$!; remember "$poisoned"
  saved_client=$client; client=$poisoned
  if audit_client_tree allow-exit; then echo 'Live poisoned root accepted' >&2; exit 1; fi
  client=$saved_client
  sleep 60 & descendant=$!; remember "$descendant"
  ps() { if [[ $* == "-o pid= --ppid $client" ]]; then printf '%s\n' "$descendant"; fi; }
  audit_client_tree
  definition=$(declare -f audit_endpoint_file)
  eval "${definition/audit_endpoint_file/original_audit_endpoint_file}"
  audit_endpoint_file() {
    if [[ $1 == /proc/$descendant/environ && ${vanish:-0} == 1 ]]; then
      kill "$descendant"; wait "$descendant" 2>/dev/null || true
    fi
    original_audit_endpoint_file "$1"
  }
  vanish=1; audit_client_tree allow-exit
  vanish=0
  # Keep an audited descendant live after root exit and force a reader rejection.
  descendant=$poisoned; audited[$descendant]=${owned[$descendant]}
  kill "$client"; wait "$client" 2>/dev/null || true
  if audit_client_tree allow-exit; then echo 'Live orphan endpoint mismatch ignored' >&2; exit 1; fi
  stop_owned
  wait "$poisoned" 2>/dev/null || true
  trap - EXIT
)
# Exercise the production ERR reporter in a fresh errexit scope. Do not log command text.
set +e
(
  set -Ee
  art=$art/shutdown-controls
  trap 'fixture_error "$?" "${BASH_SOURCE[0]}" "$LINENO" "${FUNCNAME[0]:-main}"' ERR
  bash -c 'exit 9'
)
error_status=$?
set -e
[[ $error_status == 9 ]]
grep -Eq '^status=9 source=[^ ]+ line=[0-9]+ function=main$' "$art/shutdown-controls/shell-errors.log"
printf 'PASS shutdown root0/7 and wait propagation, strict startup/identity/live endpoints, vanished descendant, live orphan, ERR status/location\n' > "$art/shutdown-tests.log"
# Area-adapter destination and post-close receipt against the production helpers.
# Only children launched here are signalled; descendant enumeration is mocked.
(
  harness_art=$art ctl=$art/adapter-controls; mkdir -m 700 "$ctl"
  request=$ctl/area-request.sexp; printf '(:area :keyboard)\n' > "$request"
  request_sha=$(sha256sum "$request" | cut -d' ' -f1)
  reject() {
    local code=0
    fixture_artifact_root "$1" "$2" > /dev/null 2>> "$ctl/reject.stderr" || code=$?
    [[ $code == 2 ]] || { echo "Artifact root accepted: $1 ($code)" >&2; return 1; }
  }
  mkdir -m 700 "$ctl"/{run1,run2,run3,run4,run5,plain,group,world,normal}
  chmod 770 "$ctl/group"; chmod 703 "$ctl/world"
  ln -s plain "$ctl/link"; ln -s "$request" "$ctl/request-link"
  run1=$(fixture_artifact_root "$ctl/run1/recorder" "$request")
  [[ $run1 == "$ctl/run1/recorder" && -d $run1 && ! -L $run1 && $(stat -c %a "$run1") == 700 ]]
  reject "$run1" "$request"
  reject run1/recorder "$request"
  for bad in "$ctl/plain/../plain/recorder" "$ctl/plain/./recorder" "$ctl//plain/recorder" "$ctl/plain/recorder/" "$ctl/plain/other" \
             "$ctl/link/recorder" "$ctl/group/recorder" "$ctl/world/recorder" "$ctl/missing/recorder"; do
    reject "$bad" "$request"
  done
  for bad_request in '' area-request.sexp "$ctl/request-link" "$ctl/absent.sexp" "$ctl"; do
    reject "$ctl/plain/recorder" "$bad_request"
  done
  code=0; LISPTERM_VTTEST_ARTIFACT_ROOT=$ctl/plain/recorder LISPTERM_VTTEST_AREA_REQUEST='' fixture_artifact_dir "$ctl/normal" x11 1 > /dev/null 2>&1 || code=$?
  [[ $code == 2 ]]
  [[ ! -e $ctl/plain/recorder && ! -e $ctl/group/recorder && ! -e $ctl/world/recorder && ! -e $ctl/missing && -z $(ls -A "$ctl/normal") ]]
  [[ $(LISPTERM_VTTEST_ARTIFACT_ROOT=$ctl/plain/recorder LISPTERM_VTTEST_AREA_REQUEST=$request fixture_artifact_dir "$ctl/normal" x11 1) == "$ctl/plain/recorder" ]]
  # Normal runs: a fresh mktemp directory per call, with the original name pattern.
  unset LISPTERM_VTTEST_ARTIFACT_ROOT
  normal1=$(fixture_artifact_dir "$ctl/normal" x11 1); normal2=$(fixture_artifact_dir "$ctl/normal" wayland 2)
  [[ -d $normal1 && -d $normal2 && ${normal1%.*} == "$ctl/normal/vttest-x11-scale1-v3" && ${normal2%.*} == "$ctl/normal/vttest-wayland-scale2-v3" ]]
  suffix=${normal1##*.}
  [[ ${#suffix} == 6 && $(ls -A "$ctl/normal" | wc -l) == 2 ]]
  token1=$(fixture_title_token); token2=$(fixture_title_token)
  [[ $token1 =~ ^[[:xdigit:]]{16}$ && $token2 =~ ^[[:xdigit:]]{16}$ && $token1 != "$token2" ]]
  # Receipt fixtures. Launch identity values mirror the gate's adapter mode.
  owned=(); audited=(); roles=(); direct_children=(); reaped=(); adoption_failures=(); identity_conflicts=(); identity_failures=()
  cp "$request" "$run1/area-request.sexp"
  trap 'stop_owned > /dev/null 2>&1' EXIT
  gate_pid=$BASHPID gate_start=$(process_token "$BASHPID") area_request_sha=$request_sha runtime=''
  export LISPTERM_VTTEST_TITLE=Lispterm-vttest-$token1-$BASHPID
  await_file() { local i; for ((i=0;i<300;i++)); do [[ -s $1 ]] && return 0; sleep .01; done; return 1; }
  fresh_run() { art=$(fixture_artifact_root "$ctl/$1/recorder" "$request"); cp "$request" "$art/area-request.sexp"; owned=(); roles=(); direct_children=(); reaped=(); adoption_failures=(); identity_conflicts=(); identity_failures=(); }
  art=$run1 backend=wayland
  bash -c 'sleep 60 & echo "$!" > "$1"; wait' bash "$ctl/server-child" & server=$!
  bash -c 'trap "" TERM; sleep 60 & echo "$!" > "$1"; while :; do sleep .05; done' bash "$ctl/client-child" & client=$!
  sleep 60 & holder=$!
  remember_role server "$server" direct; remember_role client "$client" direct; remember_role keyboard-holder "$holder" direct
  await_file "$ctl/server-child"; await_file "$ctl/client-child"
  server_child=$(< "$ctl/server-child"); client_child=$(< "$ctl/client-child")
  ps() {
    if [[ $* == "-o pid= --ppid $server" ]]; then printf '%s\n' "$server_child"
    elif [[ $* == "-o pid= --ppid $client" ]]; then printf '%s\n' "$client_child"; fi
  }
  # The holder exits and is reaped before close; its adopted identity is kept.
  kill "$holder"; code=0; wait_direct_child "$holder" 2>/dev/null || code=$?
  [[ $code == 143 && ${reaped[$holder]} == 143 ]]
  [[ -v owned[$holder] && ${roles[$holder]} == keyboard-holder && $(owned_state "$holder") == absent ]]
  stop_status=0; stop_owned || stop_status=1
  [[ $stop_status == 0 && ${roles[$server_child]} == descendant && ${roles[$client_child]} == descendant ]]
  grep -Fxq "KILL $client ${owned[$client]}" "$art/cleanup.log"
  grep -Fxq "KILL $client_child ${owned[$client_child]}" "$art/cleanup.log"
  fixture_cleanup_receipt "$stop_status"
  [[ -v reaped[$server] && ${reaped[$client]} == 137 && ${reaped[$holder]} == 143 ]]
  receipt_sha=$(sha256sum "$art/cleanup-receipt.sexp" | cut -d' ' -f1)
  # Exclusive: a second write fails and leaves the first receipt intact.
  code=0; fixture_cleanup_receipt "$stop_status" || code=$?
  [[ $code == 2 && $(sha256sum "$art/cleanup-receipt.sexp" | cut -d' ' -f1) == "$receipt_sha" ]]
  fixture_provenance
  grep -Eq "^$receipt_sha  \./cleanup-receipt\.sexp\$" "$art/provenance.sha256"
  (cd "$art"; sha256sum --quiet -c provenance.sha256)
  unset -f ps
  # A survivor that no signal reaches: entry and overall failed, nonzero result.
  fresh_run run2; run2=$art
  sleep 60 & survivor=$!; remember_role server "$survivor" direct
  sleep 60 & holder=$!; remember_role keyboard-holder "$holder" direct
  kill() { [[ $* == *" $survivor" ]] && return 0; builtin kill "$@"; }
  stop_status=0; stop_owned || stop_status=1
  unset -f kill
  code=0; fixture_cleanup_receipt "$stop_status" || code=$?
  [[ $stop_status == 1 && $code == 1 && $(owned_state "$survivor") == live ]]
  grep -Fxq "SURVIVED $survivor" "$art/cleanup.log"
  kill "$survivor"; wait "$survivor" 2>/dev/null || true
  # An unreadable probe is unknown even though is_owned and stop_owned see nothing.
  fresh_run run3; run3=$art
  sleep 60 & probe=$!; remember_role server "$probe" direct
  sleep 60 & holder=$!; remember_role keyboard-holder "$holder" direct
  cat() { [[ $* != *"/proc/$probe/stat"* ]] || return 1; command cat "$@"; }
  stop_status=0; stop_owned || stop_status=1
  code=0; fixture_cleanup_receipt "$stop_status" || code=$?
  unset -f cat
  [[ $stop_status == 0 && $code == 1 && ! -v reaped[$probe] && $(owned_state "$probe") == live ]]
  kill "$probe"; wait "$probe" 2>/dev/null || true
  # A required Wayland holder that was never adopted is incomplete, not stopped.
  fresh_run run4; run4=$art
  sleep 60 & server=$!; remember_role compositor "$server"
  stop_status=0; stop_owned || stop_status=1
  code=0; fixture_cleanup_receipt "$stop_status" || code=$?
  [[ $stop_status == 0 && $code == 1 ]]
  wait "$server" 2>/dev/null || true
  # X11 has no keyboard holder.
  fresh_run run5; run5=$art; backend=x11
  sleep 60 & server=$!; remember_role server "$server" direct
  stop_status=0; stop_owned || stop_status=1
  fixture_cleanup_receipt "$stop_status"
  # Fail closed with no owned processes, including failures before request copy.
  new_control() { mkdir -m 700 "$ctl/$1"; fresh_run "$1"; }
  expect_unknown() {
    local result=0
    fixture_cleanup_receipt 0 || result=$?
    [[ $result == 1 ]]
    grep -q '^ :state :unknown ' "$art/cleanup-receipt.sexp"
  }
  saved_pid=$gate_pid saved_start=$gate_start saved_title=$LISPTERM_VTTEST_TITLE
  for binding in gate_pid gate_start LISPTERM_VTTEST_TITLE area_request_sha; do
    new_control "missing-$binding"
    printf -v "$binding" '%s' ''
    expect_unknown
    grep -q ':launch-valid nil ' "$art/cleanup-receipt.sexp"
    gate_pid=$saved_pid gate_start=$saved_start LISPTERM_VTTEST_TITLE=$saved_title area_request_sha=$request_sha
  done
  new_control mismatch-digest
  printf 'changed request\n' >> "$art/area-request.sexp"
  expect_unknown
  new_control missing-request
  mv "$art/area-request.sexp" "$art/retained-request.sexp"
  expect_unknown
  new_control nonnormalized-path
  art=$art/.
  expect_unknown
  new_control early-x11
  early_art=$art
  # Exercise the actual production EXIT cleanup, with publication helpers stubbed.
  eval "$(sed -n '/^cleanup() {$/,/^}$/p' tests/vttest-gate.sh)"
  code=0
  (
    artifact_root=$art area_request_sha='' runtime='' mode=record
    fixture_latest_pointer() { :; }
    fixture_provenance() { :; }
    trap cleanup EXIT
    exit 9
  ) > "$art/early.stdout" 2> "$art/early.stderr" || code=$?
  printf '%s\n' "$code" > "$art/early.status"
  [[ $code == 1 && $(< "$art/harness.exit") == 1 ]]
  grep -q '^ :state :unknown ' "$art/cleanup-receipt.sexp"
  # A previously waited client retains its first ordinary nonzero exit status.
  new_control waited-client
  bash -c 'while [[ ! -f $1 ]]; do sleep .01; done; exit 7' bash "$art/release" & client=$!
  remember_role client "$client" direct
  touch "$art/release"
  code=0; fixture_wait_client || code=$?
  [[ $code == 7 && ${reaped[$client]} == 7 ]]
  code=0; wait_direct_child "$client" || code=$?
  [[ $code == 7 && ${reaped[$client]} == 7 ]]
  fixture_cleanup_receipt 0
  grep -q ':wait-status 7)' "$art/cleanup-receipt.sexp"
  # An absent identity without any wait evidence cannot prove direct-child reap.
  new_control unrecorded-wait
  bash -c 'while [[ ! -f $1 ]]; do sleep .01; done' bash "$art/release" & client=$!
  remember_role client "$client" direct
  touch "$art/release"; wait "$client"
  [[ ! -v reaped[$client] ]]
  code=0; write_cleanup_receipt 0 none || code=$?
  [[ $code == 1 ]]
  grep -q ':state :unknown :observed :absent :direct t :wait-status nil)' "$art/cleanup-receipt.sexp"
  # A failed wait cannot be upgraded by another wait, even on an absent identity.
  new_control wait127
  bash -c 'while [[ ! -f $1 ]]; do sleep .01; done' bash "$art/release" & client=$!
  remember_role client "$client" direct
  touch "$art/release"; wait "$client"
  wait() { return 127; }
  expect_unknown
  unset -f wait
  code=0; wait_direct_child "$client" || code=$?
  [[ $code == 127 && ${reaped[$client]} == 127 ]]
  grep -q ':wait-status 127)' "$art/cleanup-receipt.sexp"
  # Token-read failure is explicit; no unowned PID is signalled or probed later.
  new_control adoption-failure
  bash -c 'while [[ ! -f $1 ]]; do sleep .01; done; exit 7' bash "$art/release" & unadopted=$!
  cat() { [[ $* != "/proc/$unadopted/stat" ]] || return 1; command cat "$@"; }
  remember_role client "$unadopted" direct
  unset -f cat
  [[ ! -v owned[$unadopted] && ${#adoption_failures[@]} == 1 && ${adoption_failures[0]} == *':role "client"'* ]]
  touch "$art/release"; code=0; wait "$unadopted" || code=$?
  [[ $code == 7 ]]
  expect_unknown
  grep -q ":pid $unadopted :reason :initial-token-unreadable)" "$art/cleanup-receipt.sexp"
  grep -q ':launch-valid t ' "$art/cleanup-receipt.sexp"
  # Synthetic PID/start identities: no OS process lookup, enumeration or signals.
  (
    mock_pid=424242 mock_start=101
    process_token() { [[ -n $mock_start ]] || return 1; printf '%s\n' "$mock_start"; }
    owned_state() { echo absent; }
    wait() { printf '%s\n' "$1" >> "$ctl/mock-waits"; return 7; }
    kill() { echo 'unexpected mock signal' >&2; return 1; }
    ps() { echo 'unexpected mock enumeration' >&2; return 1; }
    new_control repeated-identity
    remember_role client "$mock_pid" direct
    remember "$mock_pid"
    remember_role client "$mock_pid" direct
    [[ ${owned[$mock_pid]} == 101 && ${roles[$mock_pid]} == client && -v direct_children[$mock_pid] && ${#identity_failures[@]} == 0 ]]
    code=0; wait_direct_child "$mock_pid" || code=$?
    [[ $code == 7 && ${reaped[$mock_pid]} == 7 ]]
    remember "$mock_pid"; remember_role client "$mock_pid" direct
    code=0; wait_direct_child "$mock_pid" || code=$?
    [[ $code == 7 && $(wc -l < "$ctl/mock-waits") == 1 ]]
    fixture_cleanup_receipt 0
    grep -q ':start "101" :state :stopped :observed :absent :direct t :wait-status 7)' "$art/cleanup-receipt.sexp"
    # A reused descendant PID cannot inherit the saved direct-client wait.
    new_control reused-identity
    remember_role client "$mock_pid" direct
    code=0; wait_direct_child "$mock_pid" || code=$?
    [[ $code == 7 ]]
    mock_start=202
    remember "$mock_pid"
    remember_role keyboard-holder "$mock_pid" direct
    [[ ${owned[$mock_pid]} == 101 && ${roles[$mock_pid]} == client && ${reaped[$mock_pid]} == 7 && -v identity_conflicts[$mock_pid] ]]
    code=0; wait_direct_child "$mock_pid" || code=$?
    [[ $code == 127 && ${reaped[$mock_pid]} == 7 && $(wc -l < "$ctl/mock-waits") == 2 ]]
    if is_owned "$mock_pid"; then echo 'reused PID treated as owned' >&2; exit 1; fi
    stop_owned
    expect_unknown
    grep -q ':start "101" :state :unknown :observed :absent :direct t :wait-status 7)' "$art/cleanup-receipt.sexp"
    grep -q ':pid 424242 :start "101" :observed-start "202" :reason :pid-reused)' "$art/cleanup-receipt.sexp"
    # Each failed initial adoption is retained even after that PID was recorded.
    new_control seen-adoption-failure
    mock_start=101; remember_role client "$mock_pid" direct
    mock_start=''
    remember_role keyboard-holder "$mock_pid" direct
    remember_role server "$mock_pid" direct
    [[ ${#adoption_failures[@]} == 2 && ${owned[$mock_pid]} == 101 && ${roles[$mock_pid]} == client && ! -v reaped[$mock_pid] ]]
    expect_unknown
    grep -q ':role "keyboard-holder" :pid 424242 :reason :initial-token-unreadable)' "$art/cleanup-receipt.sexp"
    grep -q ':role "server" :pid 424242 :reason :initial-token-unreadable)' "$art/cleanup-receipt.sexp"
    printf 'PASS same PID/start repeated tracking and one saved wait; reused PID preserves original identity/role/wait, blocks ownership and wait transfer, cleanup unknown; every seen-PID adoption failure retained\n' > "$ctl/identity-controls.log"
  )
  printf 'PASS missing bindings/request, digest mismatch, normalized path, actual early X11 EXIT cleanup nonzero, first client7/holder143 retained, absent unrecorded wait, wait127 rejected, explicit adoption failure\n' > "$ctl/blocking-controls.log"
  trap - EXIT
  # Read each receipt as data and compare its structure, not its formatting.
  RECEIPTS=$run1:$run2:$run3:$run4:$run5 scripts/sbcl --script /dev/stdin > "$ctl/receipts.txt" <<'LISP'
(defun split-receipts (string)
  (loop with start = 0 for pos = (position #\: string :start start)
        collect (subseq string start pos) while pos do (setf start (1+ pos))))
(let ((*read-eval* nil) (*package* (find-package "CL-USER")))
  (dolist (dir (split-receipts (sb-ext:posix-getenv "RECEIPTS")))
    (with-open-file (in (concatenate 'string dir "/cleanup-receipt.sexp"))
      (let* ((form (read in)) (launch (getf form :launch)))
        (assert (eq (read in nil :eof) :eof))
        (assert (and (eq (getf form :kind) :vttest-gate-cleanup) (eql (getf form :version) 1)
                     (eq (getf form :phase) :post-close) (integerp (getf launch :gate-pid))
                     (stringp (getf launch :gate-start)) (stringp (getf launch :title))))
        (dolist (entry (getf form :owned))
          (assert (and (integerp (getf entry :pid)) (stringp (getf entry :start)))))
        (format t "RECEIPT ~a ~a ~(~a ~a~)~{ ~a~}~%" (getf launch :artifacts) (getf launch :area-request-sha256)
                (getf form :state) (getf form :wayland-holder)
                (sort (mapcar (lambda (entry) (format nil "~a=~(~a~)" (getf entry :role) (getf entry :state)))
                              (getf form :owned))
                      #'string<))))))
LISP
  receipts=$ctl/receipts.txt
  grep -Fxq "RECEIPT $run1 $request_sha stopped stopped client=stopped descendant=stopped descendant=stopped keyboard-holder=stopped server=stopped" "$receipts"
  grep -Fxq "RECEIPT $run2 $request_sha failed stopped keyboard-holder=stopped server=failed" "$receipts"
  grep -Fxq "RECEIPT $run3 $request_sha unknown stopped keyboard-holder=stopped server=unknown" "$receipts"
  grep -Fxq "RECEIPT $run4 $request_sha incomplete missing compositor=stopped" "$receipts"
  grep -Fxq "RECEIPT $run5 $request_sha stopped not-applicable server=stopped" "$receipts"
  [[ $(grep -c '^RECEIPT ' "$receipts") == 5 ]]
  printf 'PASS exclusive RUN/recorder destination, %s rejected destinations/requests, normal mktemp path; receipts: roles+descendants stopped, reaped holder143 retained, TERM-ignoring KILL137, survivor failed, unknown probe, missing holder incomplete, X11 not-applicable, exclusive write, provenance; blocking launch/wait/adoption controls\n' "$(grep -c . "$ctl/reject.stderr")" > "$harness_art/adapter-tests.log"
)
# Pure/mock isolation tests, with a real disposable lock but no GUI processes.
fixture_scale x11 1; fixture_scale wayland 1; fixture_scale wayland 2
for spec in 'x11 2' 'wayland 0' 'wayland 3' 'wayland 02' 'wayland 1.0' 'unknown 1'; do
  read -r b s <<< "$spec"
  if fixture_scale "$b" "$s"; then echo "Invalid scale accepted: $spec" >&2; exit 1; fi
done
(
  clear_session_endpoints
  export XDG_RUNTIME_DIR=/tmp/lispterm-mock WAYLAND_DISPLAY=wayland-1 SWAYSOCK=/tmp/lispterm-mock/ipc
  backend=wayland
  printf 'XDG_RUNTIME_DIR=%s\0WAYLAND_DISPLAY=%s\0SWAYSOCK=%s\0' "$XDG_RUNTIME_DIR" "$WAYLAND_DISPLAY" "$SWAYSOCK" > "$art/mock-env"
  audit_endpoint_file "$art/mock-env"
  for bad in 'DISPLAY=:0' 'XAUTHORITY=/user/auth' 'DBUS_SESSION_BUS_ADDRESS=unix:/user/bus' 'XDG_RUNTIME_DIR=/run/user/1000' 'WAYLAND_DISPLAY=wayland-0' 'SWAYSOCK=/user/ipc'; do
    cp "$art/mock-env" "$art/mock-bad-env"; printf '%s\0' "$bad" >> "$art/mock-bad-env"
    if audit_endpoint_file "$art/mock-bad-env"; then echo "Endpoint accepted: ${bad%%=*}" >&2; exit 1; fi
  done
  backend=x11; export DISPLAY=:123
  unset WAYLAND_DISPLAY SWAYSOCK
  printf 'XDG_RUNTIME_DIR=%s\0DISPLAY=%s\0' "$XDG_RUNTIME_DIR" "$DISPLAY" > "$art/mock-env"
  audit_endpoint_file "$art/mock-env"
  printf 'DISPLAY=:0\0' >> "$art/mock-env"
  if audit_endpoint_file "$art/mock-env"; then exit 1; fi
)
mkdir -p "$art/mock-fd"
ln -s 'socket:[123]' "$art/mock-fd/4"
printf 'Num RefCount Protocol Flags Type St Inode Path\n000: 2 0 10000 1 01 123 /private/socket\n001: 2 0 10000 1 01 456 /user/socket\n' > "$art/mock-unix"
socket_inode_match 0 /private/socket "$art/mock-fd" "$art/mock-unix" > "$art/mock-inode-match"
if socket_inode_match 0 /user/socket "$art/mock-fd" "$art/mock-unix"; then exit 1; fi
if socket_inode_match 0 /missing/socket "$art/mock-fd" "$art/mock-unix"; then exit 1; fi
# A matching pathname without an owned socket inode is not proof.
rm "$art/mock-fd/4"; ln -s /private/socket "$art/mock-fd/4"
if socket_inode_match 0 /private/socket "$art/mock-fd" "$art/mock-unix"; then exit 1; fi
(
  scale=2
  printf '[{"name":"HEADLESS-1","active":true,"scale":2,"current_mode":{"width":2560,"height":1600}}]\n' > "$art/mock-outputs"
  fixture_outputs "$art/mock-outputs"
  for bad in '.[0].name="DP-1"' '.[0].scale=1' '.[0].current_mode.width=1280' '.[0].active=false'; do
    jq "$bad" "$art/mock-outputs" > "$art/mock-bad-outputs"
    if fixture_outputs "$art/mock-bad-outputs"; then exit 1; fi
  done
)
(
  # Never contend with the live GUI lock in non-GUI tests.
  root=$art/mock-lock-root; mkdir -p "$root/.build"
  export LISPTERM_GUI_LOCK_HELD=1
  exec 9>&-
  if fixture_lock 2>/dev/null; then echo 'Marker-only lock accepted' >&2; exit 1; fi
  exec 9> "$root/.build/vttest-gui.lock"
  if fixture_lock; then echo 'Unlocked FD9 accepted' >&2; exit 1; fi
  unset LISPTERM_GUI_LOCK_HELD
  fixture_lock
  (fixture_lock) # Real inherited held FD, no nested flock.
  LISPTERM_VTTEST_FUNCTIONS_ONLY=1 LISPTERM_GUI_LOCK_HELD=1 bash -c 'source tests/vttest-gate.sh; root=$1; fixture_lock' bash "$root"
  if flock -n "$root/.build/vttest-gui.lock" true; then echo 'Lock was not held' >&2; exit 1; fi
)
printf 'PASS scale validation, endpoint poisoning, socket-inode join, headless outputs, inherited FD9 proof\n' > "$art/isolation-tests.log"
# Exercise the production registration helper without compositor or client launches.
(
  LISPTERM_NATIVE_FUNCTIONS_ONLY=1 source tests/vt100-native-geometry.sh
  LISPTERM_SWAYMSG=mock-swaymsg
  title=Lispterm-native-rule-control-133
  rule_checks=0
  timeout() {
    local number rule
    [[ $# == 3 && $1 == 5 && $2 == mock-swaymsg ]] || return 90
    number=$(( $(wc -l < "$art/rule-calls") + 1 ))
    case $number in 1) rule='floating enable';; 2) rule='border none';; 3) rule='move position 0 0';; *) return 91;; esac
    [[ $3 == "for_window [title=\"^$title\$\"] $rule" ]] || return 92
    printf '%s\n' "$3" >> "$art/rule-calls"
    if [[ $number != "$fail_at" ]]; then printf '[{"success":true}]\n'; return; fi
    case $reply_kind in
      exit) printf '[{"success":true}]\n'; return 2;;
      empty) return 0;;
      malformed) printf '{broken\n';;
      false) printf '[{"success":false,"parse_error":true}]\n';;
      mixed) printf '[{"success":true},{"success":false}]\n';;
      extra) printf '[{"success":false}]\n[{"success":true}]\n';;
      parse) printf '[{"success":true,"parse_error":true}]\n';;
    esac
  }
  : > "$art/rule-calls"
  fail_at=0 reply_kind=success
  native_window_rules "$title" "$art/rule-success.json"
  [[ $(wc -l < "$art/rule-calls") == 3 ]]
  jq -es 'length == 3 and all(.[]; length == 1 and .[0].success == true)' "$art/rule-success.json" > /dev/null
  ((rule_checks+=1))
  for fail_at in 1 2 3; do
    for reply_kind in exit empty malformed false mixed extra parse; do
      : > "$art/rule-calls"
      if native_window_rules "$title" "$art/rule-$fail_at-$reply_kind.json" 2> "$art/rule-$fail_at-$reply_kind.stderr"; then
        echo "Registration accepted $reply_kind at $fail_at" >&2; exit 1
      fi
      [[ $(wc -l < "$art/rule-calls") == "$fail_at" ]]
      ((rule_checks+=1))
    done
  done
  export -f native_window_rules timeout
  export art title LISPTERM_SWAYMSG fail_at reply_kind
  reply_kind=exit
  for fail_at in 1 2 3; do
    : > "$art/rule-calls"
    code=0
    bash -c 'set -euo pipefail; native_window_rules "$title" "$art/rule-errexit-$fail_at.json"; touch "$art/rule-continued"' || code=$?
    [[ $code == 1 && ! -e $art/rule-continued && $(wc -l < "$art/rule-calls") == "$fail_at" ]]
    ((rule_checks+=1))
  done
  for bad_title in '' 'foreign.*' 'owned"' 'owned; border none' 'owned, move position 0 0' 'owned]' 'owned\\' $'owned\nforeign'; do
    : > "$art/rule-calls"
    if native_window_rules "$bad_title" "$art/rule-invalid.json"; then exit 1; fi
    [[ ! -s $art/rule-calls && ! -e $art/rule-invalid.json ]]
    ((rule_checks+=1))
  done
  printf 'PASS %s registration controls: exact selector per action, ordered policies, IPC/reply failure stops, invalid selector rejection\n' "$rule_checks" > "$art/window-rule-tests.log"
)
export LISPTERM_SBCL=${LISPTERM_SBCL:-$root/.build/deps/bin/sbcl}
export LISPTERM_HARNESS_TEST_ARTIFACTS="$art/fixtures-$(date +%s%N)/"
export LISPTERM_VTTEST_ARTIFACTS="$LISPTERM_HARNESS_TEST_ARTIFACTS"
scripts/sbcl --script tests/vttest-protocol-tests.lisp > "$art/protocol-tests.log" 2>&1
scripts/sbcl --script tests/vttest-requested-columns-tests.lisp > "$art/requested-columns-tests.log" 2>&1
# The definition-only recorder reader skips LOAD forms; supply its selection package.
scripts/sbcl --load tests/vttest-selection.lisp --script tests/vttest-recorder-tests.lisp > "$art/recorder-tests.log" 2>&1
bash -n tests/vttest-gate.sh tests/vttest-harness-tests.sh tests/verify.sh tests/wayland-gate.sh
# Retain a negative-control process; it is ours, but deliberately not in the
# production cleanup ownership set. No existing process is inspected or signalled.
bash -c 'trap "" TERM; while :; do sleep 10; done' & fixture_root=$!
remember "$fixture_root"
bash -c 'sleep 30' & sentinel=$!
sentinel_token=$(process_token "$sentinel")
cleanup_fixture() {
  set +e
  stop_owned
  if [[ $(process_token "$sentinel" 2>/dev/null) == "$sentinel_token" ]]; then kill "$sentinel"; fi
  wait "$sentinel" 2>/dev/null
}
trap cleanup_fixture EXIT
sleep .15
collect_tree "$fixture_root"
printf 'root=%s owned=%s sentinel=%s\n' "$fixture_root" "${!owned[*]}" "$sentinel" > "$art/fixture-processes.txt"
(( ${#owned[@]} >= 2 ))
# A simulated stale PID identity must not be signalled. Registering an incorrect
# start token exercises the exact production guard without PID-reuse tricks.
owned[$sentinel]=not-the-start-token
stop_owned
[[ $(process_token "$sentinel") == "$sentinel_token" ]]
if watchdog_check "$SECONDS"; then echo 'Expired watchdog succeeded' >&2; exit 1; else [[ $? == 124 ]]; fi
watchdog_check "$((SECONDS + 10))"
for pid in "${!owned[@]}"; do [[ $pid == "$sentinel" ]] || ! is_owned "$pid"; done
printf 'PASS bounded TERM/KILL owned descendants\nPASS unowned/stale-token sentinel untouched\nPASS watchdog returns124 at deadline\n' > "$art/cleanup-tests.log"
# Dispose only of our negative control, after its survival was established.
owned=(); remember "$sentinel"; stop_owned
wait "$fixture_root" 2>/dev/null || true
wait "$sentinel" 2>/dev/null || true
trap - EXIT
for dir in vttest-x11.soyWA3 vttest-wayland.4pz8fm vttest-wayland.P7So6b; do
  [[ -d .build/evidence/$dir ]] || continue
  tests/verify.sh vttest-check measure ".build/evidence/$dir" > "$art/$dir.measure.log"
  if tests/verify.sh vttest-check accept ".build/evidence/$dir" > "$art/$dir.reject.log"; then
    echo 'Legacy baseline incorrectly accepted' >&2; exit 1
  fi
done
git diff --check
cat "$art/protocol-tests.log" "$art/recorder-tests.log" "$art/cleanup-tests.log" "$art/isolation-tests.log" "$art/pointer-tests.log" "$art/shutdown-tests.log" "$art/adapter-tests.log"
echo 'Offline/static/disposable harness checks passed; no GUI run.'
