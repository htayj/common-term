#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
artifacts=$PWD/.build/autolith/checkpoint
mkdir -p "$artifacts"/{home,config,data,state,cache}
tests/wayland-gate.sh start "$artifacts/compositor"
trap 'tests/wayland-gate.sh stop "$artifacts/compositor"' EXIT
source "$artifacts/compositor/env.sh"
export HOME=$artifacts/home XDG_CONFIG_HOME=$artifacts/config XDG_DATA_HOME=$artifacts/data
export XDG_STATE_HOME=$artifacts/state XDG_CACHE_HOME=$artifacts/cache
export AUTOLITH_ASDF_CACHE=$artifacts/cache/autolith LISPTERM_TEST_BACKEND=wayland
unset AUTOLITH_SITE_CONFIG_ROOT AUTOLITH_ACTIVE_CORE
scripts/autolith-runtime --script tests/checkpoint-adapter.lisp < /dev/null > "$artifacts/client.log" 2>&1
cat "$artifacts/client.log"
