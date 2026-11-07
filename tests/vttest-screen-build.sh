#!/usr/bin/env bash
# One archive-derived executable per screen run. Sourcing defines testable helpers only.
set -Eeuo pipefail
screen_step() {
    local name=$1; shift
    printf '%q ' "$@" > "$build/$name.command"; printf '\n' >> "$build/$name.command"
    local e=0
    "$@" </dev/null >"$build/$name.stdout" 2>"$build/$name.stderr" || e=$?
    printf '%s\n' "$e" > "$build/$name.exit"
    return "$e"
}
screen_members_safe() {
    local listing=$1 types=$2 member type rest count=0
    declare -A seen=()
    while IFS= read -r member; do
        [[ $member == vttest-20251205/ || $member == vttest-20251205/* ]] || return 1
        [[ $member != *'/../'* && $member != */.. && $member != *'/./'* && $member != *'//'* ]] || return 1
        [[ $member =~ ^[A-Za-z0-9_./+-]+$ && ! ${seen[$member]+yes} ]] || return 1
        seen[$member]=1; ((count+=1))
    done < "$listing"
    [[ $count -gt 1 && $count -le 512 ]] || return 1
    local n=0 total=0 mode owner size tail
    while IFS= read -r rest; do
        type=${rest:0:1}; [[ $type == - || $type == d ]] || return 1
        read -r mode owner size tail <<< "$rest"
        [[ $size =~ ^[0-9]+$ && ${#size} -le 8 ]] || return 1
        ((total+=10#$size)) || :
        [[ $total -le 8388608 ]] || return 1
        ((n+=1))
    done < "$types"
    [[ $n == "$count" ]]
}
screen_pin() {
    local name=$1 path=$2 linker=${3-}
    [[ $path == /* && -f $path && -x $path ]] || return 1
    if [[ $name == compiler-collect2 ]]; then
        [[ $linker == /* && -f $linker && -x $linker ]] || return 1
    fi
    printf '%s\t%s\t%s\n' "$name" "$path" "$(readlink -f -- "$path")" >> "$build/tools.tsv"
    sha256sum -- "$path" >> "$build/tools.sha256"
    if [[ $name == compiler-collect2 ]]; then
        # collect2 forwards --version to ld; bind its lookup to the selected linker.
        screen_step "tool-$name" /nix/store/cb987zkxvsv0490753ilrvn5yk7ylgxi-coreutils-9.12/bin/env "COMPILER_PATH=${linker%/*}" "$path" --version
    else
        screen_step "tool-$name" "$path" --version
    fi
}
screen_build_main() {
    [[ $# == 2 && $1 == /* && $2 == /* && -d $1 && ! -L $1 && -f $2 && ! -L $2 ]] || return 2
    local run=$1 archive=$2
    [[ -O $run && $(readlink -f -- "$run") == "$run" && $run =~ ^/[A-Za-z0-9_./+-]+$ && $archive =~ ^/[A-Za-z0-9_./+-]+$ ]] || return 2
    build=$run/build
    mkdir -m700 -- "$build" || return 1
    state=:fail; reason=preparation
    trap 'e=$?; printf "(:kind :vttest-screen-build :version 1 :status %s :reason \"%s\" :exit %s)\n" "$state" "$reason" "$e" > "$build/BUILD.sexp"' EXIT
    [[ $(stat -c %s -- "$archive") == 243249 ]] || return 1
    printf '%s  %s\n' cd6886f9aefe6a3f6c566fa61271a55710901a71849c630bf5376aa984bf77cc "$archive" > "$build/archive.expected.sha256"
    sha256sum -- "$archive" > "$build/archive.actual.sha256"
    sha256sum -c "$build/archive.expected.sha256" > "$build/archive.check" || return 1
    local tar gzip make cc ld ar ranlib tool
    # Only pre-existing store tools. Resolve once and record every selected path.
    tar=$(readlink -f "$(command -v tar)"); gzip=$(readlink -f "$(command -v gzip)")
    make=$(screen_local_tool gnumake make); cc=$(screen_local_tool gcc-wrapper gcc)
    ld=$(screen_local_tool binutils-wrapper ld); ar=$(screen_local_tool binutils-wrapper ar)
    ranlib=$(screen_local_tool binutils-wrapper ranlib)
    for tool in tar gzip make cc ld ar ranlib; do screen_pin "$tool" "${!tool}" || return 1; done
    for tool in sed awk grep find sort xargs sha256sum stat readlink; do screen_pin "$tool" "$(readlink -f "$(command -v "$tool")")" || return 1; done
    for tool in cc1 as collect2 ld; do
        local program; program=$("$cc" -print-prog-name="$tool")
        if [[ $program != /* ]]; then program=$(PATH="${cc%/*}:${ld%/*}:/run/current-system/sw/bin" command -v "$program"); fi
        screen_pin "compiler-$tool" "$program" "$ld" || return 1
    done
    screen_step compiler-detail "$cc" -v || return 1
    screen_step members "$tar" -tzf "$archive" || return 1
    screen_step member-types "$tar" -tvzf "$archive" || return 1
    reason=unsafe-archive; screen_members_safe "$build/members.stdout" "$build/member-types.stdout" || return 1
    mkdir -m700 "$build/source" "$build/home" "$build/cache" "$build/config" "$build/data" "$build/tmp"
    screen_step extract "$tar" --no-same-owner --no-same-permissions -xzf "$archive" -C "$build/source" || return 1
    local source=$build/source/vttest-20251205
    [[ -f $source/configure && ! -L $source/configure && -f $source/main.c ]] || return 1
    (cd "$source"; find . -type f -print0 | sort -z | xargs -0 sha256sum) > "$build/source.sha256"
    local env=/nix/store/cb987zkxvsv0490753ilrvn5yk7ylgxi-coreutils-9.12/bin/env
    local bash=/nix/store/kwyi4bq5s2qgwbhz6cs4gjy57c2zdxws-bash-interactive-5.3p20/bin/bash
    local path="${cc%/*}:${ld%/*}:${make%/*}:${tar%/*}:${gzip%/*}:/run/current-system/sw/bin"
    local -a clean=("$env" -i "PATH=$path" "HOME=$build/home" "XDG_CACHE_HOME=$build/cache" "XDG_CONFIG_HOME=$build/config" "XDG_DATA_HOME=$build/data" "TMPDIR=$build/tmp" LANG=C.UTF-8 LC_ALL=C.UTF-8 "CONFIG_SHELL=$bash" "SHELL=$bash" "CC=$cc" "LD=$ld" "AR=$ar" "RANLIB=$ranlib" CFLAGS=-O2 CPPFLAGS= LDFLAGS=)
    printf '%q ' "${clean[@]}" > "$build/environment.command"; printf '\n' >> "$build/environment.command"
    cd "$source"
    reason=configure
    screen_step configure "${clean[@]}" "$bash" --noprofile --norc ./configure --enable-echo || return 1
    for tool in config.log config.status config.h makefile; do [[ -f $tool && ! -L $tool ]] || return 1; sha256sum "$tool" >> "$build/configuration.sha256"; done
    reason=make
    screen_step make "${clean[@]}" "$make" -j1 V=1 || return 1
    reason=binary
    [[ -f $source/vttest && -x $source/vttest && ! -L $source/vttest ]] || return 1
    sha256sum -- "$source/vttest" > "$build/binary.sha256"
    stat -c '%a %s %n' -- "$source/vttest" > "$build/binary.stat"
    printf '%s\n' "$source/vttest" > "$build/binary.path"
    (cd "$source"; sha256sum -c "$build/source.sha256") > "$build/source-post.check" || return 1
    sha256sum -c "$build/tools.sha256" > "$build/tools-post.check" || return 1
    state=:pass; reason=executable-produced-effects-unverified
}
screen_local_tool() {
    local package=$1 program=$2 candidate
    for candidate in /nix/store/*-"$package"-*/bin/"$program"; do
        if [[ -x $candidate && -f $candidate ]]; then printf '%s\n' "$candidate"; return 0; fi
    done
    printf 'No local %s/%s\n' "$package" "$program" >&2; return 1
}
if [[ ${BASH_SOURCE[0]} == "$0" ]]; then
    umask 077
    screen_build_main "$@"
fi
