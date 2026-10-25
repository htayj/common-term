# Pin matches the installed Autolith 0.67.0 package's nixpkgs revision.
# nix build --impure --expr 'import ./scripts/deps.nix {}' --out-link .build/deps
{ system ? builtins.currentSystem, verification ? false }:
let
  source = builtins.getFlake "github:NixOS/nixpkgs/d482ef84049d9b7276b83a06e4e4d76983830097";
  pkgs = source.legacyPackages.${system};
  lisp = pkgs.sbcl.withPackages (ps: with ps; [
    alexandria babel bordeaux-threads cffi fiveam zpng
  ]);
in pkgs.symlinkJoin {
  name = if verification then "lispterm-test-tools" else "lispterm-runtime";
  paths = [ lisp pkgs.terminus_font pkgs.libxkbcommon ]
    ++ pkgs.lib.optionals verification (with pkgs; [
      xorg.xorgserver xdotool imagemagick sway-unwrapped wtype grim fastfetch
      bash coreutils git util-linux procps
    ]);
  # Stable runtime paths consumed by scripts/sbcl, independent of store hashes.
  postBuild = ''
    test -f "$out/share/fonts/terminus/ter-u16n.otb"
    test -f "$out/lib/libxkbcommon.so.0"
  '';
}
