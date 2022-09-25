{ pkgs ? import ./nixpkgs.nix, compiler ? null, extraOverrides ? _: _: { }
, modifier ? x: x }:

let
  haskellPackages = if compiler == null then
    pkgs.haskellPackages
  else
    pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  name = "";
  root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  overrides = with pkgs.haskell.lib;
    pkgs.lib.composeExtensions
    (self: _super: { th-desugar = self.th-desugar_1_14; }) extraOverrides;
  inherit modifier;
}
