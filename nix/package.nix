{
  pkgs,
  lib,
  parse,
  ...
}: let
  inherit (builtins) readDir;
  inherit (lib) mapAttrsToList;
  inherit (lib.lists) flatten filter;
  inherit (lib.strings) concatStringsSep readFile hasSuffix;

  files = let
    match = {
      "regular" = file: root: "${root}/${file}";
      "directory" = dir: root: f "${root}/${dir}";
      "symlink" = _: _: throw "unexpected symlink";
      "unknown" = _: _: throw "unexpected unknown";
    };

    f = dir: mapAttrsToList (file: type: match."${type}" file dir) (readDir dir);
  in
    filter (hasSuffix ".el") (flatten (f ../.));

  text = map readFile files;

  packageList = parse.parsePackagesFromUsePackage {
    configText = concatStringsSep "\n" text;
    alwaysEnsure = true;
  };

  getPackage = set: name: set."${name}" or (throw "Emacs package ${name} not found.");
  packages = set: map (getPackage set) packageList;

  emacsPackages = pkgs.emacsPackagesFor pkgs.emacs-igc-pgtk;
  emacsWithPackages = emacsPackages.emacsWithPackages;
in
  emacsWithPackages packages
