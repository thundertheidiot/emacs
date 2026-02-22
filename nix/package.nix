{
  pkgs,
  inputs,
  lib,
  parse,
  extraCFlags ? "",
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

  meow-lisp = epkgs: [
    (epkgs.trivialBuild {
      pname = "meow-lisp";
      src = ../lisp;

      installPhase = ''
        mkdir -p $out/share/emacs
        cp -r $src $out/share/emacs/site-lisp
      '';

      version = "1.0";

      packageRequires = packages epkgs;
    })
  ];

  defaultInit = epkgs: [
    (epkgs.trivialBuild {
      pname = "default";
      version = "1.0";
      src = ../init.el;

      # filename must be default.el
      installPhase = ''
        mkdir -p $out/share/emacs/site-lisp
        cp $src $out/share/emacs/site-lisp/default.el
      '';

      packageRequires =
        (packages epkgs)
        ++ (meow-lisp epkgs)
        ++ [
          (epkgs.treesit-grammars.with-all-grammars)
        ];
    })
  ];

  emacsPackages' = pkgs.emacsPackagesFor (pkgs.emacs-pgtk.overrideAttrs (prev: {
    env =
      prev.env
      // {
        NIX_CFLAGS_COMPILE = "-O2 ${extraCFlags}";
      };

    configureFlags =
      prev.configureFlags
      ++ [
        "--with-native-compilation=aot"
        "--disable-gc-mark-trace"
        "--enable-link-time-optimization"
        "--with-tree-sitter"
      ];
  }));
  emacsPackages = emacsPackages'.overrideScope (import ./overrides.nix {inherit pkgs inputs;});
  emacsWithPackages = emacsPackages.emacsWithPackages;
in
  (emacsWithPackages defaultInit).overrideAttrs (prev: {
    passthru.epkgs = emacsPackages;
  })
