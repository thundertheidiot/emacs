{
  pkgs,
  inputs,
  lib,
  parse,
  extraCFlags ? [],
  elispCFlags ? [],
  extraConfigureFlags ? [],
  optLevel ? "2",
  elispOptLevel ? "2",
  dontStrip ? false,
  package ? pkgs.emacs-pgtk,
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

      packageRequires =
        (packages epkgs)
        ++ [
          epkgs.ewm
          epkgs.eglot-booster
          epkgs.emsg-blame
        ];
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

  emacsPackages' = pkgs.emacsPackagesFor (package.overrideAttrs (prev: {
    inherit dontStrip;

    env =
      prev.env
      // {
        NIX_CFLAGS_COMPILE = "-O${optLevel} ${concatStringsSep " " extraCFlags}";
      };

    # i kept running into this weird crash
    # probably a pgtk + native comp edgecase no one else has found
    # doing this manually in a gdb instance hooked up to emacs seemed to fix it so 🤷‍♀️
    patches = prev.patches ++ [./stupid.patch];

    postPatch =
      (prev.postPatch or "")
      + (let
        quote = map (s: ''\"${s}\"'');
        flags = concatStringsSep " " (quote elispCFlags);
      in ''
        substituteInPlace lisp/emacs-lisp/comp.el \
          --replace-warn "(defcustom native-comp-compiler-options nil" \
                         "(defcustom native-comp-compiler-options '(${flags})" \
          --replace-warn "(defcustom native-comp-speed 2" \
                         "(defcustom native-comp-speed ${elispOptLevel}"

        grep native-comp-compiler-options lisp/emacs-lisp/comp.el
      '');

    configureFlags =
      prev.configureFlags
      ++ [
        "--with-native-compilation=aot"
        "--disable-gc-mark-trace"
        "--enable-link-time-optimization"
        "--with-tree-sitter"
      ]
      ++ extraConfigureFlags;
  }));
  emacsPackages = emacsPackages'.overrideScope (import ./overrides.nix {inherit pkgs inputs;});
  emacsWithPackages = emacsPackages.emacsWithPackages;
in
  (emacsWithPackages defaultInit).overrideAttrs (prev: {
    passthru.epkgs = emacsPackages;
    inherit dontStrip;
  })
