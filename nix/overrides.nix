{
  pkgs,
  inputs,
  ...
}: final: prev: {
  ewm =
    (import "${inputs.ewm}/nix/default.nix" {
      inherit pkgs;
      withScreencastSupport = true;
    })
    // {
      propagatedNativeBuildInputs = [
        pkgs.wayfreeze
        pkgs.grim
        pkgs.slurp
        pkgs.wl-clipboard-rs
      ];
    };

  eglot-booster = final.trivialBuild {
    pname = "eglot-booster";
    version = "1.0.0";

    src = inputs.eglot-booster;

    propagatedNativeBuildInputs = [
      pkgs.emacs-lsp-booster
    ];
  };

  emsg-blame = final.trivialBuild {
    pname = "emsg-blame";
    version = "1.0.0";

    packageRequires = [
      final.async
    ];

    src = inputs.emsg-blame;
  };

  ghostel = final.callPackage ./ghostel.nix {
    src = inputs.ghostel;
    version = "0.45.0";
    zigHash = "sha256-yrVgiofdmVjTGJ+PGPGRCc8gb/JcEca1uAzIoPgHHqU=";
  };

  lsp-mode = prev.lsp-mode.overrideAttrs (prev: {
    buildPhase =
      ''
        export LSP_USE_PLISTS=true
      ''
      + prev.buildPhase;
  });

  # weird problem
  # https://github.com/NixOS/nixpkgs/issues/388829
  alert = prev.alert.overrideAttrs {
    __structuredAttrs = false;
  };

  empv = prev.empv.overrideAttrs {
    packageRequires = with final; [
      hydra
      s
    ];
  };

  rustic = prev.rustic.overrideAttrs {
    packageRequires = with final; [
      flycheck
      dash
      markdown-mode
      s
      xterm-color
      f
      rust-mode
      spinner
    ];
  };
}
