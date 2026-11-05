{
  pkgs,
  inputs,
  ...
}:
let
  # pull ghostel's zig 0.17.0 from nixpkgs multiverse
  zig_0_17_0 =
    inputs.multiverse.multiverse.${pkgs.stdenv.hostPlatform.system}.fast.version "zig_0_17"
      "0.17.0";
in
final: prev: {
  claudepet = final.trivialBuild {
    pname = "claudepet";
    version = "1.0.0";

    src = pkgs.lib.fileset.toSource {
      root = ../claudepet;
      fileset = pkgs.lib.fileset.unions [
        ../claudepet/claudepet.el
        ../claudepet/claudepet-animations.el
        ../claudepet/claudepet-integrations.el
      ];
    };

    packageRequires = [ final.posframe ];
  };

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

  gptel-zai = final.trivialBuild {
    pname = "gptel-zai";
    version = "1.0.0";

    packageRequires = [
      final.gptel
    ];

    src = inputs.gptel-zai;
  };

  org-defblock = final.trivialBuild {
    pname = "org-defblock";
    version = "1.0.0";

    packageRequires = [
      final.dash
      final.lf
      final.org
      final.s
      final.seq
    ];

    src = inputs.org-defblock;
  };

  # needs the delta binary; propagatedUserEnvPkgs is what the emacs
  # wrapper actually links into bin
  magit-delta = prev.magit-delta.overrideAttrs (old: {
    propagatedUserEnvPkgs = [ pkgs.delta ];
  });

  # ghostel/evil-ghostel from git
  ghostel = prev.ghostel.overrideAttrs (old: rec {
    version = "0.60.0";
    src = inputs.ghostel;
    zig = zig_0_17_0;
    # fetchDeps only downloads the zon dependencies (version-independent)
    # use nixpkgs' zig for it, the multiverse fake has no passthru.
    zigDeps = pkgs.zig_0_17.fetchDeps {
      inherit (old) pname;
      inherit version src;
      fetchAll = true;
      hash = "sha256-Kt3r1B/5kgjMO89gih/87oEtuWmby9NB6oQp5t0vUYw=";
    };
  });

  evil-ghostel = prev.evil-ghostel.overrideAttrs (old: {
    version = "0.60.0";
    src = inputs.ghostel;
  });
  lsp-mode = prev.lsp-mode.overrideAttrs (prev: {
    buildPhase = ''
      export LSP_USE_PLISTS=true
    ''
    + prev.buildPhase;
  });

  # weird problem
  # https://github.com/NixOS/nixpkgs/issues/388829
  alert = prev.alert.overrideAttrs {
    __structuredAttrs = false;
  };

  lispy = prev.lispy.overrideAttrs {
    packageRequires = with final; [
      avy
      hydra
      swiper
      indium
      zoutline
      iedit
    ];
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
