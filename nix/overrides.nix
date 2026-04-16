{
  pkgs,
  inputs,
  ...
}: final: prev: {
  diff-hl = prev.diff-hl.overrideAttrs (_: {
    src = pkgs.fetchFromGitHub {
      owner = "dgutov";
      repo = "diff-hl";
      rev = "39f076efa85110c4bcc9b73994f30a7d52312c98";
      hash = "sha256-XoiAj0AbOty1omfIptBnjU/uJyuWzGqGhILVrMEJgbk=";
    };

    packageRequires = [
      final.package-build
    ];
  });

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

  ghostel = final.trivialBuild {
    pname = "ghostel";
    version = "git";

    packageRequires = with final; [
      evil
      s
    ];

    nativeBuildInputs = [
      pkgs.zig
      pkgs.zig.hook
    ];

    # https://github.com/dakra/ghostel/blob/main/build.zig.zon
    postPatch = let
      ghosttyDep = pkgs.fetchzip {
        url = "https://github.com/ghostty-org/ghostty/archive/01825411ab2720e47e6902e9464e805bc6a062a1.tar.gz";
        sha256 = "sha256-zDOIAbNdKPfNemiz0aJDjOIWamCpb3FsYxnOr9f2ke0=";
      };

      ghosttyZigDeps = pkgs.callPackage "${ghosttyDep}/build.zig.zon.nix" {};
    in ''
      mkdir -p $TMPDIR/zig-cache/p
      ln -s ${ghosttyDep} "$TMPDIR/zig-cache/p/ghostty-1.3.2-dev-5UdBCzaaBwVjJOr-ltYINjybeEOAmLAauH5oq8-cdNGN"
      for dir in ${ghosttyZigDeps}/*; do
        ln -s $dir $TMPDIR/zig-cache/p/$(basename $dir)
      done
    '';

    postInstall = ''
      ZIG_GLOBAL_CACHE_DIR="$TMPDIR/zig-cache" \
      ZIG_LOCAL_CACHE_DIR="$TMPDIR/zig-cache" zig build \
        -Doptimize=ReleaseFast \
        --color off \
        --prefix $TMPDIR/zig-out
      cp $TMPDIR/zig-out/lib/libghostel-module.so $out/share/emacs/site-lisp/ghostel-module.so
    '';

    src = inputs.ghostel;
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
