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

  ewm = final.trivialBuild {
    pname = "ewm";
    version = "1.0";

    src = "${inputs.ewm}/lisp";
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
