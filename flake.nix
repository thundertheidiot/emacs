{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";

  inputs = {
    eglot-booster = {
      url = "github:jdtsmith/eglot-booster";
      flake = false;
    };

    emsg-blame = {
      url = "github:ISouthRain/emsg-blame";
      flake = false;
    };

    diff-hl = {
      url = "github:dgutov/diff-hl?rev=39f076efa85110c4bcc9b73994f30a7d52312c98";
      flake = false;
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];

      perSystem = {
        config,
        pkgs,
        lib,
        system,
        ...
      }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.emacs-overlay.overlays.default
          ];
        };

        packages.default = config.packages.emacs;
        packages.emacs = import ./nix/package.nix {
          inherit pkgs lib inputs;
          parse = import "${inputs.emacs-overlay}/parse.nix" {inherit pkgs lib;};
        };
      };
    };
}
