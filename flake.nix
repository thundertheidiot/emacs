{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";

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
          inherit pkgs lib;
          parse = import "${inputs.emacs-overlay}/parse.nix" {inherit pkgs lib;};
        };
      };
    };
}
