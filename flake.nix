{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";

  inputs.home-manager.url = "github:nix-community/home-manager";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";

  # not sure if aly's fork does much, but it was apparently important for her
  # https://github.com/nialov/actions.nix/compare/master...alyraffauf:actions.nix:master
  inputs.actions.url = "github:alyraffauf/actions.nix";

  nixConfig = {
    substituters = [
      "https://cache.nixos.org"
      "https://meowos.cachix.org"
    ];
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "meowos.cachix.org-1:QOXuuFPMN5TszgX8+nqd8X+BZG84toh5wK8j1IBBDH4="
    ];
  };

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

      imports = [
        inputs.home-manager.flakeModules.home-manager
        inputs.actions.flakeModules.default
        ./nix/actions.nix
      ];

      flake.homeModules = {
        default = {pkgs, ...}: {
          programs.emacs.package = inputs.self.packages.${pkgs.system}.default;

          imports = [./nix/home-manager.nix];
        };
      };

      perSystem = {
        config,
        pkgs,
        lib,
        system,
        ...
      }: let
        emacsArgs = {
          inherit pkgs lib inputs;
          parse = import "${inputs.emacs-overlay}/parse.nix" {inherit pkgs lib;};
        };
      in {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.emacs-overlay.overlays.default
          ];
        };

        packages.default = config.packages.emacs;
        packages.emacs = import ./nix/package.nix emacsArgs;

        packages.emacsZen4 = import ./nix/package.nix (emacsArgs
          // {
            extraCFlags = "-march=znver4 -mtune=znver4";
          });

        packages.emacs-empty-init-test = pkgs.writeShellScriptBin "emacs-empty-init-test" ''
          export EMACS_USER_DIR=$(mktemp -d)

          ln -sf ${./early-init.el} $EMACS_USER_DIR/early-init.el
          exec ${config.packages.emacs}/bin/emacs --init-directory=$EMACS_USER_DIR "$@"
        '';
      };
    };
}
