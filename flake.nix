{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable-small";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";

  inputs.home-manager.url = "github:nix-community/home-manager";
  inputs.home-manager.inputs.nixpkgs.follows = "nixpkgs";

  # inputs.ewm.url = "git+https://codeberg.org/thundertheidiot/ewm.git?ref=dev";
  inputs.ewm.url = "git+https://codeberg.org/ezemtsov/ewm.git?ref=crash-robustness";

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

    ghostel = {
      url = "github:dakra/ghostel";
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
        default = {
          pkgs,
          lib,
          config,
          ...
        }: let
          inherit (lib) mkDefault mkOption;
          inherit (lib.types) str;
        in {
          imports = [./nix/home-manager.nix];

          options = {
            meowEmacs.package = mkOption {
              type = str;
              default = "default";
            };
          };

          config = {
            programs.emacs.package = mkDefault inputs.self.packages.${pkgs.system}.${config.meowEmacs.package};
          };
        };
      };

      flake.nixosModules = {
        default = {
          pkgs,
          lib,
          config,
          ...
        }: let
          inherit (lib) mkDefault mkOption;
          inherit (lib.types) str;
        in {
          imports = [inputs.ewm.nixosModules.default];

          options = {
            meowEmacs.package = mkOption {
              type = str;
              default = "default";
            };
          };

          config = {
            environment.systemPackages = [pkgs.xwayland-satellite];

            programs.ewm.emacsPackage = mkDefault inputs.self.packages.${pkgs.system}.${config.meowEmacs.package};
          };
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
        packages.emacs = import ./nix/package.nix (emacsArgs
          // {
            extraCFlags = [
              "-fno-omit-frame-pointer"
              "-fno-plt"
            ];
            elispCFlags = [
              "-g0"
              "-fno-omit-frame-pointer"
              "-fno-finite-math-only"
            ];
          });

        packages.emacsDebug = import ./nix/package.nix (emacsArgs
          // {
            extraCFlags = [
              "-g3"
              "-fno-omit-frame-pointer"
              "-fno-inline"
              "-ggdb"
            ];
            elispCFlags = [
              "-g3"
              "-fno-omit-frame-pointer"
            ];

            extraConfigureFlags = [
              # "--enable-checking='yes,glyphs'"
              "--enable-check-list-object-type"
            ];

            optLevel = "0";
            elispOptLevel = "0";

            dontStrip = true;
          });

        # https://www.jamescherti.com/compiling-emacs/
        packages.emacsOpt = import ./nix/package.nix (emacsArgs
          // {
            extraCFlags = [
              "-march=znver4"
              "-mtune=znver4"
              "-fno-omit-frame-pointer"
              "-fno-plt"
              "-flto=auto"
            ];
            elispCFlags = [
              "-march=znver4"
              "-mtune=znver4"
              "-g0"
              "-fno-omit-frame-pointer"
              "-fno-finite-math-only"
            ];
            optLevel = "2";
            elispOptLevel = "2";
          });

        packages.emacsIgcOpt = import ./nix/package.nix (emacsArgs
          // {
            package = pkgs.emacs-igc-pgtk;
            extraConfigureFlags = ["--with-mps=yes"];
            extraCFlags = [
              "-march=znver4"
              "-mtune=znver4"
              "-fno-omit-frame-pointer"
              "-fno-finite-math-only"
              "-fno-plt"
              "-flto=auto"
            ];
            elispCFlags = [
              "-march=znver4"
              "-mtune=znver4"
            ];
            optLevel = "2";
            elispOptLevel = "2";
          });

        packages.emacs-empty-init-test = pkgs.writeShellScriptBin "emacs-empty-init-test" ''
          export EMACS_USER_DIR=$(mktemp -d)

          ln -sf ${./early-init.el} $EMACS_USER_DIR/early-init.el
          exec ${config.packages.emacs}/bin/emacs --init-directory=$EMACS_USER_DIR "$@"
        '';
      };
    };
}
