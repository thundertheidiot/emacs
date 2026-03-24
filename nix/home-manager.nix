{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkOption mkEnableOption mkIf;
  inherit (lib.types) bool;

  cfg = config.meowEmacs;
in {
  options.meowEmacs = {
    enable = mkEnableOption "Set up emacs.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      emacs-lsp-booster
      emacs-all-the-icons-fonts
      python314Packages.trafilatura

      # screenshot
      grim
      slurp
      wayfreeze
      wl-clipboard
    ];

    # ewm portals
    # https://codeberg.org/ezemtsov/ewm/pulls/53
    xdg.portal = {
      enable = true;
      config.ewm = {
        default = "gnome;gtk;";
        "org.freedesktop.impl.portal.Access" = "gtk";
        "org.freedesktop.impl.portal.Notification" = "gtk";
        "org.freedesktop.impl.portal.Secret" = "gnome-keyring";
      };
    };

    xdg.configFile."emacs/early-init.el" = {
      enable = true;
      source = ../early-init.el;
    };

    services.emacs = {
      enable = true;
      defaultEditor = true;
      client.enable = true;
      startWithUserSession = "graphical";
    };

    # package in flake.nix
    programs.emacs.enable = true;
    programs.man.generateCaches = true;
  };
}
