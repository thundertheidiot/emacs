{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkIf;

  cfg = config.meowEmacs;
in {
  options.meowEmacs = {
    enable = mkEnableOption "Set up emacs.";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      emacs-lsp-booster
      python314Packages.trafilatura

      # screenshot
      grim
      slurp
      wayfreeze
      wl-clipboard

      brightnessctl
    ];

    # ewm portals
    # https://codeberg.org/ezemtsov/ewm/pulls/53
    xdg.portal = {
      enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
        xdg-desktop-portal-gnome
      ];
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

    # i have super+e as emacsclient -c -a '', which starts a new daemon
    # if the service crashes and i start a new one fast enough, this causes an issue where it starts over and over again in a loop
    systemd.user.services.emacs.Service.Restart = lib.mkForce "no";

    # package in flake.nix
    programs.emacs.enable = true;
    programs.man.generateCaches = true;
  };
}
