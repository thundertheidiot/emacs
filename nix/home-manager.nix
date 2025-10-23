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
      # screenshot
      grim
      slurp
    ];

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

    programs.emacs.enable = true;
    programs.man.generateCaches = true;
  };
}
