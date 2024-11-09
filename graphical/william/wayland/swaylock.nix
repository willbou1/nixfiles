{
  config,
  pkgs,
  inputs,
  ...
}: let
  swaylock = "${config.programs.swaylock.package}/bin/swaylock";
in {
  services.swayidle = {
    enable = true;
    systemdTarget = "hyprland-session.target";
    events = [
      {
        event = "before-sleep";
        command = "${pkgs.playerctl}/bin/playerctl pause";
      }
    ];
  };

  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings = {
      daemonize = false;
      screenshots = true;
      clock = true;
      indicator = true;
      grace = 5;
      fade-in = 1;
      effect-blur = "7x5";
      effect-vignette = "0.45:0";
      indicator-thickness = 7;
      indicator-radius = 130;
    };
  };
}
