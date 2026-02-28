{
  pkgs,
  config,
  ...
}: let
  swaylock = config.programs.swaylock.package;
  lock = pkgs.writeShellScript "lock" ''
    if ! ${pkgs.procps}/bin/pgrep -x hyprlock; then
        ${pkgs.hyprlock}/bin/hyprlock "$@" &
    fi
  '';
in {
  services.swayidle = {
    timeouts = [
      {
        timeout = 600;
        command = "${lock}";
      }
      {
        timeout = 700;
        command = "${pkgs.hyprland}/bin/hyprctl dispatch dpms off";
        resumeCommand = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
      }
    ];
    events = [
      {
        event = "before-sleep";
        command = "${lock}";
      }
    ];
  };
}
