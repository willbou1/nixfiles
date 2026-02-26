{
  pkgs,
  config,
  ...
}: let
  swaylock = config.programs.swaylock.package;
  lock = pkgs.writeShellScript "lock" ''
    if ! ${pkgs.procps}/bin/pgrep '.*swaylock.*'; then
        ${swaylock}/bin/swaylock -f "$@"
        disown -a
    fi
  '';
in {
  services.swayidle = {
    timeouts = [
      {
        timeout = 600;
        command = "${lock} --grace 0";
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
        command = "${lock} --grace 0";
      }
    ];
  };
}
