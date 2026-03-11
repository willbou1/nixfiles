{
  pkgs,
  config,
  ...
}: let
  systemctl = config.systemd.user.systemctlPath;
  brillo = "${pkgs.brillo}/bin/brillo";
  brilloDim = pkgs.writeShellScript "brilloDim" ''
    echo 0 > /sys/devices/platform/dell-laptop/leds/dell::kbd_backlight/brightness
    ${brillo} -O
    ${brillo} -u 150000 -S 0
  '';
  brilloRestore = pkgs.writeShellScript "brilloRestore" ''
    echo 1 > /sys/devices/platform/dell-laptop/leds/dell::kbd_backlight/brightness
    ${brillo} -u 150000 -I
  '';
  lock = pkgs.writeShellScript "lock" ''
    if ! ${pkgs.procps}/bin/pgrep -x hyprlock; then
        ${pkgs.hyprlock}/bin/hyprlock "$@" &
        disown
    fi
  '';
in {
  wayland.windowManager.hyprland.settings.bind = [
    "$mod,Q,exec,${lock}"
  ];
  services.swayidle = {
    events = [
      {
        event = "after-resume";
        command = "${lock}";
      }
    ];
    timeouts = [
      {
        timeout = 300;
        command = "${brilloDim}";
        resumeCommand = "${brilloRestore}";
      }
      {
        timeout = 600;
        command = "${lock}";
      }
      {
        timeout = 700;
        command = "${pkgs.hyprland}/bin/hyprctl dispatch dpms off";
        resumeCommand = "${pkgs.hyprland}/bin/hyprctl dispatch dpms on";
      }
      {
        timeout = 800;
        command = "${systemctl} suspend";
      }
    ];
  };
}
