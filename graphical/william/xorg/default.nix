{
  pkgs,
  lib,
  config,
  ...
}:
with lib;
with builtins; let
  xrandrCommands =
    concatStrings
    (
      map
      (m: ''        xrandr \
                  --output ${m.xrandrName} \
                  --mode ${toString m.width}x${toString m.height} \
                  --rate ${toString m.rate} \
                  --scale ${toString (1.0 / m.hScale)}x${toString (1.0 / m.vScale)} \
                  --pos ${toString m.x}x${toString m.y}
      '')
      config.home.monitors
    );
in {
  imports = mine.autoInclude ./. [
    ./rofi
  ];

  home.file.".xinitrc".source = pkgs.writeShellScript ".xinitrc" ''
    ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY HYPRLAND_INSTANCE_SIGNATURE XDG_CURRENT_DESKTOP XAUTHORITY XDG_SESSION_ID
    ${xrandrCommands}
    xinput set-prop 'VEN_04F3:00 04F3:32AA Touchpad' "libinput Natural Scrolling Enabled" 1
    xset s on
    xset s 1200
    xset s blank
    if [[ $1 == "herbstluftwm" ]]; then
        exec herbstluftwm
    elif [[ $1 == "dwm" ]]; then
        exec dwm
    else
        echo "Choose a window manager"
    fi
  '';
}
