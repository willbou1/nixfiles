{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  colors = config.services.polybar.settings.colors;
  xrotate = pkgs.writeShellScriptBin "xrotate" ''
    if (xrandr | grep 'left (normal'); then
        xinput set-prop 'VEN_04F3:00 04F3:32AA Touchpad' 'Coordinate Transformation Matrix' 1 0 0 0 1 0 0 0 1
        xinput set-prop 'ELAN2097:00 04F3:2A15' 'Coordinate Transformation Matrix' 1 0 0 0 1 0 0 0 1
        xrandr -o normal
        feh --bg-fill ${config.stylix.image}
    else
        xinput set-prop 'VEN_04F3:00 04F3:32AA Touchpad' 'Coordinate Transformation Matrix' 0 -1 1 1 0 0 0 0 1
        xinput set-prop 'ELAN2097:00 04F3:2A15' 'Coordinate Transformation Matrix' 0 -1 1 1 0 0 0 0 1
        xrandr -o left
        feh --bg-fill ${config.stylix.image}
    fi
  '';
in {
  home.packages = [xrotate];
  xsession.windowManager.herbstluftwm = {
    settings.window_gap = mkForce 10;
    keybinds = {
      "XF86MonBrightnessDown" = "spawn ${pkgs.brillo}/bin/brillo -u 150000 -U 5";
      "XF86MonBrightnessUp" = "spawn ${pkgs.brillo}/bin/brillo -u 150000 -A 5";
      "Mod4-m" = "spawn ${xrotate}/bin/xrotate";
    };
  };
  services.polybar.settings = {
    "bar/example" = {
      width = mkForce "94%";
      offset-x = mkForce "3%";
      modules-right = mkForce "pulseaudio bluetooth memory cpu vpn wlan backlight date-seoul date";
    };
    "module/eth".label-connected = mkForce "%{F${colors.primary}%ifname%";
    "module/wlan".label-connected = mkForce "%{F${colors.primary}}%ifname%%{F${colors.secondary}} %essid%";
    "module/backlight" = {
      exec = mkForce "${pkgs.brillo}/bin/brillo -G | awk '{printf(\"%d%\",$1)}'";
      click-up = mkForce "${pkgs.brillo}/bin/brillo -u 150000 -A 5";
      click-down = mkForce "${pkgs.brillo}/bin/brillo -u 150000 -U 5";
    };
  };
}
