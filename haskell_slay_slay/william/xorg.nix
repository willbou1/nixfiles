{ pkgs, config, lib, ... }:
with lib;
let
    colors = config.services.polybar.settings.colors;
in {
    xsession.windowManager.herbstluftwm.settings.window_gap = mkForce 10;
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
