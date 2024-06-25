{ lib, config, ... }:

with config.lib.stylix.colors; {
    xdg.mimeApps.defaultApplications."application/pdf" =
        [ "zathura.desktop" ];
    programs.zathura = {
        enable = true;
        options = {
            font = "${config.stylix.fonts.monospace.name} ${toString config.stylix.fonts.sizes.applications}";
            recolor = lib.mkForce true;
            recolor-keephue = lib.mkForce false;
            recolor-reverse-video = lib.mkForce true;
            recolor-lightcolor = lib.mkForce "rgba(0,0,0,0)";
            recolor-darkcolor = lib.mkForce withHashtag.base07;
            default-bg = lib.mkForce "rgba(${toString base00-rgb-r}, ${toString base00-rgb-g}, ${toString base00-rgb-b}, ${toString config.stylix.opacity.terminal})";
        };
    };
}
