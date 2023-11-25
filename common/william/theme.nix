{ config, pkgs, inputs, lib, ... }:
with lib;
with config.lib.stylix.colors.withHashtag;
{
    stylix = {
        image = ../../resources/wallpapers/space.jpg;
        polarity = "dark";
        targets.vim.enable = false;
        opacity = {
            applications = 0.93;
            desktop = 0.75;
            popups = 0.85;
            terminal = 0.75;
        };
        fonts = {
            monospace = with pkgs; {
                name = "FiraCode Nerd Font Mono";
                package = fira-code;
            };
        };
    };
    home.pointerCursor = 
    let 
      getFrom = url: hash: name: {
          gtk.enable = true;
          x11.enable = true;
          name = name;
          size = 32;
          package = 
            pkgs.runCommand "moveUp" {} ''
              mkdir -p $out/share/icons
              ln -s ${pkgs.fetchzip {
                url = url;
                hash = hash;
              }} $out/share/icons/${name}
          '';
        };
    in
      mkForce (getFrom 
        "https://github.com/ful1e5/fuchsia-cursor/releases/download/v2.0.0/Fuchsia-Pop.tar.gz"
        "sha256-BvVE9qupMjw7JRqFUj1J0a4ys6kc9fOLBPx2bGaapTk="
        "Fuchsia-Pop");
    xdg.configFile."wofi/colors".text = ''
        ${base00}
        ${base01}
        ${base02}
        ${base03}
        ${base04}
        ${base05}
        ${base06}
        ${base07}
        ${base08}
        ${base09}
        ${base0A}
        ${base0B}
        ${base0C}
        ${base0D}
        ${base0E}
        ${base0F}
    '';
    xdg.configFile."Element/config.json".text = ''
    {
        "setting_defaults": {
            "custom_themes": [
                {
                    "name": "Stylix",
                    "is_dark": true,
                    "colors": {
                       "accent-color": "${base05}",
                "accent": "${base05}",
                "primary-color": "${base04}",
                "warning-color": "${base0A}",
                "alert": "${base09}",
                "sidebar-color": "${base01}",
                "roomlist-background-color": "${base00}",
                "roomlist-text-color": "${base05}",
                "roomlist-text-secondary-color": "${base04}",
                "roomlist-highlights-color": "${base03}",
                "roomlist-separator-color": "${base00}",
                "timeline-background-color": "${base00}",
                "timeline-text-color": "${base05}",
                "timeline-text-secondary-color": "${base04}",
                "timeline-highlights-color": "${base01}",
                "username-colors": [ "${base01}", "${base02}", "${base03}", "${base04}", "${base05}", "${base06}", "${base07}"],
                "avatar-background-colors": [ "${base08}", "${base09}", "${base0A}", "${base0B}", "${base0C}", "${base0D}", "${base0E}", "${base0F}"],
                "reaction-row-button-selected-bg-color": "${base04}",
                "menu-selected-color": "${base03}",
                "focus-bg-color": "${base03}",
                "room-highlight-color": "${base04}",
                "other-user-pill-bg-color": "${base04}",
                "icon-button-color": "${base05}",
                "togglesw-off-color": "${base04}",
                "secondary-content": "${base05}",
                "tertiary-content": "${base05}" 
                    }
                }
            ]
        }
        }
    '';
}
