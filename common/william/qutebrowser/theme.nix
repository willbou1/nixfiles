{ config, pkgs, lib, ...}:
with config.lib.stylix.colors;
let
    fontSize = config.stylix.fonts.sizes.terminal;
    opacity = builtins.ceil (config.stylix.opacity.applications * 100);
    hexOpacity =  lib.toHexString (opacity * 255 / 100);
    info = "#${hexOpacity}${base0B}";
    secondary-info = "#${hexOpacity}${base0C}";
    secondary-background = "#${hexOpacity}${base01}";
    selection-background = "#${hexOpacity}${base03}";
    background = "#${hexOpacity}${base00}";
    warning = "#${hexOpacity}${base0E}";
    error = "#${hexOpacity}${base08}";
in {
    programs.qutebrowser.settings = {
        window.transparent = true;
        colors = {
            completion = {
                odd.bg = lib.mkForce secondary-background;
                even.bg = lib.mkForce background;
                category = {
                    bg = lib.mkForce background;
                    border.top = lib.mkForce background;
                    border.bottom = lib.mkForce background;
                };
                item.selected = {
                    bg = lib.mkForce selection-background;
                    border.top = lib.mkForce selection-background;
                    border.bottom = lib.mkForce selection-background;
                };
                scrollbar = {
                    bg = lib.mkForce background;
                };
            };
            contextmenu = {
                disabled.bg = lib.mkForce secondary-background;
                menu.bg = lib.mkForce background;
                selected.bg = lib.mkForce selection-background;
            };
            downloads = {
                bar.bg = lib.mkForce background;
                start.bg = lib.mkForce info;
                stop.bg = lib.mkForce secondary-info;
                error.bg = lib.mkForce error;
            };
            messages = {
                error = {
                    bg = lib.mkForce error;
                    border = lib.mkForce error;
                };
                warning = {
                    bg = lib.mkForce warning;
                    border = lib.mkForce warning;
                };
                info = {
                    bg = lib.mkForce info;
                    border = lib.mkForce info;
                };
            };
            prompts = {
                bg = lib.mkForce background;
                border = lib.mkForce background;
                selected.bg = lib.mkForce secondary-background;
            };
            statusbar = {
                normal.bg = lib.mkForce background;
                insert.bg = lib.mkForce info;
                passthrough.bg = lib.mkForce secondary-info;
                private.bg = lib.mkForce secondary-background;
                command = {
                    bg = lib.mkForce background;
                    private.bg = lib.mkForce secondary-background;
                };
                caret = {
                    bg = lib.mkForce selection-background;
                    selection.bg = lib.mkForce selection-background;
                };
                progress.bg = lib.mkForce info;
            };
            tabs = {
                bar.bg = lib.mkForce background;
                indicator = {
                    start = lib.mkForce secondary-info;
                    stop = lib.mkForce info;
                    error = lib.mkForce error;
                };
                odd.bg = lib.mkForce background;
                even.bg = lib.mkForce secondary-background;
                pinned = {
                    even.bg = lib.mkForce info;
                    odd.bg = lib.mkForce secondary-info;
                    selected = {
                        even.bg = lib.mkForce selection-background;
                        odd.bg = lib.mkForce selection-background;
                    };
                };
                selected = {
                    even.bg = lib.mkForce selection-background;
                    odd.bg = lib.mkForce selection-background;
                };
            };
        };
        fonts = {
            default_size = "${toString fontSize}pt";
            downloads = "${toString (fontSize - 3)}pt default_family";
            hints = "bold ${toString (fontSize + 3)}pt default_family";
        };
    };
}
