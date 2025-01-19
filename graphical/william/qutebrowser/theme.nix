{
  lib,
  config,
  ...
}:
with config.lib.stylix.colors;
with lib; let
  fontSize = config.stylix.fonts.sizes.terminal;
  opacity = builtins.ceil (config.stylix.opacity.applications * 100);
  hexOpacity = toHexString (opacity * 255 / 100);
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
        odd.bg = mkForce secondary-background;
        even.bg = mkForce background;
        category = {
          bg = mkForce background;
          border.top = mkForce background;
          border.bottom = mkForce background;
        };
        item.selected = {
          bg = mkForce selection-background;
          border.top = mkForce selection-background;
          border.bottom = mkForce selection-background;
        };
        scrollbar = {
          bg = mkForce background;
        };
      };
      contextmenu = {
        disabled.bg = mkForce secondary-background;
        menu.bg = mkForce background;
        selected.bg = mkForce selection-background;
      };
      downloads = {
        bar.bg = mkForce background;
        start.bg = mkForce info;
        stop.bg = mkForce secondary-info;
        error.bg = mkForce error;
      };
      messages = {
        error = {
          bg = mkForce error;
          border = mkForce error;
        };
        warning = {
          bg = mkForce warning;
          border = mkForce warning;
        };
        info = {
          bg = mkForce info;
          border = mkForce info;
        };
      };
      prompts = {
        bg = mkForce background;
        border = mkForce background;
        selected.bg = mkForce secondary-background;
      };
      statusbar = {
        normal.bg = mkForce background;
        insert.bg = mkForce info;
        passthrough.bg = mkForce secondary-info;
        private.bg = mkForce secondary-background;
        command = {
          bg = mkForce background;
          private.bg = mkForce secondary-background;
        };
        caret = {
          bg = mkForce selection-background;
          selection.bg = mkForce selection-background;
        };
        progress.bg = mkForce info;
      };
      tabs = {
        bar.bg = mkForce background;
        indicator = {
          start = mkForce secondary-info;
          stop = mkForce info;
          error = mkForce error;
        };
        odd.bg = mkForce background;
        even.bg = mkForce secondary-background;
        pinned = {
          even.bg = mkForce info;
          odd.bg = mkForce secondary-info;
          selected = {
            even.bg = mkForce selection-background;
            odd.bg = mkForce selection-background;
          };
        };
        selected = {
          even.bg = mkForce selection-background;
          odd.bg = mkForce selection-background;
        };
      };
    };
    fonts = {
      default_size = mkForce "${toString fontSize}pt";
      web.size.default = mkForce fontSize;
      downloads = "${toString (fontSize - 3)}pt default_family";
      hints = "bold ${toString (fontSize + 3)}pt default_family";
    };
  };
}
