{
  lib,
  config,
  ...
}:
with builtins;
with lib; {
  xdg.mimeApps.defaultApplications."application/pdf" = ["zathura.desktop"];
  stylix.targets.zathura.enable = false;
  programs.zathura = {
    enable = true;
    options = with config.stylix; 
              with config.lib.stylix;
      let
        highlightTransparency = 0.5;
        getColorCh = colorName: channel: colors."${colorName}-rgb-${channel}";
        rgb =
          color:
          ''rgb(${getColorCh color "r"}, ${getColorCh color "g"}, ${getColorCh color "b"})'';
        rgba =
          color: alpha:
          ''rgba(${getColorCh color "r"}, ${getColorCh color "g"}, ${getColorCh color "b"}, ${toString alpha})'';
        size = ceil (fonts.sizes.applications * 1.2); 
      in {
        font = "${fonts.monospace.name} ${toString size}";
        recolor = true;
        recolor-keephue = false;
        recolor-reverse-video = true;
        zoom = true;

        highlight-fg = rgb "base09";
        index-fg = rgb "base07";

        default-bg = rgba "base00" opacity.terminal;
        default-fg = rgb "base01" ;
        statusbar-fg = rgb "base04";
        statusbar-bg = rgba "base02" opacity.terminal;
        inputbar-bg = rgba "base00" opacity.terminal;
        inputbar-fg = rgb "base07";
        notification-bg = rgba "base00" opacity.terminal;
        notification-fg = rgb "base07";
        notification-error-bg = rgba "base00" opacity.terminal;
        notification-error-fg = rgb "base08";
        notification-warning-bg = rgba "base00" opacity.terminal;
        notification-warning-fg = rgb "base08";
        highlight-color = rgba "base0A" highlightTransparency;
        highlight-active-color = rgba "base0D" highlightTransparency;
        completion-bg = rgba "base01" opacity.terminal;
        completion-fg = rgb "base0D";
        completion-highlight-fg = rgb "base07";
        completion-highlight-bg = rgb "base0D";
        recolor-lightcolor = rgba "base00" 0.30;
        recolor-darkcolor = rgb "base06";
      };
  };
}
