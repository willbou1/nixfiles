{
  lib,
  config,
  ...
}:
with builtins;
with lib; {
  xdg.mimeApps.defaultApplications."application/pdf" = ["zathura.desktop"];
  programs.zathura = {
    enable = true;
    options = with config.stylix; 
              with config.lib.stylix.colors;
      let size = ceil (fonts.sizes.applications * 1.2); in
      {
        font = "${fonts.monospace.name} ${toString size}";
        recolor = mkForce true;
        recolor-keephue = mkForce false;
        recolor-reverse-video = mkForce true;
        recolor-lightcolor = mkForce "rgba(0,0,0,0)";
        recolor-darkcolor = mkForce withHashtag.base07;
        default-bg = mkForce "rgba(${toString base00-rgb-r}, ${toString base00-rgb-g}, ${toString base00-rgb-b}, ${toString opacity.terminal})";
        highlight-fg = mkForce withHashtag.base00;
        index-fg = mkForce withHashtag.base07;
        zoom = mkForce true;
      };
  };
}
