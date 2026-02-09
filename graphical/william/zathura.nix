{
  lib,
  config,
  ...
}:
{
  xdg.mimeApps.defaultApplications."application/pdf" = ["zathura.desktop"];
  programs.zathura = {
    enable = true;
    options = with config.stylix; 
              with config.lib.stylix.colors;
      {
        font = "${fonts.monospace.name} ${toString fonts.sizes.applications}";
        recolor = lib.mkForce true;
        recolor-keephue = lib.mkForce false;
        recolor-reverse-video = lib.mkForce true;
        recolor-lightcolor = lib.mkForce "rgba(0,0,0,0)";
        recolor-darkcolor = lib.mkForce withHashtag.base07;
        default-bg = lib.mkForce "rgba(${toString base00-rgb-r}, ${toString base00-rgb-g}, ${toString base00-rgb-b}, ${toString opacity.terminal})";
      };
  };
}
