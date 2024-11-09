{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:
with lib;
with config.lib.stylix.colors.withHashtag; {
  stylix = {
    enable = true;
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

  # Fonts
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    baekmuk-ttf
    nerdfonts
  ];

  home.pointerCursor = let
    getFrom = url: hash: name: {
      gtk.enable = true;
      x11.enable = true;
      name = name;
      size = 32;
      package = pkgs.runCommand "moveUp" {} ''
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
}
