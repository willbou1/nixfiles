{
  config,
  pkgs,
  lib,
  wallpaper,
  ...
}:
with lib;
with config.lib.stylix.colors.withHashtag; {
  stylix = {
    enable = true;
    polarity = "dark";
    targets.vim.enable = false;
    image = wallpaper;
    opacity = {
      applications = 0.84;
      desktop = 0.69;
      popups = 0.80;
      terminal = 0.69;
    };
    fonts = with pkgs; {
      serif = {
        name = "DejaVu Serif";
        package = pkgs.dejavu_fonts;
      };
      sansSerif = {
        name = "Open Sans";
        package = open-sans;
      };
      monospace = {
        name = "JetBrains Mono";
        package = jetbrains-mono;
      };
    };
  };

  # Fonts
  fonts.fontconfig.enable = true;
  home.packages = with pkgs;
    [
      baekmuk-ttf
    ]
    ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

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
}
