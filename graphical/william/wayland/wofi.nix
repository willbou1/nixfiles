{config, ...}: {
  # mine is better
  stylix.targets.wofi.enable = false;
  xdg.configFile."wofi/colors".text = with config.lib.stylix.colors.withHashtag; ''
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

  programs.wofi = {
    enable = true;
    settings = {
      width = "35%";
      height = "45%";
      allow_images = true;
      allow_markup = true;
      single_click = true;
      key_expand = "Tab";
      colors = "colors";
    };
    style = ''
      * {
              font-family: "Hack", monospace;
              font-size: 21px;
      }

      window {
              background-color: transparent;
      }

      #img {
              margin-left: 5px;
              margin-right: 10px;
      }

      #input {
              margin-top: 5px;
              margin-bottom: 12px;
              margin-left: 20px;
              margin-right: 20px;
              border-radius: 0px;
              border: none;
              background-color: transparent;
              color: --wofi-color5;
      }

      #inner-box {
              background-color: transparent;
      }

      #outer-box {
              padding: 15px;
              border-radius: 20px;
              border-style: solid;
              border-width: ${toString config.home.borderSize}px;
              border-color: --wofi-color10;
              background-color: rgba(--wofi-rgb-color0, 0.75);
      }

      #scroll {
              margin: 5px;
      }

      #text {
              padding: 4px;
              color: --wofi-color5;
      }

      #entry {
              padding: 7px;
      }

      #entry:selected {
              background-color: rgba(--wofi-rgb-color2, 0.5);
              border: 1px solid --wofi-color5;
              border-radius: 20px;
      }

      #text:selected {
              background: transparent;
              color: --wofi-color5;
      }

      list {
              background: transparent;
              margin-left: 35px;
      }

      list label {
              font-size: 19px;
      }
    '';
  };
}
