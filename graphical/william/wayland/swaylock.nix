{config, pkgs, ...}: {
  services.swayidle = {
    enable = true;
    systemdTarget = "hyprland-session.target";
    events = [
      {
        event = "before-sleep";
        command = "${pkgs.playerctl}/bin/playerctl pause";
      }
    ];
  };

  stylix.targets.hyprlock.enable = false;
  programs.hyprlock =
    with config.stylix;
    with config.lib.stylix;
    let
      getColorCh = colorName: channel: colors."${colorName}-rgb-${channel}";
      rgb = color: ''rgb(${getColorCh color "r"}, ${getColorCh color "g"}, ${getColorCh color "b"})'';
      rgba = color: alpha: ''rgba(${getColorCh color "r"}, ${getColorCh color "g"}, ${getColorCh color "b"}, ${toString alpha})'';
      size = fonts.sizes.desktop * 2;
    in
      {
        enable = true;
        settings = {
          general = {
            hide_cursor = true;
          };

          background = {
            monitor = "";
            path = "screenshot";
            blur_passes = 2;
            brightness = 0.5;
          };

          label = [
            {
              monitor = "";
              text = "cmd[update:1000] echo -e \"$(${pkgs.coreutils}/bin/date +\"%I:%M:%S %p\")\"";
              color = rgb "base05";
              font_size = size * 2;
              font_family = fonts.sansSerif.name;
              halign = "center";
              valign = "top";
              position = "0, -15%";
            }
            {
              monitor = "";
              text = "cmd[update:43200000] echo \"$(${pkgs.coreutils}/bin/date +\"%A, %d %B %Y\")\"";
              color = rgb "base05";
              font_size = size;
              font_family = fonts.sansSerif.name;
              halign = "center";
              valign = "top";
              position = "0, -23%";
            }
            {
              monitor = "";
              text = "Let there be math!";
              color = rgb "base0E";
              font_size = size;
              font_family = fonts.sansSerif.name;
              halign = "center";
              valign = "bottom";
              position = "0, 15%";
            }
          ];

          input-field = {
            monitor = "";
            halign = "center";
            valign = "center";
            hide_input = false;
            fade_on_empty = false;
            outer_color = rgba "base03" opacity.desktop;
            inner_color = rgba "base00" opacity.desktop;
            font_color = rgb "base05";
            fail_color = rgba "base08" opacity.desktop;
            check_color = rgba "base0A" opacity.desktop;
          };

          auth = {
            fingerprint = {
              enabled = true;
            };
          };
        };
      };
}
