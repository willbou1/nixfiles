{
  services.kanshi = {
    enable = true;
    systemdTarget = "hyprland-session.target";
    settings = [
      {
        profile = {
          name = "undocked";
          outputs = [
            {
              criteria = "eDP-1";
              mode = "3840x2400@60Hz";
              position = "0,0";
              scale = 2.0;
            }
          ];
        };
      }
      {
        profile = {
          name = "docked";
          outputs = [
            {
              criteria = "eDP-1";
              mode = "3840x2400@60Hz";
              position = "0,0";
              scale = 2.0;
            }
            {
              criteria = "DP-4";
              mode = "3440x1440@100Hz";
              position = "1920,0";
              scale = 1.2;
            }
          ];
        };
      }
    ];
  };
}
