{
  services.kanshi = {
    enable = false;
    systemdTarget = "hyprland-session.target";
    settings = [
      {
        profile = {
          name = "undocked";
          outputs = [
            {
              criteria = "eDP-1";
              mode = "3840x2400@59.99400";
              position = "0,0";
              scale = 2.0;
            }
          ];
        };
      }
      {
        profile = {
          name = "ducked1";
          outputs = [
            {
              criteria = "eDP-1";
              mode = "3840x2400@59.99400";
              position = "0,0";
              scale = 2.0;
            }
            {
              criteria = "DP-3";
              mode = "1920x1080@120.00000";
              position = "1920,0";
              scale = 1.0;
            }
          ];
        };
      }
      {
        profile = {
          name = "ducked2";
          outputs = [
            {
              criteria = "eDP-1";
              mode = "3840x2400@59.99400";
              position = "0,0";
              scale = 2.0;
            }
            {
              criteria = "DP-4";
              mode = "1920x1080@120.00000";
              position = "1920,0";
              scale = 1.0;
            }
          ];
        };
      }
      {
        profile = {
          name = "ultraducked";
          outputs = [
            {
              criteria = "eDP-1";
              mode = "3840x2400@59.99400";
              position = "0,960";
              scale = 2.0;
            }
            {
              criteria = "DP-3";
              mode = "1920x1080@120.00000";
              position = "1920,0";
              scale = 1.0;
            }
            {
              criteria = "DP-4";
              mode = "1920x1080@120.00000";
              position = "1920,1080";
              scale = 1.0;
            }
          ];
        };
      }
    ];
  };
}
