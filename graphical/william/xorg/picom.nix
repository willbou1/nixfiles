{
  services.picom = {
    enable = true;
    vSync = true;
    backend = "glx";

    fade = true;
    fadeSteps = [0.05 0.05];

    shadow = true;
    shadowOpacity = 0.5;
    shadowOffsets = [(-30) (-30)];
    shadowExclude = [
      "name = 'Notification'"
      "class_i = 'dwm'"
      "class_g = 'dwm'"
      "class_i = 'Dunst'"
      "class_g = 'Dunst'"
      "window_type = 'dock'"
      "window_type = 'menu'"
      "window_type = 'dropdown_menu'"
      "window_type = 'popup_menu'"
      "window_type = 'tooltip'"
      "class_g = 'Conky'"
      "class_g ?= 'Notify-osd'"
      "class_g = 'slop'"
    ];

    opacityRules = [
      "93:class_g = 'qutebrowser' && fullscreen = 0"
      "93:class_g = 'firefox' && fullscreen = 0"
      "93:class_g = 'Tor Browser' && fullscreen = 0"
      "85:class_g = 'Element'"
      "90:class_g = 'easyeffects'"
      "90:class_g = 'spotify'"
    ];
  };
}
