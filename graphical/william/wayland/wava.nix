{pkgs, lib, config, ...}:
with lib;
{
  home.packages = with pkgs; [wava];

  xdg.configFile."wava/config".text = generators.toINI {} {
    general = {
      framerate = 100;

      # -1 = adaptive Vsync (G-Sync. FreeSync, etc.)
      # 0 = off
      # 1 up to screen refresh rate = visualizer runs at refresh rate divided by this
      # number
      vsync = 1;

      # bars = 100;
      bar_width = 20;
      bar_spacing = 1;
    };

    window = {
      width = foldl (w: m: max w m.width) 0 config.home.monitors;
      height = foldl (h: m: max h m.height) 0 config.home.monitors;
      fullscreen = false;
      border = false;
      transparency = true;
      keep_below = false;

      # top_left, top, top_right, left, center, right, bottom_left,
      # bottom, bottom_right and none if you don't want manual positioning
      alignment = "bottom";
      x_padding = 0;
      y_padding = 0;

      interactable = true;
      taskbar_icon = false;

      # Do or don't hold on to an viewport size specified by width and height
      #
      # In Layman's terms: The visualizer holds the same size as you're resizing the
      # window
      #
      # NOTE: Works only on "supported" modes, aka cairo or OpenGL
      hold_size = true;
    };

    x11 = {
      root_window = false;
      override_redirect = false;
      reload_on_display_configure = false;
      # monitor_name = HDMI-A-2
    };

    wayland = {
      background_layer = true;

      # monitor_name = DP-2
    };

    gl = {
      module_1 = "bars";

      post_shader = "shadow";
      resolution_scale = 1.0;
    };

    cairo = {
      module_1 = "bars";
    };

    input = {
      method = "pulseaudio";
      source = "auto";

      # method = "pipewire";
      # source = "default";

      size = 12; # 2^size
      rate = 44100;
      latency = 128;
    };

    output = {
      # they are the following: x11_opengl, x11_cairo, wayland_opengl, wayland_cairo,
      #   win_opengl, win_cairo, sdl2_opengl and the "unsupported" ncurses mode
      method = "wayland_opengl";

      channels = "stereo";
    };

    color = with config.stylix.opacity;
            with config.lib.stylix.colors.withHashtag;
      {
        background = "\"${base00}\"";
        foreground = "\"${base05}\"";

        foreground_opacity = applications;
        background_opacity = terminal;

        gradient_count = 8;
        gradient_color_1 = "\"${base08}\"";
        gradient_color_2 = "\"${base09}\"";
        gradient_color_3 = "\"${base0A}\"";
        gradient_color_4 = "\"${base0B}\"";
        gradient_color_5 = "\"${base0C}\"";
        gradient_color_6 = "\"${base0D}\"";
        gradient_color_7 = "\"${base0E}\"";
        gradient_color_8 = "\"${base0F}\"";
      };

    filter = {
      name = "default";
      lower_cutoff_freq = 26;
      higher_cutoff_freq = 15000;
      integral = 85; # % smoothing
      monstercat = 1;
      waves = 1;
      gravity = 400; # %

      # In bar height, bars that would have been lower that this (in pixels) will not be drawn.
      ignore = 0;

      # Adjust the logarithmic scale of the frequency band (the lower the value,
      # the less difference between bars in frequency)
      log = 1.55;

      # Average out the value of each ~~odd~~ even bar (just like the monstercat visualizer)
      oddoneout = false;

      # Balances the eq towards higher or lower frequencies
      # lower value->lower frequencies, higher value->higher frequencies
      eq_balance = 0.67;

      fft_size = 14;

      # 'autosens' will attempt to balance the sensitivity to keep the amplitude of the bars
      # within a reasonable range
      autosens = false;

      # 'overshoot' allows bars to overshoot (in % of display height) without initiating autosens
      overshoot = 0;

      # Manual sensitivity in %. Autosens must be turned off for this to take effect.
      # 200.0 means double height and so on. Accepts only non-negative values.
      sensitivity = 60.0;
    };


    eq = {
      "1" = 1;# bass
      "2" = 1;
      "3" = 1;# midtone
      "4" = 1;
      "5" = 1;# treble
    };
  };
}
