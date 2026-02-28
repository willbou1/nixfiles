with builtins; {
  # default to aptx_ll for my bluetooth headphones
  home.file.".local/state/wireplumber/default-profile" = {
    force = true;
    text = ''
      [default-profile]
      bluez_card.00_1B_66_C0_52_BD=a2dp-sink-aptx_ll
    '';
  };

  services = {
    easyeffects.enable = true;
    playerctld.enable = true;
  };

  xdg.configFile."easyeffects/output/HD600.json".text = toJSON {
    output = {
      blocklist = [];
      "equalizer#0" = let
        bands = {
          band0 = {
            frequency = 20.0;
            gain = 4.0;
            mode = "RLC (BT)";
            mute = false;
            q = 1.1;
            slope = "x1";
            solo = false;
            type = "Bell";
            width = 4.0;
          };
          band1 = {
            frequency = 97.0;
            gain = -2.5;
            mode = "RLC (BT)";
            mute = false;
            q = 0.7;
            slope = "x1";
            solo = false;
            type = "Bell";
            width = 4.0;
          };
          band2 = {
            frequency = 105.0;
            gain = 5.5;
            mode = "RLC (BT)";
            mute = false;
            q = 0.71;
            slope = "x1";
            solo = false;
            type = "Lo-shelf";
            width = 4.0;
          };
          band3 = {
            frequency = 215.0;
            gain = -1.7;
            mode = "RLC (BT)";
            mute = false;
            q = 1.1;
            slope = "x1";
            solo = false;
            type = "Bell";
            width = 4.0;
          };
          band4 = {
            frequency = 1400.0;
            gain = -2.1;
            mode = "RLC (BT)";
            mute = false;
            q = 1.5;
            slope = "x1";
            solo = false;
            type = "Bell";
            width = 4.0;
          };
          band5 = {
            frequency = 2000.0;
            gain = 3.0;
            mode = "RLC (BT)";
            mute = false;
            q = 0.71;
            slope = "x1";
            solo = false;
            type = "Hi-shelf";
            width = 4.0;
          };
          band6 = {
            frequency = 2700.0;
            gain = -1.3;
            mode = "RLC (BT)";
            mute = false;
            q = 3.0;
            slope = "x1";
            solo = false;
            type = "Bell";
            width = 4.0;
          };
          band7 = {
            frequency = 3370.0;
            gain = -3.9;
            mode = "RLC (BT)";
            mute = false;
            q = 2.5;
            slope = "x1";
            solo = false;
            type = "Bell";
            width = 4.0;
          };
          band8 = {
            frequency = 5350.0;
            gain = -3.5;
            mode = "RLC (BT)";
            mute = false;
            q = 3.0;
            slope = "x1";
            solo = false;
            type = "Bell";
            width = 4.0;
          };
          band9 = {
            frequency = 11000.0;
            gain = -4.0;
            mode = "RLC (BT)";
            mute = false;
            q = 0.71;
            slope = "x1";
            solo = false;
            type = "Hi-shelf";
            width = 4.0;
          };
        };
      in {
        balance = 0.0;
        bypass = false;
        input-gain = 0.0;
        left = bands;
        mode = "FFT";
        num-bands = 10;
        output-gain = 0.0;
        pitch-left = 0.0;
        pitch-right = 0.0;
        right = bands;
        "split-channels" = false;
      };
      plugins_order = [ "equalizer#0" ];
    };
  };

  xdg.configFile."easyeffects/output/mono.json".text = toJSON {
    output = {
      blocklist = [];
      plugins_order = [
        "stereo_tools#0"
      ];
      "stereo_tools#0" = {
        balance-in = 0.0;
        balance-out = 0.0;
        bypass = false;
        delay = 0.0;
        input-gain = 0.0;
        middle-level = 0.0;
        middle-panorama = 0.0;
        mode = "LR > L+R (Mono Sum L+R)";
        mutel = false;
        muter = false;
        output-gain = 0.0;
        phasel = false;
        phaser = false;
        sc-level = 1.0;
        side-balance = 0.0;
        side-level = 0.0;
        softclip = false;
        stereo-base = 0.0;
        stereo-phase = 0.0;
      };
    };
  };
}
