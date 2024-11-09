{
  lib,
  config,
  pkgs,
  inputs,
  ...
}:
with builtins; {
  # default to aptx_ll for my bluetooth headphones
  home.file.".local/state/wireplumber/default-profile" = {
    force = true;
    text = ''
      [default-profile]
      bluez_card.00_1B_66_C0_52_BD=a2dp-sink-aptx_ll
    '';
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
