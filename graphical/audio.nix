{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    pulseaudio # for pactl command
    alsa-tools
  ];

  hardware.bluetooth.enable = true;

  security.rtkit.enable = true;
  services = {
    pulseaudio.enable = false;
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
      #wireplumber.extraConfig = {
      #  "disable-suspension" = {
      #    "monitor.alsa.rules" = [
      #      {
      #        matches = [
      #          { "node.name" = "alsa_input.*"; }
      #          { "node.name" = "alsa_output.*"; }
      #        ];
      #        actions.update-props = {
      #          "session.suspend-timeout-seconds" = 0;
      #        };
      #      }
      #    ];
      #    "monitor.bluez.rules" = [
      #      {
      #        matches = [
      #          { "node.name" = "bluez_input.*"; }
      #          { "node.name" = "bluez_output.*"; }
      #        ];
      #        actions.update-props = {
      #          "session.suspend-timeout-seconds" = 0;
      #        };
      #      }
      #    ];
      #  };
      #};
    };
  };
}
