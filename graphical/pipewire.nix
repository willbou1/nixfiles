{
  hardware.bluetooth.enable = true;

  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services = {
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
      #wireplumber.configPackages = with pkgs; [
      #    (writeTextDir "share/wireplumber/main.lua.d/51-disable-suspension.lua" ''
      #        table.insert (alsa_monitor.rules, {
      #            matches = {
      #                {
      #                    -- Matches all sources.
      #                    { "node.name", "matches", "alsa_input.*" },
      #                },
      #                {
      #                    -- Matches all sinks.
      #                    { "node.name", "matches", "alsa_output.*" },
      #                },
      #            },
      #            apply_properties = {
      #                ["session.suspend-timeout-seconds"] = 0,  -- 0 disables suspend
      #            },
      #        })
      #    '')
      #    (writeTextDir "share/wireplumber/bluetooth.lua.d/51-disable-suspension.lua" ''
      #        table.insert (bluez_monitor.rules, {
      #            matches = {
      #                {
      #                    -- Matches all sources.
      #                    -- Note: bluez_input, not alsa_input
      #                    { "node.name", "matches", "bluez_input.*" },
      #                },
      #                {
      #                    -- Matches all sinks.
      #                    -- Note: bluez_output, not alsa_output
      #                    { "node.name", "matches", "bluez_output.*" },
      #                },
      #            },
      #            apply_properties = {
      #                ["session.suspend-timeout-seconds"] = 0,  -- 0 disables suspend
      #            },
      #        })
      #    '')
      #];
    };
  };
}
