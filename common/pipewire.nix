{ inputs, config, pkgs, ... }:

{
	sound.enable = true;
	hardware.pulseaudio.enable = false;
	security.rtkit.enable = true;
	services.pipewire = {
		enable = true;
		alsa.enable = true;
		alsa.support32Bit = true;
		pulse.enable = true;
		jack.enable = true;
	};
#    environment.etc = {
#        "wireplumber/bluetooth.lua.d/51-bluez-config.lua".text = ''
#            rule = {
#                matches = {
#                    {
#                        { "node.name", "equals", "bluez_output.00_1B_66_C0_52_BD.a2dp-sink" },
#                    }
#                },
#                apply_properties = {
#                    ["api.bluez5.codec"] = "aptx_ll",
#                },
#            }
#            table.insert(bluez_monitor.rules, rule)
#        '';
#    };
}
