{ inputs, config, pkgs, ... }:

{
	hardware.bluetooth.enable = true;

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
#                        { "device.name", "matches", "bluez_card.*" },
#                    }
#                },
#                apply_properties = {
#                    ["bluez5.codecs"] = "[ aptx_ll aptx aac sbc ]",
#                },
#            }
#            table.insert(bluez_monitor.rules, rule)
#        '';
#    };
}
