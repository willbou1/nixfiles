{ inputs, config, pkgs, ... }:

{
	users = {
		users.william = {
			extraGroups = [ "networkmanager" "libvirtd" "video" ];
		};
	};
}
