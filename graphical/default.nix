{ lib, inputs, config, pkgs, ... }:
with lib;
{
    imports = (mine.autoInclude ./. [
        ./specialisations
        ./william
        ./networking-alt.nix
    ]);

    stylix = {
        image = ../resources/wallpapers/space.jpg;
        polarity = "dark";
        homeManagerIntegration.followSystem = false;
        targets.grub.useImage = true;
        fonts.sizes = {
            terminal = 48;
            applications = 48;
            desktop = 48;
            popups = 48;
        };
    };

	environment = {
		systemPackages = with pkgs; [
            pulseaudio
            exfat
		];
		shellAliases = {
            "b" = "bluetoothctl";
			"virsh" = "virsh -c qemu:///system";
			"vs" = "virsh";
		};
		sessionVariables.NIXOS_OZONE_WL = "1";
	};

	nixpkgs = {
		config.joypixels.acceptLicense = true;
	};

    xdg = {
        autostart.enable = true;
        portal = {
           enable = true;
           extraPortals = [
               pkgs.xdg-desktop-portal
               pkgs.xdg-desktop-portal-hyprland
               pkgs.xdg-desktop-portal-gtk
           ];
           config.common.default = "*";
        };
    };

    hardware.sane = {
        enable = true;
        extraBackends = [ pkgs.sane-airscan ];
    };

    services.udisks2.enable = true;
    services.xserver = {
        enable = true;
        xkb = {
            layout = "ca";
            variant = "multix";
        };
        displayManager.startx.enable = true;
    };
}
