{ lib, inputs, config, pkgs, ... }:

{
	imports =
		[ 
        ./security.nix
        ./specialisations
		./networking.nix
		./pipewire.nix
		./users.nix
        ./virtualisation
        ./boot.nix
        ./deluge.nix
		inputs.impermanence.nixosModules.impermanence
		];

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

	hardware.enableRedistributableFirmware = true;
	hardware.enableAllFirmware = true;

	hardware.bluetooth.enable = true;

	time.timeZone = "America/Toronto";
	i18n.defaultLocale = "en_CA.UTF-8";
	console.keyMap = "ca";
    console.earlySetup = true;
    console.font = "ter-i32b";
    console.packages = with pkgs; [ terminus_font ];

	environment = {
		systemPackages = with pkgs; [
            sysstat
            lm_sensors
            vlock
            file
            unzip
            lsof
            pulseaudio
			lftp
			tmux
			mlocate
			nix-prefetch-github
			miniupnpc
            acpi
            exfat
		];
		persistence."/persist" = {
			hideMounts = true;
			directories = [
				"/etc/nixos"
				"/var/log"
				"/var/lib/bluetooth"
				"/var/lib/nixos"
			];
			files = [
				"/etc/machine-id"
			];
		};
		etc = {
			"lftp.conf" = {
				text = "set ssl:verify-certificate no";
				mode = "0644";
			};
		};
		shellAliases = {
			"ll" = "ls -alF";
			"la" = "ls -A";
			"l" = "ls -CF";
			"dir" = "dir --color=auto";
			"vdir" = "vdir --color=auto";
			"fgrep" = "fgrep --color=auto";
			"egrep" = "egrep --color=auto";
			"ip" = "ip --color=auto";
            "b" = "bluetoothctl";
			"c" = "clear";
			"ftp" = "lftp";
			"fd" = "fd -H";
			"du" = "du -h";
			"free" = "free -h";
			"df" = "df -h";
			"virsh" = "virsh -c qemu:///system";
			"vs" = "virsh";
			"vpn" = "expressvpn";
			"vim" = "nvim";
			"se" = "sudoedit";
			"s" = ''sudo SSH_AUTH_SOCK="$SSH_AUTH_SOCK" '';
		};
		sessionVariables.NIXOS_OZONE_WL = "1";
	};

	programs = {
        git = {
            enable = true;
            config = {
                include.path = "/home/william/.config/git/config";
            };
        };
		fuse.userAllowOther = true;
		neovim = {
			enable = true;
			defaultEditor = true;
			viAlias = true;
			vimAlias = true;
		};
		fish.enable = true;
	};

	nix = {
		package = pkgs.nixFlakes;
		gc = {
			automatic = true;
			dates = "monthly";
			options = "--delete-older-than 1m";
		};
		settings ={
            auto-optimise-store = true;
			experimental-features = [ "nix-command" "flakes" ];
			substituters = ["https://hyprland.cachix.org"];
			trusted-public-keys = ["hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="];
		};
	};

	system.stateVersion = "23.05";

	nixpkgs = {
		hostPlatform = lib.mkDefault "x86_64-linux";
		config.joypixels.acceptLicense = true;
		config.allowUnfree = true;
# For LTO overlay
#        overlays = [
#            (final: prev: {
#                stdenv = prev.stdenvAdapters.addAttrsToDerivation {
#                NIX_CFLAGS_COMPILE = "-pipe -march=native -O2";
#                NIX_LDFLAGS = "";
#                } prev.stdenv;
#            })
#        ];
	};

    xdg = {
        autostart.enable = true;
        portal = {
           enable = true;
           extraPortals = [
               pkgs.xdg-desktop-portal
                pkgs.xdg-desktop-portal-hyprland
           ];
        };
    };
    hardware.sane = {
        enable = true;
        extraBackends = [ pkgs.sane-airscan ];
    };

    services.udisks2.enable = true;
    services.xserver = {
        enable = true;
        layout = "ca";
        xkbVariant = "multix";
        displayManager.startx.enable = true;
    };
}
