{ lib, inputs, config, pkgs, ... }:

{
	imports =
		[ 
		./boot.nix
		./virtualisation.nix
		./networking.nix
		./pipewire.nix
		./users.nix
        ./video.nix
		inputs.impermanence.nixosModules.impermanence
		];

	boot.kernelPackages = pkgs.linuxPackages_latest;
	hardware.enableRedistributableFirmware = true;
	hardware.enableAllFirmware = true;

	hardware.bluetooth.enable = true;

	time.timeZone = "America/Toronto";
	i18n.defaultLocale = "en_CA.UTF-8";
	console ={
		keyMap = "ca";
		earlySetup = true;
		font = "ter-v32n";
		packages = with pkgs; [
			terminus_font
		];
	};

    powerManagement = {
        enable = true;
        cpuFreqGovernor = "powersave";
    };

	environment = {
		systemPackages = with pkgs; [
            pulseaudio
			virt-manager
			libguestfs
			lftp
            killall
			tmux
			mlocate
			gnupg
			nix-prefetch-github
			miniupnpc
            acpi
            expressvpn
		];
		persistence."/persist" = {
			hideMounts = true;
			directories = [
				"/etc/nixos"
				"/var/log"
				"/var/lib/bluetooth"
				"/var/lib/nixos"
                "/var/lib/expressvpn"
				"/etc/NetworkManager/system-connections"
                "/cat_installer"
			];
			files = [
				"/etc/machine-id"
                "/crypto_keyfile.cpio.gz"
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
			"s" = "sudo ";
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

	services = {
		printing.enable = true;
	};

	nix = {
		package = pkgs.nixFlakes;
		gc = {
			automatic = true;
			dates = "monthly";
			options = "--delete-older-than 1m";
		};
		settings ={
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

  security.pam.services.swaylock.text = ''
        auth include login
  '';
}
