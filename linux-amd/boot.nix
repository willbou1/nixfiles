{ inputs, config, pkgs, ... }: let
EFI_UUID = "/dev/disk/by-uuid/";
NVME_LUKS_UUID = "/dev/disk/by-uuid/";
PRIV_LUKS_UUID = "/dev/disk/by-uuid/";
DATA_LUKS_UUID = "/dev/disk/by-uuid/";
NVME/_BTRFS_UUID = "/dev/disk/by-uuid/";
PRIV_BTRFS_UUID = "/dev/disk/by-uuid/";
DATA_BTRFS_UUID = "/dev/disk/by-uuid/";
in {
	environment.persistence."/persist".files = [
        "/crypto_keyfile.cpio.gz"
    ];

	boot.kernel.sysctl = { "vm.swappiness" = 10;};	
	boot.loader.efi.canTouchEfiVariables = true;
	boot.loader.efi.efiSysMountPoint = "/boot/efi";
	boot.loader.grub = {
		enable = true;
		device = "nodev";
		efiSupport = true;
		enableCryptodisk = true;
		#useOSProber = true;
		gfxmodeEfi = "3840x2400";
	};

    # check it out cause it may not work first try
	boot.initrd.availableKernelModules = [ "vmd" "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
	boot.kernelModules = [ "iwlwifi" ];
	boot.extraModulePackages = [ ];

	boot.initrd.luks.devices = let
        luksDevice = uuid: {
            device = uuid;
            preLVM = true;
            keyFile = "/crypto_keyfile.bin";
            allowDiscards = true;
        };
    in {
		nvme = luksDevice NVME_LUKS_UUID;
		priv = luksDevice PRIV_LUKS_UUID;
		data = luksDevice DATA_LUKS_UUID;
	};
    boot.initrd.prepend = ["${/crypto_keyfile.cpio.gz}"];

	fileSystems = {
        # In RAM
		"/" = {
			device = "none";
			fsType = "tmpfs";
			options = [ "size=10G" "mode=755" ];
		};
		"/home/william" = {
			device = "none";
			fsType = "tmpfs";
			options = [ "size=10G" "mode=777" ];
		};
        # nvme
		"/persist" = {
			device = NVME_BTRFS_UUID;
			fsType = "btrfs";
			neededForBoot = true;
			options = [ "subvol=/@persist" "noatime" "compress=zstd" ];
		};
		"/nix" = {
			device = NVME_BTRFS_UUID;
			fsType = "btrfs";
			options = [ "subvol=/@nix" "noatime" "compress=zstd" ];
		};
		"/boot" = {
			device = NVME_BTRFS_UUID;
			fsType = "btrfs";
			options = [ "subvol=/@boot" "noatime" "compress=zstd" ];
		};
        "/.snapshots/nvme" = {
            device = NVME_BTRFS_UUID;
            fsType = "btrfs";
            options = [ "subvol=/" "noatime" "compress=zstd" ];
        };
        # priv
		"/prib" = {
			device = PRIV_BTRFS_UUID;
			fsType = "btrfs";
			options = [ "subvol=/@boot" "noatime" "compress=zlib" ];
		};
        "/.snapshots/priv" = {
            device = PRIV_BTRFS_UUID;
            fsType = "btrfs";
            options = [ "subvol=/" "noatime" "compress=zlib" ];
        };
        # data
		"/data" = {
			device = DATA_BTRFS_UUID;
			fsType = "btrfs";
			options = [ "subvol=/@boot" "noatime" "compress=zlib" ];
		};
        "/.snapshots/data" = {
            device = DATA_BTRFS_UUID;
            fsType = "btrfs";
            options = [ "subvol=/" "noatime" "compress=zlib" ];
        };
        # misc
		"/boot/efi" = {
			device = EFI_UUID;
			fsType = "vfat";
		};
		"/swap" = {
			device = NVME_BTRFS_UUID;
			fsType = "btrfs";
			options = [ "subvol=/@swap" "noatime" ];
		};
	};

	systemd.services.create-swapfile = {
		serviceConfig.Type = "oneshot";
		wantedBy = [ "swap-swapfile.swap" ];
		script = ''
			${pkgs.coreutils}/bin/truncate -s 0 /swap/swapfile
			${pkgs.e2fsprogs}/bin/chattr +C /swap/swapfile
			${pkgs.btrfs-progs}/bin/btrfs property set /swap/swapfile compression none
		'';
	};

  swapDevices = [{
  	device = "/swap/swapfile";
	size = 1024 * 64;
  }];
}
