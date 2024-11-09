{ inputs, config, pkgs, ... }: let
EFI_UUID = "/dev/disk/by-uuid/04DC-F1BD";
NVME_LUKS_UUID = "/dev/disk/by-uuid/4d0c061c-d3f2-4553-9aca-ddb5ec7bd1d7";
PRIV_LUKS_UUID = "/dev/disk/by-uuid/82bc1f36-42ef-4548-aa86-3171543b51c3";
DATA_LUKS_UUID = "/dev/disk/by-uuid/5f8a6096-144d-48bd-8334-3e96b36ab772";
NVME_BTRFS_UUID = "/dev/disk/by-uuid/e52972d1-0aa0-4c70-afce-05452f847ac9";
PRIV_BTRFS_UUID = "/dev/disk/by-uuid/ca6def83-269e-43d8-9699-bb0c9e994f27";
DATA_BTRFS_UUID = "/dev/disk/by-uuid/15def696-18d6-4531-8cd0-9fa9817af8ea";
in {
	environment.persistence."/persist".files = [
        "/crypto_keyfile.cpio.gz"
    ];

	boot.kernelPackages = pkgs.linuxPackages_zen;
    hardware.cpu.amd.updateMicrocode = true;
	boot.kernel.sysctl = { "vm.swappiness" = 10;};	
	boot.loader.efi.canTouchEfiVariables = true;
	boot.loader.efi.efiSysMountPoint = "/boot/efi";
	boot.loader.grub = {
		enable = true;
		device = "nodev";
		efiSupport = true;
		enableCryptodisk = true;
		#gfxmodeEfi = "3840x2400";
	};

	boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
    boot.kernelModules = [ "i2c-dev" ];

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
		"/home/william/priv" = {
			device = PRIV_BTRFS_UUID;
			fsType = "btrfs";
			options = [ "subvol=/@william" "noatime" "compress=zlib" ];
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
			options = [ "subvol=/@data" "noatime" "compress=zlib" ];
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
