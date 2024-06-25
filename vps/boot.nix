{ inputs, config, pkgs, ... }: let
DISK_ID = "/dev/disk/by-id/fill-this-shit";
BTRFS_UUID = "/dev/disk/by-uuid/5a7e9eb7-cd9d-43af-892e-a7c70e09e6d5";
in {
    # confirm that this is good for a VM
	boot.kernel.sysctl = { "vm.swappiness" = 10;};	

	boot.loader.grub = {
		enable = true;
		device = "/dev/disk/by-uuid/";
		efiSupport = false;
		enableCryptodisk = false;
		useOSProber = false;
		gfxmodeEfi = "1024x768";
	};

    # change this
	boot.initrd.availableKernelModules = [ "vmd" "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
	boot.extraModulePackages = [ ];

    # Impermanence with BTRFS without RAM root and home
    boot.initrd.postDeviceCommands = pkgs.lib.mkBefore ''
        mkdir -p /mnt
        mount -o subvol=/ ${BTRFS_UUID} /mnt

        btrfs subvolume list -o /mnt/@root |
        cut -f9 -d' ' |
        while read subvolume; do
            echo "deleting /$subvolume subvolume..."
            btrfs subvolume delete "/mnt/$subvolume"
        done &&
        echo "deleting /root subvolume..." &&
        btrfs subvolume delete /mnt/@root

        echo "restoring blank /@root subvolume..."
        btrfs subvolume snapshot /mnt/@root-blank /mnt/@root

        umount /mnt
    '';

	fileSystems = {
		"/" = {
			device = BTRFS_UUID;
			fsType = "btrfs";
            neededForBoot = true;
			options = [ "subvol=/@root" "noatime" "compress=zstd" ];
		};
		"/persist" = {
			device = BTRFS_UUID;
			fsType = "btrfs";
			neededForBoot = true;
			options = [ "subvol=/@persist" "noatime" "compress=zstd" ];
		};
        "/srv" = {
            device = BTRFS_UUID;
            fsType = "btrfs";
            options = [ "subvol=/@srv" "noatime" "compress=zstd" ];
        };
		"/nix" = {
			device = BTRFS_UUID;
			fsType = "btrfs";
			options = [ "subvol=/@nix" "noatime" "compress=zstd" ];
		};
		"/boot" = {
			device = BTRFS_UUID;
			fsType = "btrfs";
			options = [ "subvol=/@boot" "noatime" "compress=zstd" ];
		};
		"/swap" = {
			device = BTRFS_UUID;
			fsType = "btrfs";
			options = [ "subvol=/@swap" "noatime" ];
		};
        "/.snapshots" = {
            device = BTRFS_UUID;
            fsType = "btrfs";
            options = [ "subvol=/" "noatime" "compress=zstd" ];
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
