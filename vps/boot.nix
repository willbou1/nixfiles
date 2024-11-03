{ inputs, config, pkgs, ... }: let
MAIN_UUID = "/dev/disk/by-uuid/145d4728-714c-4e5c-b7cc-02eea3e08e45";
BTRFS_UUID = "/dev/disk/by-uuid/9454a4f2-b850-46d7-baad-696fd038ef68";
BOOT_UUID = "/dev/disk/by-uuid/7ee8b57d-a43a-4266-9951-b0b522caff20";
EFI_UUID = "/dev/disk/by-uuid/C52E-3DA5";
in {
    # confirm that this is good for a VM
	boot.kernel.sysctl = { "vm.swappiness" = 10;};	

	boot.loader.grub = {
		enable = true;
		device = "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_drive-scsi0"; 
		efiSupport = false;
		enableCryptodisk = false;
		useOSProber = false;
	};
	boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
	boot.kernelModules = [ "kvm-amd" ];
	boot.extraModulePackages = [ ];
	#boot.kernelParams = [ "ip=92.112.181.70::92.112.181.254:255.255.255.0:srv631436::none" ];

	boot.initrd = {
		secrets."crypto_keyfile.bin" = null;
		luks.devices."main".device = MAIN_UUID;
		luks.devices."main".keyFile = "/crypto_keyfile.bin";
		availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci" "virtio_scsi" ];
#		network = {
#			enable = true;
#			ssh = {
#				enable = true;
#				port = 22;
#				authorizedKeys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICJ2b2UtfnyyWsNKR96dUK6l1iVaEUc1uTEf8X8VBZeC willbou2@gmail.com" ];
#				hostKeys = [ "/etc/secrets/initrd/ssh_host_rsa_key" ];
#		};
#		};
};
    # Impermanence with BTRFS without RAM root and home
#    boot.initrd.postDeviceCommands = pkgs.lib.mkBefore ''
#        mkdir -p /mnt
#        mount -o subvol=/ ${BTRFS_UUID} /mnt
#
#        btrfs subvolume list -o /mnt/@root |
#        cut -f9 -d' ' |
#        while read subvolume; do
#            echo "deleting /$subvolume subvolume..."
#            btrfs subvolume delete "/mnt/$subvolume"
#        done &&
#        echo "deleting /root subvolume..." &&
#        btrfs subvolume delete /mnt/@root
#
#        echo "restoring blank /@root subvolume..."
#        btrfs subvolume snapshot /mnt/@root-blank /mnt/@root
#
#        umount /mnt
#    '';

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
			device = BOOT_UUID;
			fsType = "ext4";
			options = [ "noatime" ];
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
	"/boot/efi" = {
	    device = EFI_UUID;
	    fsType = "vfat";
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
