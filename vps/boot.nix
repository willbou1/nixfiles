{ inputs, config, pkgs, ... }:
with builtins;
let
MAIN_UUID = "/dev/disk/by-uuid/577f2240-c659-4876-b01b-143c2bd8bf8a";
BTRFS_UUID = "/dev/disk/by-uuid/ce29b4f5-71ab-47ae-9788-8a0c30371e90";
BOOT_UUID = "/dev/disk/by-uuid/c885ad98-0369-4740-b553-122c477fe025";
EFI_UUID = "/dev/disk/by-uuid/80CB-574F";
bootNetworkConfig = {
    ip = "185.165.171.79";
    gateway = "185.165.171.1";
    subnetMask = "255.255.255.128";
    hostname = "ourmiraculous";
    dns1 = "9.9.9.9";
    dns2 = "8.8.8.8";
};
toKernelIPParam = c: "ip=${c.ip}::${c.gateway}:${c.subnetMask}:${c.hostname}::none:${c.dns1}:${c.dns2}";
in {
    # confirm that this is good for a VM
	boot.kernel.sysctl = { "vm.swappiness" = 10;};	
    environment.persistence."/persist".directories = [
        "/etc/secrets"
    ];
	
	boot.loader.grub = {
		enable = true;
		device = "/dev/vda";
		efiSupport = false;
		enableCryptodisk = false;
		useOSProber = false;
        extraConfig = ''
            set timeout=30
        '';
	};
	boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
	boot.kernelModules = [ ];
	boot.extraModulePackages = [ ];
	boot.kernelParams = [ (toKernelIPParam bootNetworkConfig) ];

	boot.initrd = {
		#luks.devices."main".device = MAIN_UUID; # not needed when using ssh
        luks.forceLuksSupportInInitrd = true;
		availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci" "sr_mod" "virtio_blk" "virtio_net" ];
		network = {
			enable = true;
			ssh = {
				enable = true;
				port = head config.services.openssh.ports;
                authorizedKeys = config.users.users.william.openssh.authorizedKeys.keys;
				hostKeys = [ "/etc/secrets/initrd/ssh_host_rsa_key" ];
            };
            postCommands = ''
                echo 'cryptsetup luksOpen ${MAIN_UUID} main && echo > /tmp/continue || /bin/sh' >> /root/.profile
                echo "Starting sshd"
            '';
		};
        postDeviceCommands = pkgs.lib.mkBefore ''
            echo "Waiting for boot device to be opened..."
            mkfifo /tmp/continue
            cat /tmp/continue
            # decryption done

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

    };
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
	size = 1024 * 4;
  }];
}
