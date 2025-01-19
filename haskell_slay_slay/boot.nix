{pkgs, ...}: let
  LUKS_UUID = "/dev/disk/by-uuid/5a7e9eb7-cd9d-43af-892e-a7c70e09e6d5";
  BTRFS_UUID = "/dev/disk/by-uuid/ac418410-e9e7-4e54-9dec-5c9b0a0fb75d";
in {
  environment.persistence."/persist".files = [
    "/crypto_keyfile.cpio.gz"
  ];

  boot.kernelPackages = pkgs.linuxPackages_zen;
  hardware.cpu.intel.updateMicrocode = true;
  boot.kernel.sysctl = {"vm.swappiness" = 10;};
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

  boot.initrd.availableKernelModules = ["vmd" "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc"];
  boot.initrd.kernelModules = ["kvm-intel"];
  boot.kernelModules = ["iwlwifi"];
  boot.extraModulePackages = [];

  boot.initrd.luks.devices = {
    root = {
      device = LUKS_UUID;
      preLVM = true;
      keyFile = "/crypto_keyfile.bin";
      allowDiscards = true;
    };
  };
  boot.initrd.prepend = ["${/crypto_keyfile.cpio.gz}"];

  fileSystems = {
    "/" = {
      device = "none";
      fsType = "tmpfs";
      options = ["size=10G" "mode=755"];
    };
    "/home/william" = {
      device = "none";
      fsType = "tmpfs";
      options = ["size=10G" "mode=777"];
    };
    "/persist" = {
      device = BTRFS_UUID;
      fsType = "btrfs";
      neededForBoot = true;
      options = ["subvol=/@persist" "noatime" "compress=zstd"];
    };
    "/nix" = {
      device = BTRFS_UUID;
      fsType = "btrfs";
      options = ["subvol=/@nix" "noatime" "compress=zstd"];
    };
    "/boot" = {
      device = BTRFS_UUID;
      fsType = "btrfs";
      options = ["subvol=/@boot" "noatime" "compress=zstd"];
    };
    "/boot/efi" = {
      device = "/dev/disk/by-uuid/39D4-AD91";
      fsType = "vfat";
    };
    "/swap" = {
      device = BTRFS_UUID;
      fsType = "btrfs";
      options = ["subvol=/@swap" "noatime"];
    };
    "/.snapshots" = {
      device = BTRFS_UUID;
      fsType = "btrfs";
      options = ["subvol=/" "noatime" "compress=zstd"];
    };
  };

  systemd.services.create-swapfile = {
    serviceConfig.Type = "oneshot";
    wantedBy = ["swap-swapfile.swap"];
    script = ''
      ${pkgs.coreutils}/bin/truncate -s 0 /swap/swapfile
      ${pkgs.e2fsprogs}/bin/chattr +C /swap/swapfile
      ${pkgs.btrfs-progs}/bin/btrfs property set /swap/swapfile compression none
    '';
  };

  swapDevices = [
    {
      device = "/swap/swapfile";
      size = 1024 * 64;
    }
  ];
}
