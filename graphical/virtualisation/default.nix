{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  lsiommu = pkgs.writeShellScriptBin "lsiommu" ''
    #!/bin/bash
    for d in $(find /sys/kernel/iommu_groups/ -type l | sort -n -k5 -t/); do
        n=''${d#*/iommu_groups/*}
        n=''${n%%/*}
        printf 'IOMMU Group %s ' "$n"
        ${pkgs.pciutils}/bin/lspci -nns "''${d##*/}"
    done;
  '';
  downloads_pool = pkgs.writeText "downloads_pool.xml" (builtins.readFile ./downloads_pool.xml);
in {
  boot.initrd.kernelModules = ["vfio" "vfio_iommu_type1" "vfio_pci"];
  boot.kernelModules = ["kvmfr"];
  boot.extraModulePackages = mkBefore (with config.boot.kernelPackages; [ kvmfr ]);

  environment = {
    persistence."/persist".directories = [
      "/var/lib/libvirt"
    ];
    systemPackages = with pkgs; [
      virt-manager
      libguestfs
      lsiommu
      virtio-win
      virtiofsd
    ];
  };

  virtualisation = {
    libvirtd = {
      enable = true;
      onBoot = "ignore";
      onShutdown = "shutdown";
      qemu = {
        verbatimConfig = ''
          cgroup_device_acl = [
            "/dev/null", "/dev/full", "/dev/zero",
            "/dev/random", "/dev/urandom",
            "/dev/ptmx", "/dev/kvm",
            "/dev/rtc","/dev/hpet",
            "/dev/kvmfr0", "/dev/kvmfr1"
          ]
        '';
        swtpm.enable = true;
        runAsRoot = true;
      };
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="kvmfr", OWNER="william", GROUP="libvirtd", MODE="0600"
  '';

  system.activationScripts.libvirt-pools.text = ''
    storage="/var/lib/libvirt/storage"
    cp -f ${downloads_pool} "$storage/downloads_pool.xml"
    ln -sf "$storage/downloads_pool.xml" "$storage/autostart/"
  '';
}
