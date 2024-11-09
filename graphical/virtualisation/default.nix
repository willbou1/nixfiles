{
  inputs,
  config,
  pkgs,
  ...
}: let
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
  environment = {
    persistence."/persist".directories = [
      "/var/lib/libvirt"
    ];
    systemPackages = with pkgs; [
      virt-manager
      libguestfs
      lsiommu
      win-virtio
    ];
  };

  virtualisation = {
    libvirtd = {
      enable = true;
      onBoot = "ignore";
      onShutdown = "shutdown";
      qemu = {
        ovmf = {
          enable = true;
          packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
              httpSupport = false;
              tlsSupport = false;
            })
            .fd
          ];
        };
        swtpm.enable = true;
        runAsRoot = true;
      };
    };
    #        kvmfr = {
    #            enable = true;
    #            shm = {
    #                enable = true;
    #                size = 128;
    #                user = "william";
    #                group = "libvirtd";
    #                mode = "0600";
    #            };
    #        };
  };

  systemd.tmpfiles.rules = [
    "f /dev/shm/looking-glass1 0660 william qemu-libvirtd -"
  ];

  system.activationScripts.libvirt-pools.text = ''
    storage="/var/lib/libvirt/storage"
    cp -f ${downloads_pool} "$storage/downloads_pool.xml"
    ln -sf "$storage/downloads_pool.xml" "$storage/autostart/"
  '';
}
