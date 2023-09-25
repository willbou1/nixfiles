{ inputs, config, pkgs, ... }: let
lsiommu = pkgs.writeShellScriptBin "lsiommu" ''
#!/bin/bash
for d in $(find /sys/kernel/iommu_groups/ -type l | sort -n -k5 -t/); do 
    n=''${d#*/iommu_groups/*}
    n=''${n%%/*}
    printf 'IOMMU Group %s ' "$n"
    ${pkgs.pciutils}/bin/lspci -nns "''${d##*/}"
done;
'';
in {
    boot.kernelParams = [ "intel_iommu=on" "default_hugepagesz=1G" "hugepagesz=1G" "hugepages=24" "kvm-intel.enable_apicv=y" "vfio-pci.ids=10de:27a0,10de:22bc" ];
    boot.kernelModules = [ "vfio" "vfio_iommu_type1" "vfio_pci" "vfio_virqfd" ];
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
                    packages = [(pkgs.OVMF.override {
                            secureBoot = true;
                            csmSupport = false;
                            httpSupport = true;
                            tpmSupport = true;
                            }).fd];
                };
                swtpm.enable = true;
                runAsRoot = false;
            };
            hooks.qemu = {
                win11 = ./win11.sh;
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

    environment.etc."libvirt/vbios/RTX4080.rom".source = ../../resources/RTX4080.rom;

    systemd.tmpfiles.rules = [
  "f /dev/shm/looking-glass 0660 william qemu-libvirtd -"
];

}
