{ inputs, config, pkgs, ... }:
with builtins;
let
    win11 = pkgs.writeText "win11.xml" (builtins.readFile ./win11.xml);
in {
    boot.kernelParams = [
        "intel_iommu=on"
        "default_hugepagesz=1G"
        "hugepagesz=1G"
        "hugepages=24"
        "kvm-intel.enable_apicv=y"
        "vfio-pci.ids=10de:27a0,10de:22bc"
    ];

    virtualisation = {
        libvirtd = {
            hooks.qemu = {
                win11 = pkgs.writeShellScript "win11.sh" (readFile ./win11.sh);
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
    system.activationScripts.win11.text = ''
        yes | cp -f ${win11} /var/lib/libvirt/
    '';
}
