{ lib, inputs, config, pkgs, ... }: let
win11 = pkgs.writeText "win11.xml" (builtins.readFile ./win11.xml);
in {
    boot.extraModulePackages = lib.mkBefore (with config.boot.kernelPackages; [
        vendor-reset
    ]);
    boot.initrd.kernelModules = [ "vendor_reset" ];
    boot.kernelParams = [
        "video=efifb:off,vesafb:off"
        "pcie_acs_override=downstream,multifunction"
        "amd_iommu=pt"
        "default_hugepagesz=1G"
        "hugepagesz=1G"
        "hugepages=24"
        "vfio-pci.ids=1002:731f,1002:ab38"
        "kvm_amd.avic=1"
        "kvm_amd.nested=0"
        "kvm_amd.sev=0"
    ];

    environment.systemPackages = with pkgs; [
        virtiofsd
    ];

    virtualisation = {
        libvirtd = {
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

    environment.etc."libvirt/vbios/RX5700XT.rom".source = ../../resources/RTX4080.rom;
    system.activationScripts.win11.text = ''
        yes | cp -f ${win11} /var/lib/libvirt
    '';

    systemd.tmpfiles.rules = [
        "f /dev/shm/looking-glass2 0660 william qemu-libvirtd -"
    ];
}
