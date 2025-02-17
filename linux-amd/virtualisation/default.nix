{
  lib,
  config,
  pkgs,
  ...
}:
with builtins; let
  win11 = pkgs.writeText "win11.xml" (builtins.readFile ./win11.xml);
  isos_pool = pkgs.writeText "isos_pool.xml" (builtins.readFile ./isos_pool.xml);
in {
  boot.extraModulePackages = lib.mkBefore (with config.boot.kernelPackages; [
    vendor-reset
  ]);
  boot.initrd.kernelModules = ["vendor_reset"];
  system.activationScripts.vendor-reset.text = ''
    echo 'device_specific' > /sys/bus/pci/devices/0000:10:00.0/reset_method
  '';

  boot = {
    extraModprobeConfig = ''
      softdep amdgpu pre: vfio-pci
    '';
    kernelParams = [
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
      "kvmfr.static_size_mb=64,64"
    ];
  };

  environment = {
    systemPackages = with pkgs; [virtiofsd];
    etc."libvirt/vbios/RX5700XT.rom".source = ../../resources/RTX4080.rom;
  };

  virtualisation.libvirtd = {
    hooks.qemu = {
      win11 = pkgs.writeShellScript "win11.sh" (readFile ./win11.sh);
    };
  };

  system.activationScripts.win11.text = ''
    cp -f ${win11} /var/lib/libvirt/qemu/win11.xml
  '';

  system.activationScripts.libvirt-pools.text = ''
    storage="/var/lib/libvirt/storage"
    cp -f ${isos_pool} "$storage/isos_pool.xml"
    ln -sf "$storage/isos_pool.xml" "$storage/autostart/"
  '';
}
