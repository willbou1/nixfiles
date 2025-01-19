{pkgs, ...}:
with builtins; let
  win11 = pkgs.writeText "win11.xml" (builtins.readFile ./win11.xml);
  bind-gpu = pkgs.writeShellScriptBin "bind-gpu" ''
    echo 0000:01:00.0 > /sys/bus/pci/drivers/vfio-pci/unbind
    echo 0000:01:00.1 > /sys/bus/pci/drivers/vfio-pci/unbind
    echo 10de 27a0 > /sys/bus/pci/drivers/vfio-pci/remove_id
    echo 10de 22bc > /sys/bus/pci/drivers/vfio-pci/remove_id

    modprobe nvidia
    modprobe nvidia_drm
    modprobe nvidia_uvm
  '';
  unbind-gpu = pkgs.writeShellScriptBin "unbind-gpu" ''
    rmmod nvidia_drm
    rmmod nvidia_uvm
    rmmod nvidia_modeset
    rmmod nvidia

    echo 10de 27a0 > /sys/bus/pci/drivers/vfio-pci/new_id
    echo 10de 22bc > /sys/bus/pci/drivers/vfio-pci/new_id
  '';
in {
  boot.kernelParams = [
    "intel_iommu=on"
    "default_hugepagesz=1G"
    "hugepagesz=1G"
    "hugepages=24"
    "kvm-intel.enable_apicv=y"
    "vfio-pci.ids=10de:27a0,10de:22bc"
    "kvmfr.static_size_mb=64"

    # TODO Something seems to have broken with MSRs in the new KVM version so
    # let's siable them temporarily
    "kvm.ignore_msrs=1"
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

  environment = {
    systemPackages = with pkgs; [
      bind-gpu
      unbind-gpu
    ];
    etc."libvirt/vbios/RTX4080.rom".source = ../../resources/RTX4080.rom;
  };
  system.activationScripts.win11.text = ''
    cp -f ${win11} /var/lib/libvirt/qemu/win11.xml
  '';

  # If we bind at boot, shit goes south so let's blacklist the GPU
  # and bind it 2 minutes after we boot
  systemd = {
    services."bind-gpu" = {
      path = with pkgs; [
        kmod
      ];
      script = ''
        ${bind-gpu}/bin/bind-gpu
      '';
      serviceConfig = {
        Type = "oneshot";
        User = "root";
      };
    };
    timers."bind-gpu-timer" = {
      wantedBy = ["timers.target"];
      timerConfig = {
        OnBootSec = "3min";
        AccuracySec = "1min";
        Persistent = true;
        Unit = "bind-gpu.service";
      };
    };
  };
}
