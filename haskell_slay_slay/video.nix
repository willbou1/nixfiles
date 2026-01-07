{
  config,
  pkgs,
  ...
}: {
  boot = {
    kernelModules = ["evdi"];
  };
  services.xserver.videoDrivers = [
    "nvidia"
  ];
  hardware = {
    brillo.enable = true;
    nvidia = {
      modesetting.enable = true;
      powerManagement.enable = true;
      open = false;
      nvidiaSettings = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      prime = {
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";
        #sync.enable = true;
        offload = {
          enable = true;
          enableOffloadCmd = true;
        };
      };
    };
    graphics = {
      enable = true;
      enable32Bit = true;
      extraPackages = with pkgs; [
        intel-media-driver
        libvdpau-va-gl
        vpl-gpu-rt
      ];
    };
  };

  nixpkgs.config = {
    cudaSupport = true;
  };
  environment.sessionVariables = {
    LIBVA_DRIVER_NAME = "iHD"; # Force intel-media-driver
  };
  environment.systemPackages = with pkgs; [
    cudaPackages.cudatoolkit
    displaylink
  ];
}
