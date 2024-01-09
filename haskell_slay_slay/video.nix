{ lib, inputs, config, pkgs, ... }:

{
    environment.systemPackages = [ pkgs.displaylink ];
    boot.kernelModules = [ "evdi" ];
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
        opengl = {
            enable = true;
            driSupport = true;
            driSupport32Bit = true;
            extraPackages = with pkgs; [
                intel-media-driver
                vaapiIntel
                    vaapiVdpau
                    libvdpau-va-gl
            ];
            extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
            setLdLibraryPath = true;
        };
    };
    nixpkgs.config.packageOverrides = pkgs: {
        vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
    };
}
