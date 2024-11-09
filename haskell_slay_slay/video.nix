{ lib, inputs, config, pkgs, ... }:

{
    # Fix for low framerate on intel iGPU when not stimulated for a while
    # - Enable framebuffer compression
    # - Offload decoding to iGPU with new firmware
    # - Enable panel self-refresh to save battery
    # - Disable regular power-saving to avoid bad framerate
    boot.extraModprobeConfig = ''
        options i915 enable_dc=4 enable_fbc=1 enable_guc=3 enable_psr=2 enable_psr2_sel_fetch=1 disable_power_well=1
    '';

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

    nixpkgs.config = {
        cudaSupport = true;
        packageOverrides = pkgs: {
            vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
        };
    };
    environment.systemPackages = with pkgs; [
        cudaPackages.cudatoolkit
        displaylink
    ];
}
