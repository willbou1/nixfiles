{ lib, inputs, config, pkgs, ... }:

{
    boot.initrd.kernelModules = [ "amdgpu" ];
    services.xserver.videoDrivers = ["amdgpu"];
    hardware = {
        opengl = {
            enable = true;
            driSupport = true;
            driSupport32Bit = true;
            extraPackages = with pkgs; [
                vaapiVdpau
                libvdpau-va-gl
                #rocmPackages.clr.icd
            ];
            setLdLibraryPath = true;
        };
    };
}
