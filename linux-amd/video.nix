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
                rocm-opencl-icd
                rocm-opencl-runtime
                vaapiVdpau
                libvdpau-va-gl
            ];
            setLdLibraryPath = true;
        };
    };
}
