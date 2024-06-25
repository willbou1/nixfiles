{ pkgs, lib, ... }:

{
    specialisation."rt".configuration = {
        boot.kernelPackages = lib.mkForce pkgs.linuxPackages-rt_latest;
    };
}
