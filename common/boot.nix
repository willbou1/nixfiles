{ pkgs, ... }:

{
	boot.kernelPackages = pkgs.linuxPackages_zen;
    boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
	boot.supportedFilesystems = [ "ntfs" ];
}
