{ inputs, config, pkgs, ... }:

{
	virtualisation.libvirtd = {
		enable = true;
		onBoot = "ignore";
		onShutdown = "shutdown";
		qemu.ovmf.enable = true;
		#qemuRunAsRoot = true;
	};

	systemd.tmpfiles.rules = [
		"f /dev/shm/looking-glass 0660 william qemu-libvirtd -"
	];	
}
