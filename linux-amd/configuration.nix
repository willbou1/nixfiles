{ pkgs, ... }:

{
	imports = [
		./boot.nix
		./networking.nix
		./video.nix
        ./virtualisation
	];
    environment.shellAliases = {
        "nr" = "sudo nixos-rebuild --flake /etc/nixos#linux-amd switch";
    };
    programs.i3lock = {
      enable = true;  
      package = pkgs.i3lock-color;
    };
}
