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
}
