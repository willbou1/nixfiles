{
	imports = [
		./boot.nix
		./video.nix
		./power.nix
		./virtualisation
        ./security.nix
        ./virtualisation
	];
    environment.persistence."/persist".directories = [
        "/srv/torrents"
    ];
	networking.networkmanager.wifiAddress = "10.0.0.161/24,10.0.0.1";
	networking.hostName = "haskell_slay_slay";
    environment.shellAliases = {
        "nr" = "sudo nixos-rebuild --flake /etc/nixos#haskell_slay_slay switch";
    };
    services.deluge.config.download_location = "/srv/torrents";
}
