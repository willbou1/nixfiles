{
	imports = [
		./boot.nix
		./video.nix
		./power.nix
		./virtualisation
        ./security.nix
        ./virtualisation
	];
	networking.networkmanager.wifiAddress = "10.0.0.161/24,10.0.0.1";
	networking.hostName = "haskell_slay_slay";
}
