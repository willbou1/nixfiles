{ inputs, config, pkgs, ... }:

{
	users = {
		mutableUsers = false;
		users.root.hashedPassword = "$y$j9T$PY3O4ZGgd.CdIq.EkwuIV/$WYwblb5ck3iX3Y76DFCFJDbdfRRh7xF9QTB3tc6tb31";
		users.william = {
			isNormalUser = true;
			description = "william";
			extraGroups = [ "wheel" ];
			shell = pkgs.fish;
			hashedPassword = "$y$j9T$fF72zVZ48p/SPowZ83vF//$gpvRfCo3Qr04rkRwsZh.p3kOkOflmhQBHQM8kr6ee98";
		};
	};
}
