{ inputs, config, pkgs, ... }:

{
	users = {
		mutableUsers = false;
		users.root.hashedPassword = "$6$A9gS4xYxLg5KSlsf$yrWzk6pmzPkq5aodf3nYOveDwTvbWE3URU1htTYOiQig/nK1JNIlOHLWjRYiUrWLu/0SDK25J87UFwNI2xQR01";
		users.william = {
			isNormalUser = true;
			description = "william";
			extraGroups = [ "wheel" ];
			shell = pkgs.fish;
			hashedPassword = "$6$A9gS4xYxLg5KSlsf$yrWzk6pmzPkq5aodf3nYOveDwTvbWE3URU1htTYOiQig/nK1JNIlOHLWjRYiUrWLu/0SDK25J87UFwNI2xQR01";
		};
	};
}
