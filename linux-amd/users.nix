{ inputs, config, pkgs, ... }:

{
	users.users.william = {
        extraGroups = [
            "deluge"
        ];
    };
}
