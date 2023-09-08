{
	description = "flake for linux-laptop";

	inputs = {
		nixpkgs.url = "nixpkgs/nixos-unstable";
		home-manager = {
			url = github:nix-community/home-manager;
			inputs.nixpkgs.follows = "nixpkgs";
		};
		nixvim = {
			url = github:nix-community/nixvim;
			inputs.nixpkgs.follows = "nixpkgs";
		};
		impermanence.url = github:nix-community/impermanence;
		hosts.url = github:StevenBlack/hosts;
		spicetify-nix.url = github:the-argus/spicetify-nix;

        stylix.url = github:danth/stylix;
	};

	outputs = { self, nixpkgs, home-manager, ... } @ inputs: rec {
		nixosConfigurations = {
			linux-laptop = nixpkgs.lib.nixosSystem {
				system = "x86_64-linux";
				specialArgs = { inherit inputs; };
				modules = [
                    {
                        nixpkgs.overlays = [ 
                            (import ./pkgs).overlay
                        ];
                    }
					./modules/configuration.nix
					home-manager.nixosModules.home-manager
					{
						home-manager.extraSpecialArgs = {inherit inputs;};
						home-manager.useGlobalPkgs = true;
						home-manager.useUserPackages = true;
						home-manager.users.william = import ./modules/william/home.nix;
					}
				];
			};
		};
		homeConfigurations = {
			william = nixosConfigurations.linux-laptop.config.home-manager.users.william.home;
		};
	};
}
