{
	description = "flake for linux-laptop";

	inputs = {
		nixpkgs.url = "nixpkgs/nixos-unstable";
        nixos-stable.url = "github:NixOS/nixpkgs/nixos-23.05";
        sops-nix.url = github:Mic92/sops-nix;
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

        nur.url = github:nix-community/NUR;
	};

	outputs = { self, nixpkgs, home-manager, ... } @ inputs: rec {
		nixosConfigurations = {
			haskell_slay_slay = nixpkgs.lib.nixosSystem {
				system = "x86_64-linux";
				specialArgs = { inherit inputs; };
				modules = [
                    inputs.nur.nixosModules.nur
                    {
                        nixpkgs.overlays = [ 
                            (import ./pkgs).overlay
                        ];
                    }
                    inputs.stylix.nixosModules.stylix
                    inputs.sops-nix.nixosModules.sops
					./modules/configuration.nix
					home-manager.nixosModules.home-manager
					{
						home-manager.extraSpecialArgs = {inherit inputs;};
						home-manager.useGlobalPkgs = true;
						home-manager.useUserPackages = true;
                        home-manager.sharedModules = [
                            inputs.impermanence.nixosModules.home-manager.impermanence
                            inputs.sops-nix.homeManagerModules.sops
                        ];
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
