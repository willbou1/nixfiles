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
        neovim-nightly-overlay.url = github:nix-community/neovim-nightly-overlay;

        stylix.url = github:danth/stylix;

        nur.url = github:nix-community/NUR;
	};

	outputs = { self, nixpkgs, home-manager, ... } @ inputs: let
        commonNixosModules = [
            inputs.nur.nixosModules.nur
            {
                nixpkgs.overlays = [ 
                    (import ./pkgs).overlay
                    inputs.neovim-nightly-overlay.overlay
                ];
            }
            inputs.stylix.nixosModules.stylix
            inputs.sops-nix.nixosModules.sops
            inputs.hosts.nixosModule
            ./common/configuration.nix
            home-manager.nixosModules.home-manager
            {
                home-manager.extraSpecialArgs = {inherit inputs;};
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.sharedModules = [
                    inputs.impermanence.nixosModules.home-manager.impermanence
                    inputs.sops-nix.homeManagerModules.sops
                    inputs.nixvim.homeManagerModules.nixvim
                    inputs.nur.hmModules.nur
                ];
                home-manager.users.william = import ./common/william/home.nix;
            }
        ];
    in rec {
		nixosConfigurations = {
			haskell_slay_slay = nixpkgs.lib.nixosSystem {
				system = "x86_64-linux";
				specialArgs = { inherit inputs; };
				modules = [
					./haskell_slay_slay/configuration.nix
                    {home-manager.sharedModules = [
                        ./haskell_slay_slay/william/home.nix
                    ];}
				] ++ commonNixosModules;
			};
			linux-amd = nixpkgs.lib.nixosSystem {
				system = "x86_64-linux";
				specialArgs = { inherit inputs; };
				modules = [
					./linux-amd/configuration.nix
                    {home-manager.sharedModules = [
                        ./linux-amd/william/home.nix
                    ];}
				] ++ commonNixosModules;
			};
		};
		homeConfigurations = {
			"william@haskell_slay_slay" = nixosConfigurations.haskell_slay_slay.config.home-manager.users.william.home;
			"william@linux-amd" = nixosConfigurations.linux-amd.config.home-manager.users.william.home;
		};
	};
}
