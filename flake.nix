{
	description = "flake for linux-laptop";

	inputs = {
		nixpkgs.url = "nixpkgs/nixos-unstable";
		stable.url = "github:NixOS/nixpkgs/nixos-23.05";

		sops-nix.url = github:Mic92/sops-nix;
		impermanence.url = github:nix-community/impermanence;

		home-manager = {
			url = github:nix-community/home-manager/2f3367769a93b226c467551315e9e270c3f78b15;
			#url = "git+file:///home/william/priv/code/home-manager";
			inputs.nixpkgs.follows = "nixpkgs";
		};
		nixvim = {
			url = github:nix-community/nixvim;
			inputs.nixpkgs.follows = "nixpkgs";
		};
		hosts.url = github:StevenBlack/hosts;
        notnft.url = github:chayleaf/notnft;
		spicetify-nix.url = github:the-argus/spicetify-nix;
		neovim-nightly-overlay.url = github:nix-community/neovim-nightly-overlay;

		stylix.url = github:danth/stylix;

		nur.url = github:nix-community/NUR;

		hyprland.url = github:hyprwm/Hyprland;
		hyprgrass = {
			url = github:horriblename/hyprgrass;
			inputs.hyprland.follows = "hyprland"; # IMPORTANT
		};
	};

	outputs = { self, nixpkgs, home-manager, ... } @ inputs: let
        mkLib = nixpkgs:
            nixpkgs.lib.extend
            (self: super: {mine = import ./lib {lib = self;};});
        lib = mkLib inputs.nixpkgs;
        commonNixosModules = [
            {
                nixpkgs.overlays = [ 
                    (import ./pkgs).overlay
                    inputs.neovim-nightly-overlay.overlay
                    (final: prev: {
                        stable = import inputs.stable { system = final.system; };
                    })
                ];
            }
            inputs.nur.nixosModules.nur
            inputs.stylix.nixosModules.stylix
            inputs.sops-nix.nixosModules.sops
            inputs.hosts.nixosModule
            #inputs.notnft.nixosModules.default
            ./common
            home-manager.nixosModules.home-manager
            {
                nixpkgs.overlays = [
                    inputs.nur.overlay
                ];
                home-manager.extraSpecialArgs = {inherit inputs;};
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.sharedModules = [
                    inputs.impermanence.nixosModules.home-manager.impermanence
                    inputs.sops-nix.homeManagerModules.sops
                    inputs.nixvim.homeManagerModules.nixvim
                    inputs.nur.hmModules.nur
                ];
                home-manager.users.william = import ./common/william;
            }
        ];
    in rec {
		nixosConfigurations = {
			haskell_slay_slay = nixpkgs.lib.nixosSystem {
				system = "x86_64-linux";
				specialArgs = { inherit inputs; inherit lib; };
				modules = [
                    ./graphical
					./haskell_slay_slay
                    {home-manager.sharedModules = [
                        ./graphical/william
                        ./haskell_slay_slay/william
                    ];}
				] ++ commonNixosModules;
			};
			linux-amd = nixpkgs.lib.nixosSystem {
				system = "x86_64-linux";
				specialArgs = { inherit inputs; inherit lib; };
				modules = [
                    ./graphical
					./linux-amd
                    {home-manager.sharedModules = [
                        ./graphical/william
                        ./linux-amd/william
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
