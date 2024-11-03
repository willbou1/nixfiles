{
	description = "flake for linux-laptop";

	inputs = {
		#nixpkgs.url = github:NixOS/nixpkgs/nixos-24.05;
		nixpkgs.url = "git+file:./devel/nixpkgs";

		#unstable.url = github:NixOS/nixpkgs/nixos-unstable;
		unstable.url = "git+file:./devel/nixpkgs-unstable";

		#sops-nix.url = github:Mic92/sops-nix;
		sops-nix.url = "git+file:./devel/sops-nix";

		#impermanence.url = github:nix-community/impermanence;
		impermanence.url = "git+file:./devel/impermanence";

		home-manager = {
			#url = github:nix-community/home-manager/release-24.05;
			url = "git+file:./devel/home-manager";
			inputs.nixpkgs.follows = "nixpkgs";
		};

		nixvim = {
			#url = github:nix-community/nixvim/nixos-24.05;
			url = "git+file:./devel/nixvim";
			inputs.nixpkgs.follows = "nixpkgs";
		};

		hosts.url = github:StevenBlack/hosts;

		#spicetify-nix.url = github:the-argus/spicetify-nix;
		spicetify-nix.url = "git+file:./devel/spicetify-nix";

        #notnft.url = github:chayleaf/notnft;

		neovim-nightly-overlay = {
            #url = github:nix-community/neovim-nightly-overlay;
            url = "git+file:./devel/neovim-nightly-overlay";
            inputs.nixpkgs.follows = "unstable";
        };

		#stylix.url = github:danth/stylix/release-24.05;
		stylix.url = "git+file:./devel/stylix";

		nur.url = github:nix-community/NUR;

		hyprland = {
            type = "git";
            url = "https://www.github.com/hyprwm/Hyprland";
            #url = "file:./devel/hyprland";
            submodules = true;
        };

		hyprgrass = {
			#url = github:horriblename/hyprgrass;
			url = "git+file:./devel/hyprgrass";
			inputs.hyprland.follows = "hyprland"; # IMPORTANT
		};
	};

	outputs = { self, nixpkgs, home-manager, ... } @ inputs: let
        mkLib = nixpkgs:
            nixpkgs.lib.extend
            (self: super: {mine = import ./lib {lib = self;};});
        lib = mkLib inputs.nixpkgs;
        bleedingEdgePackages = [
            "neovim" "neovim-unwrapped"
            "qutebrowser"
            "mpv" "mpv-unwrapped"
            "element-desktop"
            "libreoffice-fresh"
            "steam"
        ];
        bleedingEdgeOverlay = final: prev:
            builtins.listToAttrs (builtins.map
            (p: { name = p; value = final.unstable."${p}"; }) bleedingEdgePackages);
        commonNixosModules = [
            {
                nixpkgs.overlays = [ 
                    inputs.nur.overlay
                    (import ./pkgs).overlay
                    #(import ./pkgs).nurOverlay
                    inputs.neovim-nightly-overlay.overlays.default
                    (final: prev: {
                        unstable = import inputs.unstable {
                            system = final.system;
                            config.allowUnfree = final.config.allowUnfree;
                        };
                    })
                    bleedingEdgeOverlay
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
			vps = nixpkgs.lib.nixosSystem {
				system = "x86_64-linux";
				specialArgs = { inherit inputs; inherit lib; };
				modules = [
					./vps
                    {home-manager.sharedModules = [
                        ./vps/william
                    ];}
				] ++ commonNixosModules;
			};
		};
		homeConfigurations = {
			"william@haskell_slay_slay" = nixosConfigurations.haskell_slay_slay.config.home-manager.users.william.home;
			"william@linux-amd" = nixosConfigurations.linux-amd.config.home-manager.users.william.home;
			"william@vps" = nixosConfigurations.vps.config.home-manager.users.william.home;
		};
	};
}
