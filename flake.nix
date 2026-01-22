{
  description = "All my NixOS systems including my tower, my laptop and my VPS";

  inputs = {
    # Common dependencies
    nixpkgs.url = ./devel/nixpkgs;
    unstable.url = ./devel/nixpkgs-unstable;
    flake-compat.url = "github:edolstra/flake-compat";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
    systems.url = "github:nix-systems/default";

    sops-nix = {
      url = ./devel/sops-nix;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    impermanence.url = ./devel/impermanence;

    home-manager = {
      url = ./devel/home-manager;
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = ./devel/nixvim;
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
        systems.follows = "systems";
        nuschtosSearch.inputs.flake-utils.follows = "flake-utils";
      };
    };

    hosts = {
      url = "github:StevenBlack/hosts";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    spicetify-nix = {
      url = ./devel/spicetify-nix;
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    #notnft.url = github:chayleaf/notnft;

    neovim-nightly-overlay = {
      url = ./devel/neovim-nightly-overlay;
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
        hercules-ci-effects.inputs.flake-parts.follows = "neovim-nightly-overlay/flake-parts";
      };
    };

    zen-browser = {
      url = "github:MarceColl/zen-browser-flake";
      inputs.nixpkgs.follows = "unstable";
    };

    emacs-overlay = {
      url = ./devel/emacs-overlay;
      inputs.nixpkgs-stable.follows = "nixpkgs";
      inputs.nixpkgs.follows = "unstable";
    };

    stylix = {
      url = ./devel/stylix;
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
        systems.follows = "systems";
        nur.follows = "nur";
      };
    };

    nur = {
      url = "github:nix-community/NUR";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };

    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "flake-compat";
      };
    };
  };

  outputs = {
    self,
    nixpkgs,
    home-manager,
    ...
  } @ inputs: let
    settings = import ./settings.nix;
    mkLib = nixpkgs:
      nixpkgs.lib.extend
      (self: super: {mine = import ./lib {lib = self;};});
    lib = mkLib inputs.nixpkgs;
    bleedingEdgeOverlay = final: prev:
      builtins.listToAttrs (builtins.map
        (p: {
          name = p;
          value = final.unstable."${p}";
        })
        settings.bleedingEdgePackages);
    nixpkgsPRsToPatches = map (p: {
      url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/${toString p.id}.diff";
      inherit (p) sha256;
    });
    patchNixpkgs = name: patches: unpatched:
      if patches == [] then unpatched
      else let
        tools = unpatched.legacyPackages."x86_64-linux";
      in
        tools.applyPatches {
          inherit name;
          src = unpatched;
          patches = map tools.fetchpatch patches;
        };
    unstable = patchNixpkgs "nixpkgs-unstable-patched" (nixpkgsPRsToPatches settings.unstableNixpkgsPRs) inputs.unstable;
    commonNixosModules = [
      {
        nixpkgs.overlays = [
          (import ./pkgs).overlay
          (import ./pkgs).nurOverlay
          inputs.nur.overlays.default
          inputs.neovim-nightly-overlay.overlays.default
          inputs.emacs-overlay.overlays.default
          inputs.nix-alien.overlays.default
          (final: prev: {
            zen-browser = inputs.zen-browser.packages.${final.system}.default;
            unstable = import unstable {
              inherit (final) system config;
              overlays = [
                (import ./pkgs).overlay
                (import ./pkgs).nurOverlay
              ];
            };
          })
          bleedingEdgeOverlay
        ];
      }
      inputs.nur.modules.nixos.default
      inputs.stylix.nixosModules.stylix
      inputs.sops-nix.nixosModules.sops
      inputs.hosts.nixosModule
      #inputs.notnft.nixosModules.default
      ./common
      home-manager.nixosModules.home-manager
    ];
  in rec {
    formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.alejandra;
    nixosConfigurations = {
      haskell_slay_slay = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {
          inherit inputs;
          inherit lib;
          inherit unstable;
        };
        modules = [./graphical ./haskell_slay_slay] ++ commonNixosModules;
      };
      linux-amd = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {
          inherit inputs;
          inherit lib;
          inherit unstable;
        };
        modules = [./graphical ./linux-amd] ++ commonNixosModules;
      };
      vps = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = {
          inherit inputs;
          inherit lib;
          inherit unstable;
        };
        modules = [./vps] ++ commonNixosModules;
      };
    };
    homeConfigurations = {
      "william@haskell_slay_slay" = nixosConfigurations.haskell_slay_slay.config.home-manager.users.william.home;
      "william@linux-amd" = nixosConfigurations.linux-amd.config.home-manager.users.william.home;
      "william@vps" = nixosConfigurations.vps.config.home-manager.users.william.home;
    };

    # Little "hack" to re-export all my inputs for use with nix shell
    packages.x86_64-linux = {
      nixpkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
      unstable = inputs.unstable.legacyPackages.x86_64-linux;
    };
  };
}
