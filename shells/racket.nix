{
  description = "Racket development on NixOS";

  inputs = {
    devshell.url = "github:numtide/devshell";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, devshell, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system: {
      devShell = let
          pkgs = import nixpkgs {
              inherit system;
              overlays = [ devshell.overlays.default ];
          };
        in pkgs.devshell.mkShell rec {
            devshell = {
              packages = with pkgs; [
                racket
              ];
              startup.installRacketPackages.text = ''
                raco pkg install --skip-installed racket-langserver
              '';
            };
        };
    });
}
