{
  description = "Data science on NixOS";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    devshell.url = "github:numtide/devshell";
  };

  outputs = { self, flake-utils, devshell, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system: {
      devShell = let
          pkgs = import nixpkgs {
              inherit system;
              overlays = [ devshell.overlays.default ];
          };
        in pkgs.devshell.mkShell rec {
            packages = with pkgs; [
              (python3.withPackages (ps: with ps; [
                jupyter
                ipython
                zeroconf

                scipy
                numpy
                matplotlib
                sympy

                pytorch
              ]))
            ];
        };
    });
}
