{ pkgs, lib, ... }:
with lib;
let
  kicadThemes = pkgs.fetchFromGitHub {
    owner = "pointhi";
    repo = "kicad-color-schemes";
    rev = "68ea0402f334bdbae175f6ca924743640c07183d";
    hash = "sha256-PYgFOyK5MyDE1vTkz5jGnPWAz0pwo6Khu91ANgJ2OO4=";
  };
  kicadVersion = "${versions.major pkgs.kicad.version}.0";
in {
  home.packages = with pkgs; [
    kicad
    ngspice

    # lab
    novnc
  ];

  home.persistence."/persist/home/william" = {
    files = [
      ".ngspice_history"
    ];
    directories = [
      ".config/kicad"
    ];
  };

  xdg.configFile."kicad/${kicadVersion}/colors/wdark.json".source = "${kicadThemes}/wdark/wdark.json";
}
