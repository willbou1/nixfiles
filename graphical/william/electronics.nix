{ pkgs, ... }:

{
  home.packages = with pkgs; [
    kicad
    ngspice
  ];

  home.persistence."/persist/home/william" = {
    files = [
      ".ngspice_history"
    ];
    directories = [
      ".config/kicad"
    ];
  };
}
