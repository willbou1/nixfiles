{pkgs,...}:

# This file is a home-manager module injected into all graphical nodes
{
  # OOP
  home = {
    persistence."/persist/home/william".directories = [
      "eclipse-workspace"
      ".config/JetBrains"
    ];
    packages = with pkgs; [
      (symlinkJoin {
        name = "x11-eclipse";
        paths = [eclipses.eclipse-java];
        buildInputs = [makeWrapper];
        postBuild = ''
          wrapProgram $out/bin/eclipse --set GDK_BACKEND x11
        '';
      })
      jetbrains.idea
      jetbrains.dataspell
      jetbrains.pycharm
      jetbrains-toolbox
      jetbrains.rider

      unityhub
    ];
  };

  # Statistics
}
