{pkgs,...}:

# This file is a home-manager module injected into all graphical nodes
{
  # OOP
  home = {
    persistence."/persist".directories = [
      ".config/JetBrains"
    ];
    packages = with pkgs; [
      jetbrains.idea
      jetbrains.clion
      jetbrains.dataspell
      jetbrains.pycharm
      jetbrains-toolbox
      jetbrains.rider

      unityhub
    ];
  };

  # Statistics
}
