{pkgs, ...}: let
  dmenu = pkgs.writeShellScript "dmenu.sh" ''
    if [[ $XDG_SESSION_TYPE == "wayland" ]]; then
        if [[ $1 == "password" ]]; then
            ${pkgs.wofi}/bin/wofi --dmenu --password --lines=1
        else
            ${pkgs.wofi}/bin/wofi --dmenu
        fi
    else
        if [[ $1 == "password" ]]; then
            ${pkgs.rofi}/bin/wofi -dmenu -password
        else
            ${pkgs.rofi}/bin/wofi -dmenu
        fi
    fi
  '';
in {
  home.packages = with pkgs; [
    bitwarden-desktop
    bitwarden-cli
    keyutils
  ];
  home.persistence."/persist/home/william" = {
    directories = [
      ".config/Bitwarden"
    ];
    files = [
      ".config/Bitwarden CLI/data.json"
    ];
  };
  # Fix a bug with keyutils
  home.activation.keyutils = "${pkgs.keyutils}/bin/keyctl link @u @s";

  programs.qutebrowser.keyBindings = {
    normal = {
      ",B" = "open https://vault.ourmiraculous.com";
      ",b" = "spawn --userscript qute-bitwarden --dmenu-invocation '${dmenu}' --password-prompt-invocation '${dmenu} password'";
    };
  };
}
