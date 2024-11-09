{
  programs.looking-glass-client = {
    enable = true;
    settings = {
      win.autoScreensaver = true;
      input = {
        escapeKey = "KEY_F2";
        rawMouse = true;
      };
      spice.alwaysShowCursor = true;
      audio.micDefault = "allow";
    };
  };
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
    "org/virt-manager/virt-manager/urls" = {
      isos = ["/home/william/priv/downloads/"];
    };
    "org/virt-manager/virt-manager/confirm" = {
      forcepoweroff = false;
    };
    "org/virt-manager/virt-manager" = {
      xmleditor-enabled = true;
    };
  };
}
