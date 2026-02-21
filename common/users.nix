{pkgs, ...}: {
  users = {
    mutableUsers = false;
    users.root.hashedPassword = "$y$j9T$PY3O4ZGgd.CdIq.EkwuIV/$WYwblb5ck3iX3Y76DFCFJDbdfRRh7xF9QTB3tc6tb31";
    users.william = {
      isNormalUser = true;
      description = "william";
      extraGroups = ["wheel"];
      shell = pkgs.fish;
      hashedPassword = "$y$j9T$fF72zVZ48p/SPowZ83vF//$gpvRfCo3Qr04rkRwsZh.p3kOkOflmhQBHQM8kr6ee98";
      openssh.authorizedKeys.keys = ["ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICJ2b2UtfnyyWsNKR96dUK6l1iVaEUc1uTEf8X8VBZeC willbou2@gmail.com"];
    };
  };
}
