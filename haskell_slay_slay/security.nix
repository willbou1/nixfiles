{pkgs, ...}: {
  environment.persistence."/persist".directories = [
    "/var/lib/fprint"
  ];

  services = {
    printing.enable = true;
    fprintd = {
      enable = true;
    };
  };

  security.pam = {
    services.swaylock.text = ''
      # Account management.
      account required ${pkgs.linux-pam}/lib/security/pam_unix.so

      # Authentication management.
      auth sufficient ${pkgs.linux-pam}/lib/security/pam_unix.so likeauth try_first_pass
      auth sufficient ${pkgs.fprintd}/lib/security/pam_fprintd.so
      auth required ${pkgs.linux-pam}/lib/security/pam_deny.so

      # Password management.
      password sufficient ${pkgs.linux-pam}/lib/security/pam_unix.so nullok yescrypt

      # Session management.
      session required ${pkgs.linux-pam}/lib/security/pam_env.so conffile=/etc/pam/environment readenv=0
      session required ${pkgs.linux-pam}/lib/security/pam_unix.so
      #session required ${pkgs.linux-pam}/lib/security/pam_limits.so conf=/nix/store/3jyf3k7majk3lbkzmv3i9bdb57ifnh9v-limits.conf
    '';
  };
}
