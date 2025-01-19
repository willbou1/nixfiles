{
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
      account required pam_unix.so # unix (order 10900)

      # Authentication management.
      auth sufficient pam_unix.so likeauth try_first_pass # unix (order 11500)
      auth sufficient /nix/store/z9i7vy9lr4srafd38bfqclqxxs6dr9ks-fprintd-1.94.2/lib/security/pam_fprintd.so # fprintd (order 11300)
      auth required pam_deny.so # deny (order 12300)

      # Password management.
      password sufficient pam_unix.so nullok yescrypt # unix (order 10200)

      # Session management.
      session required pam_env.so conffile=/etc/pam/environment readenv=0 # env (order 10100)
      session required pam_unix.so # unix (order 10200)
      session required /nix/store/l5syg3vsx9bfpnbqjraqjckcj3n1b2as-linux-pam-1.6.1/lib/security/pam_limits.so conf=/nix/store/qz3qz8n43rxqliyw36ikawsvlp8gppq6-limits.conf # limits (order 12200)
              };
    '';
  };
}
