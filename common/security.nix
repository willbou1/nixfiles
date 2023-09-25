{ lib, inputs, config, pkgs, ... }:

{
    sops = {
        defaultSopsFile = ../secrets.yaml;
        age.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_ed25519_key" ];
        gnupg.sshKeyPaths = [ "/persist/etc/ssh/ssh_host_rsa_key" ];
    };

    environment.persistence."/persist".directories = [
        "/etc/ssh"
        "/var/lib/fprint"
    ];

    programs = {
        gnupg.agent = {
            enable = true;
            pinentryFlavor = "curses";
            enableSSHSupport = true;
        };
    };

    services = {
        printing.enable = true;
        fprintd = {
            enable = true;
        };
    };

    security.pam = {
        services.swaylock = {
            fprintAuth = true;
            unixAuth = true;
        };
        loginLimits = [
            {
                domain = "*";
                type = "soft";
                item = "nproc";
                value = "1000";
            }
            {
                domain = "*";
                type = "hard";
                item = "nproc";
                value = "2000";
            }
            {
                domain = "william";
                type = "soft";
                item = "nproc";
                value = "3000";
            }
            {
                domain = "william";
                type = "hard";
                item = "nproc";
                value = "4000";
            }
            {
                domain = "william";
                type = "soft";
                item = "nofile";
                value = "8000";
            }
            {
                domain = "william";
                type = "hard";
                item = "nofile";
                value = "10000";
            }
        ];
    };
}
