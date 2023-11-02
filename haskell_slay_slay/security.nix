{ lib, inputs, config, pkgs, ... }:

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
        services.swaylock = {
            fprintAuth = true;
        };
    };
}
