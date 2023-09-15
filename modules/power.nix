{ lib, inputs, config, pkgs, ... }:

{
    powerManagement.enable = true;
    services = {
        thermald.enable = true;
        auto-cpufreq = {
            enable = true;
            settings = {
                battery = {
                    governor = "powersave";
                    turbo = "never";
                };
                charger = {
                    governor = "performance";
                    turbo = "auto";
                };
            };
        };
    };
}
