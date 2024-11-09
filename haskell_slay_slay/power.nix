{ lib, inputs, config, pkgs, ... }:

{
    powerManagement.enable = true;
    services = {
        logind = {
            powerKey = "suspend";
            extraConfig = ''
                IdleActionSec=1200
                IdleAction=ignore
            '';
        };
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
