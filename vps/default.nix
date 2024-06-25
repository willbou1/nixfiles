{ lib, ... }:

{
    imports = lib.mine.autoInclude ./. [];

    networking = {
        hostName = "ourmiraculous.com";
        interfaces.ens18 = {
            useDHCP = false;
            ipv4 = {
                addresses = [
                    {
                        address = "185.146.232.69";
                        prefixLength = 24;
                    }
                ];
                routes = [
                    {
                        address = "0.0.0.0";
                        prefixLength = 0;
                        via = "185.146.232.1";
                    }
                ];
            };
        };
    };
}
