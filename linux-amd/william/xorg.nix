{ lib, ... }:

{
    home.monitors = [
        {
            name = "HDMI-A-0";
            width = 3440;
            height = 1440;
            rate = 100;
            x = 0;
            y = 0;
            hScale = 1;
            vScale = 1;
        }
        {
            name = "DisplayPort-0";
            width = 3440;
            height = 1440;
            rate = 100;
            x = 3440;
            y = 0;
            hScale = 1;
            vScale = 1;
        }
    ];
