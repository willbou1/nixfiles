{ config, ... }:

{
    services.dunst = {
        enable = true;
        settings = {
            global = {
                origin = "top-right";
                width = "(0,700)";
                height = 300;
                notification_limit = 3;
                padding = 16;
                horizontal_padding = 24;
                frame_width = config.home.borderSize;
                transparency = 30;
                corner_radius = 20;
                follow = "none";
                monitor = 0;
                browser = "qutebrowser";
                markup = "full";
            };
        };
    };
}
