{
    programs.looking-glass-client = {
        enable = true;
        settings = {
            input = {
                escapeKey = "KEY_F2";
                rawMouse = true;
            };
            spice.alwaysShowCursor = true;
            audio.micDefault = "allow"; 
        };
    };
}
