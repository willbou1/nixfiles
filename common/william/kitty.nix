{ lib, pkgs, ... }:

{
    home.terminal = "${pkgs.kitty}/bin/kitty";
    programs.kitty = {
        enable = true;
        keybindings = {
            "ctrl+plus" = "change_font_size all +2.0";
            "ctrl+minus" = "change_font_size all -2.0";
        };
        settings = {
            confirm_os_window_close = 0;
            disable_ligatures = "never";
            font_features = "FiraCode-Retina +liga +zero +onum";
            use_system_wcwidth = false;
            window_padding_width = 10;
            tab_bar_edge = "bottom";
            tab_bar_align = "center";
            open_url_with = "qutebrowser";
            detect_urls = true;
            paste_actions = "quote-urls-at-prompt";
            strip_trailing_spaces = "always";
            input_delay = 0;
            sync_to_monitor = true;
            cursor_shape = "beam";
            enable_audio_bell = false;
            visual_bell_duration = "0.2";
        };
    };
}
