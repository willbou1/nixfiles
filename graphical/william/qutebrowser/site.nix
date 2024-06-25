{ ... }:

{
#    programs.qutebrowser.domainSettings = {
#        "https://teams.microsoft.com".content.notifications.enable = true;
#        "https://www.netflix.com".content.notifications.enable = false;
#        "https://www.facebook.com".content.notifications.enable = false;
#        "https://mail.google.com".content.register_protocol_handler = true;
#    };
    programs.qutebrowser.extraConfig = ''
        # Notifications
        config.set("content.notifications.enabled", True, "https://teams.microsoft.com")
        config.set("content.notifications.enabled", False, "https://www.netflix.com")
        config.set("content.notifications.enabled", False, "https://www.facebook.com/")

        config.set("content.register_protocol_handler", True, "https://mail.google.com")
    '';
}
