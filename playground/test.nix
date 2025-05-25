let
  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;
in
with lib;
with builtins;
let 
  permissions = {
    notification = "notifications";
    clipboard = "clipboard";
    protocol = "protocol";
    call = "call";
  };

  siteSettings = ss: listToAttrs (map (s: with permissions; let
    toMerge = map (p: if p == notification then { content.notifications.enabled = true; }
                      else if p == clipboard then { content.javascript.clipboard = true; }
                      else if p == protocol then { content.register_protocol_handler = true; }
                      else if p == call then
                        { content = { media.audio_video_capture = true; desktop_capture = true; }; }
                      else error "Unrecognized permission for Qutebrowser") s.permissions;
    in {
      name = "https://${s.domain}";
      value = mkMerge (toMerge ++ [{ colors.webpage.darkmode.enabled = !s.light or true; }]);
    }) ss);

    domainSettings = with permissions; siteSettings [
      { domain = "teams.microsoft.com";
        permissions = [notification clipboard call]; light = true; }
      { domain = "www.facebook.com"; permissions = [notification]; }
      { domain = "www.netflix.com"; permissions = [notification]; }
      { domain = "github.com"; permissions = [clipboard]; }
      { domain = "mail.google.com"; permissions = [protocol]; light = true; }
    ];
    output = { inherit permissions domainSettings; };
in deepSeq output output
