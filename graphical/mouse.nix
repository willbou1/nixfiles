{pkgs, ...}: {
  environment = {
    systemPackages = with pkgs; [
      logiops
    ];
    etc."logid.cfg".text = ''
      // What's working:
      //   1. Window snapping using Gesture button (Thumb)
      //   2. Forward Back Buttons
      //   3. Top button (Ratchet-Free wheel)
      // What's not working:
      //   1. Thumb scroll (H-scroll)
      //   2. Scroll button

      devices: ({
        name: "Wireless Mouse MX Master 3";

        buttons: (
          // Gesture button (hold and move)
          {
            cid: 0xc3;
            action = {
              type: "Gestures";
              gestures: (
                {
                  direction: "None";
                  mode: "OnThreshold";
                  threshold: "1";
                  action = {
                    type: "Keypress";
                    keys: [ "KEY_LEFTMETA", "BTN_LEFT" ]; // open activities overview
                  }
                }
              );
            };
          }
        );
      });
    '';
  };
}
