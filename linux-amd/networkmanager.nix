{ ... }:

{
    environment = {
        etc."NetworkManager/system-connections/screen1.nmconnection".text = ''
            [connection]
            id=screen1
            uuid=073a8f9a-fdbd-3377-97ab-9a9b491216ad
            type=ethernet
            autoconnect-priority=-999
            interface-name=enp18s0f3u2u2

            [ethernet]

            [ipv4]
            method=shared

            [ipv6]
            addr-gen-mode=default
            method=auto

            [proxy]

        '';
    };
}
