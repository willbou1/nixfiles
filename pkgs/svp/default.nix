{
    lib,
    stdenv,
    fetchzip,
    p7zip,

    lsof,
    xdg-utils,
    libusb,
    vapoursynth,
    libmediainfo,
    libsForQt5
}: let
    qt5 = libsForQt5.qt5;
in stdenv.mkDerivation rec {
    pname = "svp";
    version = "4.5.210";

    meta = with lib; {
        description = "SmoothVideo Project 4 (SVP)";
        homepage = "https://www.svp-team.com/wiki/SVP:Linux";
        platforms = platforms.linux;
    };

    src = fetchzip {
        url = "https://www.svp-team.com/files/svp4-linux.${version}-2.tar.bz2";
        hash = "sha256-Hk3h8KzP4VNVDhRDqlhkniOQyxU5M17YWQ25uX3AzZA=";
    };

    buildInputs = [
        libmediainfo qt5.qtbase qt5.qtsvg
        qt5.qtscript qt5.qtdeclarative
        vapoursynth libusb xdg-utils lsof 
    ];
    nativeBuildInputs = [ qt5.wrapQtAppsHook ];

    buildPhase = ''
        mkdir installer
        LANG=C grep --only-matching --byte-offset --binary --text  $'7z\xBC\xAF\x27\x1C' "svp4-linux-64.run" | cut -f1 -d: | while read ofs; do dd if="svp4-linux-64.run" bs=1M iflag=skip_bytes status=none skip=$ofs of="installer/bin-$ofs.7z"; done
        for f in "installer/"*.7z; do
            ${p7zip}/bin/7z -bd -bb0 -y x -o"extracted/" "$f" || true
        done
    '';

    installPhase = ''
        mkdir -p "$out"/{opt/svp,usr/bin,usr/share/licenses/svp}
        if [[ -d "extracted/licenses" ]]; then
            mv "extracted/licenses" "$out/usr/share/licenses/${pname}"
        fi
        mv "extracted/"* "$out/opt/${pname}"
        # rm "$out/opt/$pkgname/extensions/libsvpcode.so" # previously this extension caused the whole thing to segfault. lmk if that's still the case
        ln -s "/opt/${pname}/SVPManager" "$out/usr/bin/SVPManager"
        chmod -R +rX "$out/opt/svp" "$out/usr/share"
    '';
}
