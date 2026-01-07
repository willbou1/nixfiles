{
  fetchurl,
  stdenv,
  buildFHSEnv,
  writeShellScriptBin,
  callPackage,
  makeDesktopItem,
  copyDesktopItems,
  # Dependencies
  ffmpeg,
  glibc,
  jq,
  lib,
  libmediainfo,
  qt6,
  libusb1,
  eudev,
  openssl,
  ocl-icd,
  p7zip,
  patchelf,
  socat,
  vapoursynth,
  xdg-utils,
  xorg,
  zenity,
  # MPV dependencies
  mpv-unwrapped,
  customMpv ? null,
}:
################################################################################
# Based on svp package from AUR:
# https://aur.archlinux.org/packages/svp
################################################################################
let
  sources = rec {
    version = "4.7.305";
    cdnVersion = "4.7.0.305-4";
    src = fetchurl {
      url = "https://www.svp-team.com/files/svp4-linux.${version}.tar.bz2";
      hash = "sha256-PWAcm/hIA4JH2QtJPP+gSJdJLRdfdbZXIVdWELazbxQ=";
    };
    libs = fetchurl {
      url = "http://cdn.svp-team.com/repo/full-lin64/core.full/${cdnVersion}libs.7z";
      sha256 = "1xrv19gw455zc1ibbvx0gkdjz0wkrfsfwxv8wbydwhmxyxz6iv8a";
    };
    licenses = fetchurl {
      url = "http://cdn.svp-team.com/repo/full-lin64/core.full/${cdnVersion}licenses.7z";
      hash = "sha256-Xf9/UjUqMMLPSBbRVs6AIoHApuIOHfR0vad5QsEgKyE=";
    };
    content = fetchurl {
      url = "http://cdn.svp-team.com/repo/full-lin64/core.full/${cdnVersion}content.7z";
      hash = "sha256-YK8POShqnts3vapeyALl/Wh0Bg15/0mIDfHDYL9lpAE=";
    };
  };

  mpvForSVP = callPackage ./mpv.nix {inherit mpv-unwrapped;};

  # Script provided by GitHub user @xrun1
  # https://github.com/xddxdd/nur-packages/issues/31#issuecomment-1812591688
  fakeLsof = writeShellScriptBin "lsof" ''
    for arg in "$@"; do
      if [ -S "$arg" ]; then
        printf %s p
        echo '{"command": ["get_property", "pid"]}' |
          ${socat}/bin/socat - "UNIX-CONNECT:$arg" |
          ${jq}/bin/jq -Mr .data
        printf '\n'
      fi
    done
  '';

  libraries = [
    fakeLsof
    ffmpeg.bin
    glibc
    zenity
    libmediainfo
    qt6.qtbase
    qt6.qtwayland
    qt6.qtdeclarative
    qt6.qtsvg
    eudev
    libusb1
    (
      if (customMpv != null)
      then customMpv
      else mpvForSVP
    )
    ocl-icd
    stdenv.cc.cc.lib
    vapoursynth
    xdg-utils
    xorg.libX11
    openssl
  ];

  svp-dist = stdenv.mkDerivation rec {
    pname = "svp-dist";
    inherit (sources) version src libs licenses content;

    nativeBuildInputs = [
      p7zip
      patchelf
    ];
    dontFixup = true;

    unpackPhase = ''
      tar xf $src
      mkdir -p svp_updates
      7z x $libs -osvp_updates
      7z x $licenses -osvp_updates
      7z x $content -osvp_updates
    '';

    buildPhase = ''
      mkdir installer
      LANG=C grep --only-matching --byte-offset --binary --text  $'7z\xBC\xAF\x27\x1C' "svp4-linux.run" |
        cut -f1 -d: |
        while read ofs; do dd if="svp4-linux.run" bs=1M iflag=skip_bytes status=none skip=$ofs of="installer/bin-$ofs.7z"; done
    '';

    installPhase = ''
      mkdir -p $out/opt
      for f in "installer/"*.7z; do
        7z -bd -bb0 -y x -o"$out/opt/" "$f" || true
      done

      for SIZE in 32 48 64 128; do
        install -Dm644 "$out/opt/svp-manager4-''${SIZE}.png" "$out/share/icons/hicolor/''${SIZE}x''${SIZE}/apps/svp-manager4.png"
      done
      rm -f $out/opt/{add,remove}-menuitem.sh

      cp -a "svp_updates/"* "$out/opt/"
    '';
  };

  fhs = buildFHSEnv {
    name = "SVPManager";
    targetPkgs = _pkgs: libraries;
    runScript = "${svp-dist}/opt/SVPManager";
    unshareUser = false;
    unshareIpc = false;
    unsharePid = false;
    unshareNet = false;
    unshareUts = false;
    unshareCgroup = false;
  };
in
  stdenv.mkDerivation {
    pname = "svp";
    inherit (sources) version src;

    dontUnpack = true;

    nativeBuildInputs = [copyDesktopItems];

    postInstall = ''
      install -Dm755 ${fhs}/bin/SVPManager $out/bin/SVPManager

      mkdir -p $out/share
      ln -s ${svp-dist}/share/icons $out/share/icons
    '';

    passthru.mpv = mpvForSVP;

    desktopItems = [
      (makeDesktopItem {
        name = "svp-manager4";
        exec = "${fhs}/bin/SVPManager %f";
        desktopName = "SVP 4 Linux";
        genericName = "Real time frame interpolation";
        icon = "svp-manager4";
        categories = [
          "AudioVideo"
          "Player"
          "Video"
        ];
        mimeTypes = [
          "video/x-msvideo"
          "video/x-matroska"
          "video/webm"
          "video/mpeg"
          "video/mp4"
        ];
        terminal = false;
        startupNotify = true;
      })
    ];

    meta = {
      mainProgram = "SVPManager";
      maintainers = with lib.maintainers; [xddxdd];
      description = "SmoothVideo Project 4 (SVP4) converts any video to 60 fps (and even higher) and performs this in real time right in your favorite video player";
      homepage = "https://www.svp-team.com/wiki/SVP:Linux";
      platforms = ["x86_64-linux"];
      license = lib.licenses.unfree;
      sourceProvenance = with lib.sourceTypes; [binaryNativeCode];
    };
  }
