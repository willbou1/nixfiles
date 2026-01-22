{
  wineWowPackages,
  msitools,
  icoutils,
  imagemagick,
  makeDesktopItem,
  copyDesktopItems,
  lib,
  stdenv,
  fetchurl,
  winePrefix ? "\$HOME/.local/share/ltspice/wine",
  ...
}:

let
start-script = ''
  #!${stdenv.shell}
  export WINEPREFIX=$winePrefix
  export WINEARCH=win64
  export WINEDLLOVERRIDES="winemenubuilder.exe=d;mscoree=d;mshtml=d"

  WINE="${wineWowPackages.stable}/bin/wine"

  LTSPICE_EXE="\$WINEPREFIX/drive_c/Program Files/ADI/LTspice/LTspice.exe"
  INSTALLER="$src"

  if [ ! -f "\$LTSPICE_EXE" ]; then
    echo "Running installer..."
    "\$WINE" "\$INSTALLER"
  fi

  exec "\$WINE" "\$LTSPICE_EXE" "\$@"
'';
in stdenv.mkDerivation rec {
  inherit winePrefix;

  pname = "ltspice";
  version = "26.0.1";

  src = fetchurl {
    url = "https://LTspice.analog.com/download/26.0.1/LTspice64.msi";
    sha256 = "1080xi3qr059j5j34ribchgn6yw7r2iy6yxiaypd4cpdjxk04dgc";
  };
  dontUnpack = true;

  nativeBuildInputs = [
    msitools
    icoutils
    imagemagick
    copyDesktopItems
  ];

  installPhase = ''
    runHook preInstall

    msiextract $src

    mkdir -p "$out/bin" "$out/share/pixmaps"
    
    find . -name "LTspice.exe" -exec wrestool -x -t 14 "{}" \; -quit \
      | magick ico:- -thumbnail 256x256 "$out/share/pixmaps/ltspice.png"
    
    cat > $out/bin/ltspice <<EOF
    ${start-script}
    EOF

    chmod +x $out/bin/ltspice

    runHook postInstall
  '';

  desktopItems = [
    (makeDesktopItem {
      name = "ltspice";
      exec = "ltspice  %f";
      desktopName = "LTspice ${version}";
      genericName = "SPICE-based analog simulation program";
      icon = "ltspice";
      categories = [
        "Development"
        "Electronics"
      ];
      terminal = false;
      startupNotify = true;
    })
  ];

  meta = {
    mainProgram = "LTspice";
    description = "LTspice is a powerful, fast, and free SPICE simulator software, schematic capture and waveform viewer with enhancements and models for improving the simulation of analog circuits.";
    homepage = "https://www.analog.com/en/resources/design-tools-and-calculators/ltspice-simulator.html";
    platforms = ["x86_64-linux"];
    license = lib.licenses.unfree;
    sourceProvenance = with lib.sourceTypes; [binaryNativeCode];
  };
}
