{
  wineWowPackages,
  icoutils,
  imagemagick,
  unzip,
  makeDesktopItem,
  copyDesktopItems,
  lib,
  stdenv,
  requireFile,
  winePrefix ? "\$HOME/.local/share/tina-ti/wine",
  ...
}:

let
start-script = ''
  #!${stdenv.shell}
  export WINEPREFIX=$winePrefix
  export WINEARCH=win64
  export WINEDLLOVERRIDES="winemenubuilder.exe=d;mscoree=d;mshtml=d"

  WINE="${wineWowPackages.stable}/bin/wine"

  TINA_EXE="\$WINEPREFIX/drive_c/Program Files (x86)/DesignSoft/Tina 9 - TI/TINA.EXE"
  INSTALLER="$out/share/tina-ti/Tina90-TIen.exe"

  if [ ! -f "\$TINA_EXE" ]; then
    echo "Running installer..."
    "\$WINE" "\$INSTALLER"
  fi

  exec "\$WINE" "\$TINA_EXE" "\$@"
'';
in stdenv.mkDerivation rec {
  inherit winePrefix;

  pname = "TINA-TI";
  version = "9.3.200.277";

  src = requireFile rec {
    name = "Tina90-TIen.9.3.200.277.zip";
    sha256 = "16i5yy43pg23vqjzc8xqllka31vysi20siwyj95by4hawais1s93";
    message = ''
      As Texas Instruments are not big into FOSS, the zip for TINA-TI's installer must be provided manually.
      Add it to the Nix store with:
        nix-store --add-fixed sha256 ${name}
    '';
  };
  dontUnpack = true;

  nativeBuildInputs = [
    icoutils
    imagemagick
    unzip
    copyDesktopItems
  ];

  installPhase = ''
    runHook preInstall

    unzip $src

    mkdir -p "$out/bin" "$out/share/pixmaps" $out/share/tina-ti
    
    wrestool -x -t 14 Tina90-TIen.exe \
      | magick ico:- -thumbnail 256x256 "$out/share/pixmaps/tina-ti.png"
    
    cp Tina90-TIen.exe $out/share/tina-ti/
    
    cat > $out/bin/tina-ti <<EOF
    ${start-script}
    EOF

    chmod +x $out/bin/tina-ti

    runHook postInstall
  '';

  desktopItems = [
    (makeDesktopItem {
      name = "TINA-TI";
      exec = "tina-ti  %f";
      desktopName = "TINA-TI";
      genericName = "SPICE-based analog simulation program";
      icon = "tina-ti";
      categories = [
        "Development"
        "Electronics"
      ];
      terminal = false;
      startupNotify = true;
    })
  ];

  meta = {
    mainProgram = "TINA-TI";
    description = "SPICE-based analog simulation program";
    homepage = "https://www.ti.com/tool/TINA-TI";
    platforms = ["x86_64-linux"];
    license = lib.licenses.unfree;
    sourceProvenance = with lib.sourceTypes; [binaryNativeCode];
  };
}
