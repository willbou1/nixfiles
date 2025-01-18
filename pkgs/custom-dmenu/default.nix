{
  lib,
  stdenv,
  fetchFromGitHub,
  libX11,
  libXinerama,
  libXft,
  zlib,
  patches ? null,
  # update script dependencies
  gitUpdater,
}:
stdenv.mkDerivation rec {
  pname = "custom-dmenu";
  version = "5.3";

  src = fetchFromGitHub {
    owner = "willbou1";
    repo = "dmenu";
    rev = "78ff93302b4c7cbca7792dd55c49a5228e4751c1";
    hash = "sha256-naowy3DuodhLlfkvwK5iRak29X088uifowlXnOpYlJM=";
  };

  buildInputs = [
    libX11
    libXinerama
    zlib
    libXft
  ];

  inherit patches;

  postPatch = ''
    sed -ri -e 's!\<(dmenu|dmenu_path|stest)\>!'"$out/bin"'/&!g' dmenu_run
    sed -ri -e 's!\<stest\>!'"$out/bin"'/&!g' dmenu_path
  '';

  preConfigure = ''
    sed -i "s@PREFIX = /usr/local@PREFIX = $out@g" config.mk
  '';

  makeFlags = ["CC:=$(CC)"];

  passthru.updateScript = gitUpdater {
    url = "git://git.suckless.org/dmenu";
  };

  meta = with lib; {
    description = "A generic, highly customizable, and efficient menu for the X Window System";
    homepage = "https://tools.suckless.org/dmenu";
    license = licenses.mit;
    maintainers = with maintainers; [
      pSub
      globin
      qusic
    ];
    platforms = platforms.all;
    mainProgram = "custom-dmenu";
  };
}
