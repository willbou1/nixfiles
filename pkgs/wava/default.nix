{
  wayland,
  wayland-protocols,
  wayland-scanner,
  cmake,
  gcc,
  egl-wayland,
  glew,
  pulseaudio,
  pipewire,
  fftw,
  fftwFloat,
  pkg-config,
  imagemagick,
  librsvg,
  stdenv,
  fetchgit,
}:
stdenv.mkDerivation {
  name = "wava";

  src = fetchgit {
    url = "https://github.com/Dominara1/wava";
    rev = "43710cbbd60b219325015196e36ace1b749ffd1f";
    hash = "sha256-JCZLkspqffwzmdkyRXHleciUrvKSe+880EDVUtTgCJw=";
    leaveDotGit = true;
  };

  buildInputs = [
    wayland
    wayland-protocols
    wayland-scanner
    cmake
    gcc
    egl-wayland
    glew
    pulseaudio
    pipewire
    fftw
    fftwFloat
    pkg-config
    imagemagick
    librsvg
  ];

  buildPhase = ''
    cmake .. -DCMAKE_SKIP_BUILD_RPATH=ON
    make -j $(nproc)
  '';

  installPhase = ''
    make install
  '';
}
