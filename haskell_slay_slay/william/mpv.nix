{
  pkgs,
  config,
  ...
}: let
  nmpv = pkgs.writeShellScriptBin "nmpv" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    ${config.programs.mpv.package}/bin/mpv "$@"
  '';
in {
  home.packages = [nmpv];
  programs.mpv.config = {
    panscan = 1.0;
    sub-font-size = 45;
    #glsl-shaders = "~/.config/mpv/shaders/film/FSRCNNX_x2_8-0-4-1.glsl:~/.config/mpv/shaders/film/SSimDownscaler.glsl:~/.config/mpv/shaders/film/KrigBilateral.glsl";
  };
  xdg.configFile = {
    "mpv/shaders/film".source =
      pkgs.fetchFromGitHub {
        owner = "classicjazz";
        repo = "mpv-config";
        rev = "90eebbbc8e1edaab233cafb2412d644cb4c7877d";
        hash = "sha256-ReXiDCAsWNWBrk48VyNWqSoR2Ic09Zw/TPvWRcG8xvA=";
      }
      + "/shaders";
  };
}
