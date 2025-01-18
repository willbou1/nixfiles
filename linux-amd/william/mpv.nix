{pkgs, ...}: {
  programs.mpv = {
    config = {
      glsl-shaders = "~~/shaders/ai_upscale/Photo/2x/AiUpscale_HQ_Sharp_2x_Photo.glsl";
      sub-font-size = 75;
    };
    bindings = {
      "CTRL+1" = "no-osd change-list glsl-shaders set \"~~/shaders/Photo/2x/AiUpscale_HQ_Sharp_2x_Photo.glsl\"; show-text \"AiUpscale: 2x HQ Sharp\"";
      "CTRL+2" = "no-osd change-list glsl-shaders set \"~~/shaders/Photo/2x/AiUpscale_HQ_2x_Photo.glsl\"; show-text \"AiUpscale: 2x HQ\"";
      "CTRL+3" = "no-osd change-list glsl-shaders set \"~~/shaders/Photo/4x/AiUpscale_HQ_Sharp_4x_Photo.glsl\"; show-text \"AiUpscale: 4x HQ Sharp\"";
      "CTRL+4" = "no-osd change-list glsl-shaders set \"~~/shaders/Photo/4x/AiUpscale_HQ_4x_Photo.glsl\"; show-text \"AiUpscale: 4x HQ\"";
    };
  };
  xdg.configFile = {
    "mpv/shaders/ai_upscale".source =
      pkgs.fetchFromGitHub {
        owner = "Alexkral";
        repo = "AviSynthAiUpscale";
        rev = "d04cf8154e4ba9914f3ead0dec9f7a4a7df7369f";
        hash = "sha256-MKW7y/I+LDAsOU3ROVN/ySrTYJ0YMN+P/oBB7x6HXys=";
      }
      + "/mpv user shaders";
  };
}
