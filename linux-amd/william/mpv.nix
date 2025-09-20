{pkgs, ...}: {
  programs.mpv = {
    config = {
      glsl-shaders = "~~/shaders/ai_upscale/Photo/2x/AiUpscale_HQ_Sharp_2x_Photo.glsl";
      sub-font-size = 65;
    };
    bindings = {
      "CTRL+1" = "no-osd change-list glsl-shaders set \"~~/shaders/Photo/2x/AiUpscale_HQ_Sharp_2x_Photo.glsl\"; show-text \"AiUpscale: 2x HQ Sharp\"";
      "CTRL+2" = "no-osd change-list glsl-shaders set \"~~/shaders/Photo/2x/AiUpscale_HQ_2x_Photo.glsl\"; show-text \"AiUpscale: 2x HQ\"";
      "CTRL+3" = "no-osd change-list glsl-shaders set \"~~/shaders/Photo/4x/AiUpscale_HQ_Sharp_4x_Photo.glsl\"; show-text \"AiUpscale: 4x HQ Sharp\"";
      "CTRL+4" = "no-osd change-list glsl-shaders set \"~~/shaders/Photo/4x/AiUpscale_HQ_4x_Photo.glsl\"; show-text \"AiUpscale: 4x HQ\"";
    };
  };
}
