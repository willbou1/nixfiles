{lib, ...}:
with lib; {
  services.llama-cpp = {
    enable = true;
    host = "*";
    port = 8080;
    model = "/data/models/Meta-Llama-3.1-70B-Instruct/ggml-model-f16.gguf";
    openFirewall = true;
  };
  systemd.services.llama-cpp = {
    wantedBy = mkForce [];
    serviceConfig.ReadWritePaths = [/srv/models];
  };
}
