{ ... }:

{
    services.llama-cpp = {
        enable = true;
        host = "localhost";
        port = 8080;
        model = "/srv/models/Meta-Llama-3.1-8B-Instruct.gguf";
    };
}
