{ config, inputs, pkgs, ... }: let
in with config.lib.stylix.colors.withHashtag; {
    programs.nixvim = {
        enable = true;
        package = pkgs.neovim-nightly;
        plugins = {
            #rainbow-delimiters.enable = true;
            lualine.enable = true;
            barbar = {
                enable = true;
                autoHide = true;
            };
            nvim-colorizer.enable = true;
            undotree.enable = true;
            treesitter.enable = true;
            telescope = {
                enable = true;
                extensions = {
                    file_browser.enable = true;
                    fzy-native.enable = true;
                };
                keymaps = {
                    "<leader>tf" = "find_files";
                    "<leader>tb" = "buffers";
                };
            };
            dap = {
                enable = true;
                extensions.dap-ui.enable = true;
                adapters.executables = {
                    cpptools.command = "${pkgs.vscode-extensions.ms-vscode.cpptools}/share/vscode/extensions/ms-vscode.cpptools/debugAdapters/bin/OpenDebugAD7";
                };
            };
            nvim-cmp = {
                enable = true;
                sources = [
                { name = "nvim_lsp"; }
                { name = "path"; }
                { name = "buffer"; }
                { name = "cmdline"; }
                { name = "vsnip"; }
                ];
                mappingPresets = [ "insert" ];
                mapping = {
                    "<CR>" = "cmp.mapping.confirm({ select = false })";
                };
            };
            lsp = {
                enable = true;
                servers = {
                    java-language-server.enable = true;
                    hls.enable = true;
                    texlab.enable = true;
                    cmake.enable = true;
                    bashls.enable = true;
                    ccls.enable = true;
                    rust-analyzer = {
                        enable = true;
                        installCargo = false;
                        installRustc = false;
                    };
                    cssls.enable = true;
                    omnisharp.enable = true;
                };
            };
            lspkind.enable = true;
            none-ls = {
                enable = true;
                sources.formatting.nixfmt.enable = true;
            };
        };
        extraPlugins = with pkgs.vimPlugins; [
            firenvim
            vim-tpipeline
            vim-gas
        ];
        options = {
            termguicolors = true;
            wildignore = "*.pyc,*_build/*,**/coverage/*,**/node_modules/*,**/android/*,**/ios/*,**/.git/*";
            number = true;
            relativenumber = true;
            cursorline = true;
            mouse = "ar";
            colorcolumn = "80";
            clipboard = "unnamedplus";
            fillchars = { vert = "\\"; };
            tabstop = 4;
            shiftwidth = 4;
            expandtab = true;
            splitbelow = true;
            splitright = true;
            spelllang = "en_ca";
            guifont = "Monospace:h18";
            undofile = true;
        };
        filetype.extension = {
            as = "asm";
        };
        globals.mapleader = " ";
        keymaps = [
            {
                key = "<C-h>";
                action = "<C-w>h";
            }
            {
                key = "<C-j>";
                action = "<C-w>j";
            }
            {
                key = "<C-k>";
                action = "<C-w>k";
            }
            {
                key = "<C-l>";
                action = "<C-w>l";
            }
        ];
        globals.tpipeline_restore = 1;
        # Firenvim
        globals.firenvim_config = {
            globalSettings.alt = "all";
            localSettings = {
                ".*" = {
                    cmdline = "nvim";
                    priority = 0;
                    selector = "textarea";
                    takeover = "never";
                };
                "https?://[^/]*youtu\\.?be[^/]*/" = {
                    selector = "#contenteditable-root";
                    takeover = "always";
                };
            };
        };
    };
}
