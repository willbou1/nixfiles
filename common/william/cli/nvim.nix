{ config, inputs, pkgs, ... }: let
themeFile = config.lib.stylix.colors {
    templateRepo = pkgs.fetchFromGitHub {
        owner = "chriskempson";
        repo = "base16-vim";
        rev = "6191622d5806d4448fa2285047936bdcee57a098";
        sha256 = "6FsT87qcl9GBxgxrPx2bPULIMA/O8TRxHaN49qMM4uM=";
    };
};
themePlugin = pkgs.vimUtils.buildVimPlugin {
    name = "stylix";
    pname = "stylix";

    src = themeFile;
    dontUnpack = true;

    buildPhase = ''
        install -D $src $out/colors/base16-stylix.vim
        '';
};
in with config.lib.stylix.colors.withHashtag; {
    programs.nixvim = {
        enable = true;
        colorscheme = "base16-stylix";
        package = pkgs.neovim-nightly;
        plugins = {
            lualine = {
                enable = true;
                theme = {
                    normal = {
                        a.fg = base00;
                        a.bg = base04;
                        b.fg = base07;
                        b.bg = base01;
                        c.fg = base08;
                        c.bg = base00;
                    };
                    insert.a = {
                        fg = base00;
                        bg = base08;
                    };
                    visual.a = {
                        fg = base00;
                        bg = base02;
                    };
                    replace.a = {
                        fg = base00;
                        bg = base08;
                    };
                    inactive = {
                        a.fg = base08;
                        a.bg = base00;
                        b.fg = base08;
                        b.bg = base00;
                        c.fg = base00;
                        c.bg = base00;
                    };
                };
            };
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
                    cssls.enable = true;
                    omnisharp.enable = true;
                };
            };
            lspkind.enable = true;
            null-ls = {
                enable = true;
                sources.formatting.nixfmt.enable = true;
            };
        };
        extraPlugins = with pkgs.vimPlugins; [
            firenvim
                vim-tpipeline
                themePlugin
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
