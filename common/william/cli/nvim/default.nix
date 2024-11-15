{
  config,
  inputs,
  pkgs,
  ...
}:
with builtins; let
in
  with config.lib.stylix.colors.withHashtag; {
    xdg.configFile."clangd/config.yaml".source = ./clangd.yaml;
    programs.nixvim = {
      enable = true;
      package = pkgs.neovim-unwrapped;
      plugins = {
        rainbow-delimiters.enable = true;
        lualine.enable = true;
        barbar = {
          enable = true;
        };
        nvim-colorizer.enable = true;
        undotree.enable = true;
        treesitter.enable = true;
        telescope = {
          enable = true;
          extensions = {
            file-browser.enable = true;
            fzy-native.enable = true;
          };
          keymaps = {
            "<leader>tf" = "find_files";
            "<leader>tb" = "buffers";
            "<leader>td" = "diagnostics";
          };
        };
        dap = {
          enable = true;
          extensions.dap-ui.enable = true;
          adapters.executables = {
            cpptools.command = "${pkgs.vscode-extensions.ms-vscode.cpptools}/share/vscode/extensions/ms-vscode.cpptools/debugAdapters/bin/OpenDebugAD7";
          };
        };
        cmp = {
          enable = true;
          settings = {
            sources = [
              {name = "nvim_lsp";}
              {name = "path";}
              {name = "buffer";}
              {name = "cmdline";}
              {name = "vsnip";}
            ];
            mappingPresets = ["insert"];
            mapping = {
              "<CR>" = "cmp.mapping.confirm({ select = false })";
            };
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
            clangd = {
              enable = true;
              cmd = [
                "clangd"
                "--clang-tidy"
                "--enable-config"
              ];
            };
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
      opts = {
        termguicolors = true;
        wildignore = "*.pyc,*_build/*,**/coverage/*,**/node_modules/*,**/android/*,**/ios/*,**/.git/*";
        number = true;
        relativenumber = true;
        cursorline = true;
        mouse = "ar";
        colorcolumn = "80";
        clipboard = "unnamedplus";
        fillchars = {vert = "\\";};
        tabstop = 2;
        shiftwidth = 2;
        expandtab = true;
        splitbelow = true;
        splitright = true;
        smartcase = true;
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
