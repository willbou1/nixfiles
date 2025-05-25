{pkgs, ...}:
with builtins; 
{
  xdg.configFile."clangd/config.yaml".source = ./clangd.yaml;
  programs.nixvim = {
    enable = true;
    package = pkgs.neovim-unwrapped;
    plugins = {
      web-devicons.enable = true;
      rainbow-delimiters.enable = true;
      lualine.enable = true;
      barbar = {
        enable = true;
        settings = {
          auto_hide = 1;
          highlight_alternate = true;
        };
      };
      nvim-colorizer.enable = true;
      treesitter.enable = true;
      fugitive.enable = true;
      telescope = {
        enable = true;
        extensions = {
          file-browser.enable = true;
          fzy-native.enable = true;
          undo.enable = true;
        };
        settings = {
          defaults = {
            layout_config = {
              width = 0.85;
              preview_width = 0.60;
            };
          };
          set_env.COLORTERM = "truecolor";
        };
        keymaps = {
          "<leader>tf" = "find_files";
          "<leader>tb" = "buffers";
          "<leader>td" = "diagnostics";
          "<leader>tgs" = "git_status";
          "<leader>tgc" = "git_commits";
          "<leader>tgb" = "git_branches";
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
          sources =
            map (n: {name = n;})
            ["nvim_lsp" "path" "buffer" "cmdline" "vsnip" "calc" "spell"];
          mappingPresets = ["insert"];
          mapping = {
            "<C-Space>" = "cmp.mapping.complete()";
            "<CR>" = "cmp.mapping.confirm({ select = true })";
            "<C-j>" = "cmp.mapping.select_next_item()";
            "<C-k>" = "cmp.mapping.select_prev_item()";
          };
          preselect = "cmp.PreselectMode.None";
        };
      };
      lsp = {
        enable = true;
        servers = {
          java_language_server.enable = true;
          hls = {
            enable = true;
            installGhc = false;
          };
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
          rust_analyzer = {
            enable = true;
            installCargo = false;
            installRustc = false;
          };
          cssls.enable = true;
          omnisharp.enable = true;
          nil_ls.enable = true;
          gopls.enable = true;
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
      (pkgs.vimUtils.buildVimPlugin {
        name = "transpose-words";
        src = pkgs.fetchFromGitHub {
          owner = "vim-scripts";
          repo = "transpose-words";
          rev = "b9e7cb0f8cd1a59ae7850287086ebdcd50f7c4fb";
          hash = "sha256-r0K8xm8VGc1/PXUbmn+NW52IgBDziIhmoJEdJ88PTMY=";
        };
      })
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
      fillchars.vert = "❙";
      tabstop = 4;
      shiftwidth = 4;
      expandtab = true;
      splitbelow = true;
      splitright = true;
      spelllang = "en_ca";
      guifont = "Monospace:h18";
      undofile = true;
      ignorecase = true;
      smartcase = true;
    };
    autoCmd = [
      {
        pattern = ["*.nix"];
        event = ["BufNewFile" "BufRead"];
        command = ''
          set tabstop=2
          set shiftwidth=2
        '';
      }
    ];
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
