return {
  -- LSP configuration
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        -- CSS LSP for styling
        cssls = {},
        -- HTML LSP for JSX
        html = {},
        -- JSON LSP
        jsonls = {
          settings = {
            json = {
              schemas = require("schemastore").json.schemas(),
              validate = { enable = true },
            },
          },
        },
      },
    },
  },

  -- Better LSP UI
  {
    "nvimdev/lspsaga.nvim",
    event = "LspAttach",
    opts = {
      symbol_in_winbar = {
        enable = true,
        separator = "  ",
        ignore_patterns = {},
        show_file = true,
        folder_level = 2,
        respect_root = true,
        color_mode = true,
      },
      lightbulb = {
        enable = true,
        enable_in_insert = true,
        sign = true,
        sign_priority = 40,
        virtual_text = true,
      },
      code_action = {
        enable = true,
        show_server_name = true,
        extend_gitsigns = true,
      },
      hover = {
        enable = true,
        open_link = "<C-l>",
        open_cmd = "!open",
      },
      diagnostic = {
        on_insert = false,
        on_insert_follow = false,
        insert_winblend = 0,
        show_code_action = true,
        show_source = true,
        border_follow = true,
        extend_relatedInformation = false,
        keys = {
          exec_action = "o",
          quit = { "q", "<ESC>" },
          go_action = "g",
        },
      },
    },
  },
}