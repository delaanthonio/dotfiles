return {
  "nvim-neo-tree/neo-tree.nvim",
  branch = "v3.x",
  cmd = "Neotree",
  keys = {
    { "<leader>e", "<cmd>Neotree toggle<cr>", desc = "Explorer (Neo-tree)" },
    { "<leader>E", "<cmd>Neotree reveal<cr>", desc = "Reveal in Neo-tree" },
    { "<leader>b", "<cmd>Neotree buffers toggle<cr>", desc = "Buffers (Neo-tree)" },
    { "<leader>G", "<cmd>Neotree git_status float<cr>", desc = "Git status (Neo-tree)" },
  },
  opts = {
    close_if_last_window = true,
    popup_border_style = "rounded",

    default_component_configs = {
      indent = { padding = 0 },
      git_status = {
        symbols = {
          added = "✚",
          modified = "",
          deleted = "✖",
          renamed = "",
          untracked = "",
          ignored = "",
          unstaged = "",
          staged = "",
          conflict = "",
        },
      },
    },

    window = {
      position = "left",
      width = 30,
      mappings = {
        ["<space>"] = "toggle_node",
        ["l"] = "open",
        ["h"] = "close_node",
      },
    },

    filesystem = {
      follow_current_file = { enabled = true },
      hijack_netrw_behavior = "open_default",
      filtered_items = {
        visible = true, -- show dotfiles
        hide_gitignored = true, -- hide node_modules, dist, etc.
        hide_hidden = false,
      },
    },

    buffers = {
      follow_current_file = { enabled = true },
      group_empty_dirs = true,
      show_unloaded = true,
    },

    git_status = {
      window = { position = "float" },
    },
  },
}
