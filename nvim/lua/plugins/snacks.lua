return {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
  opts = {
    -- Core components
    bigfile = { enabled = true },
    dashboard = { enabled = true },
    indent = { enabled = true },
    input = { enabled = true },
    notifier = { enabled = true },
    picker = { enabled = true },
    quickfile = { enabled = true },
    scope = { enabled = true },
    scroll = { enabled = true },
    statuscolumn = { enabled = true },
    words = { enabled = true },

    -- Disable components that require specific terminal support
    explorer = { enabled = false },
    image = { enabled = false }, -- Requires kitty/ghostty/wezterm with graphics support

    -- Optional components
    animate = { enabled = true },
    lazygit = { enabled = true },

    -- Notifier configuration
    notifier = {
      enabled = true,
      timeout = 3000,
      width = { min = 40, max = 120 },
      height = { min = 1, max = 20 },
      top_down = true,
      left_margin = 1,
      right_margin = 1,
    },

    -- Input dialog configuration
    input = {
      enabled = true,
      icon = " ",
      win = {
        style = "minimal",
        border = "rounded",
      },
    },

    -- Picker configuration
    picker = {
      enabled = true,
      layout = {
        preset = "telescope",
      },
    },

    -- Scroll animation
    scroll = {
      enabled = true,
      animate = {
        duration = { step = 15, total = 250 },
        easing = "linear",
      },
    },

    -- Dashboard configuration
    dashboard = {
      enabled = true,
      width = 60,
      height = 30,
      preset = {
        keys = {
          { icon = " ", key = "f", desc = "Find File", action = ":Snacks picker files" },
          { icon = " ", key = "n", desc = "New File", action = ":ene | startinsert" },
          { icon = " ", key = "g", desc = "Find Text", action = ":Snacks picker grep" },
          { icon = " ", key = "r", desc = "Recent Files", action = ":Snacks picker recent" },
          { icon = " ", key = "c", desc = "Config", action = ":Snacks picker files cwd=~/.config/nvim" },
          { icon = "󰒲 ", key = "l", desc = "Lazy", action = ":Lazy" },
          { icon = " ", key = "q", desc = "Quit", action = ":qa" },
        },
        header = [[
███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗
████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║
██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║
██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║
██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║
╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝]],
      },
    },
  },
}
