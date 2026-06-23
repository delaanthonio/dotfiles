return {
  "ahmedkhalf/project.nvim",
  event = "BufReadPre",
  opts = {
    detection_methods = { "lsp", "pattern" },
    patterns = { ".git", "_darcs", ".hg", ".bzr", ".svn", "Makefile", "package.json" },
    show_hidden = true,
    silent_chdir = true,
    scope_chdir = "global",
  },
  config = function(_, opts)
    require("project_nvim").setup(opts)
  end,
  keys = {
    { "<leader>p", function() Snacks.picker.projects() end, desc = "Projects" },
  },
}
