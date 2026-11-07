return {
  "stevearc/overseer.nvim",
  dependencies = { "nvim-lua/plenary.nvim" },
  event = "VeryLazy",
  opts = {
    templates = { "builtin" },
    task_list = {
      direction = "bottom",
      min_height = 10,
      bindings = {
        ["q"] = "Close",
        ["<CR>"] = "RunAction",
      },
    },
  },
  keys = {
    { "<leader>or", "<cmd>OverseerRun<cr>", desc = "Run Task" },
    { "<leader>ot", "<cmd>OverseerToggle<cr>", desc = "Toggle Tasks" },
    { "<leader>oa", "<cmd>OverseerTaskAction<cr>", desc = "Task Action" },
    { "<leader>ol", "<cmd>OverseerRestartLast<cr>", desc = "Restart Last" },
  },
}
