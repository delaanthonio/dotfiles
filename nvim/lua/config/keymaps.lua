-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

vim.keymap.set("n", "<leader>r", vim.lsp.buf.rename, { desc = "Rename Symbol" })

-- TypeScript/React specific keymaps
local function map(mode, lhs, rhs, opts)
  local options = { noremap = true, silent = true }
  if opts then
    options = vim.tbl_extend("force", options, opts)
  end
  vim.keymap.set(mode, lhs, rhs, options)
end

-- TypeScript Tools keymaps
map("n", "<leader>to", "<cmd>TSToolsOrganizeImports<cr>", { desc = "Organize Imports" })
map("n", "<leader>tr", "<cmd>TSToolsRemoveUnusedImports<cr>", { desc = "Remove Unused Imports" })
map("n", "<leader>ta", "<cmd>TSToolsAddMissingImports<cr>", { desc = "Add Missing Imports" })
map("n", "<leader>tf", "<cmd>TSToolsFixAll<cr>", { desc = "Fix All" })
map("n", "<leader>tg", "<cmd>TSToolsGoToSourceDefinition<cr>", { desc = "Go to Source Definition" })
map("n", "<leader>td", "<cmd>TSToolsRemoveUnused<cr>", { desc = "Remove Unused" })

-- Formatting
map("n", "<leader>f", function()
  require("conform").format({ async = true, lsp_fallback = true })
end, { desc = "Format Code" })

-- Linting
map("n", "<leader>l", "<cmd>Lint<cr>", { desc = "Run Linter" })

-- TypeScript specific
map("n", "<leader>ti", "<cmd>TSToolsInlayHintsToggle<cr>", { desc = "Toggle Inlay Hints" })
map("n", "<leader>ts", "<cmd>TSToolsSortImports<cr>", { desc = "Sort Imports" })

-- React/JSX specific
map("n", "<leader>rc", "i{/*  */}<Esc>hi", { desc = "Add Comment Block" })
map("n", "<leader>rf", "i{() => {}}<Esc>hi", { desc = "Add Function" })
map("n", "<leader>re", "i{error && <div>Error: {error}</div>}<Esc>hi", { desc = "Add Error Display" })

-- Testing keymaps
map("n", "<leader>tt", function()
  require("neotest").run.run()
end, { desc = "Run Test" })

map("n", "<leader>tf", function()
  require("neotest").run.run(vim.fn.expand("%"))
end, { desc = "Run Test File" })

map("n", "<leader>td", function()
  require("neotest").run.run({ strategy = "dap" })
end, { desc = "Debug Test" })

map("n", "<leader>ts", function()
  require("neotest").summary.toggle()
end, { desc = "Test Summary" })

map("n", "<leader>to", function()
  require("neotest").output.open({ enter = true, auto_close = true })
end, { desc = "Test Output" })

map("n", "<leader>tp", function()
  require("neotest").output_panel.toggle()
end, { desc = "Test Panel" })

-- Coverage
map("n", "<leader>tc", function()
  require("coverage").load()
end, { desc = "Load Coverage" })

map("n", "<leader>tC", function()
  require("coverage").toggle()
end, { desc = "Toggle Coverage" })
