-- Consolidated formatting configuration (conform.nvim)
return {
  {
    "stevearc/conform.nvim",
    opts = {
      formatters_by_ft = {
        -- TypeScript/JavaScript (prettierd for speed)
        typescript = { "prettierd" },
        typescriptreact = { "prettierd" },
        javascript = { "prettierd" },
        javascriptreact = { "prettierd" },
        -- Web formats
        json = { "prettierd" },
        jsonc = { "prettierd" },
        css = { "prettierd" },
        scss = { "prettierd" },
        html = { "prettierd" },
        markdown = { "prettierd" },
        yaml = { "prettierd" },
        -- Python (ruff for speed and consistency)
        python = { "ruff_organize_imports", "ruff_format" },
      },
      default_format_opts = {
        lsp_format = "fallback",
      },
    },
  },
}
