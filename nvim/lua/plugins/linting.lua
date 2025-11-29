-- Consolidated linting configuration (nvim-lint)
return {
  {
    "mfussenegger/nvim-lint",
    opts = {
      linters_by_ft = {
        -- TypeScript/JavaScript (eslint_d for speed)
        typescript = { "eslint_d" },
        typescriptreact = { "eslint_d" },
        javascript = { "eslint_d" },
        javascriptreact = { "eslint_d" },
        -- Python
        python = { "ruff" },
      },
    },
  },
}
