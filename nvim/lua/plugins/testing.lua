return {
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter",
      -- Adapters (only listed here, not as separate specs)
      "nvim-neotest/neotest-jest",
      "nvim-neotest/neotest-python",
      "codymikol/neotest-kotlin",
    },
    config = function()
      require("neotest").setup({
        adapters = {
          -- Jest
          require("neotest-jest")({
            jestCommand = function(path)
              -- Auto-detect package manager from lockfiles
              local dir = vim.fs.dirname(path)
              local pkg_root = vim.fs.find({ "package.json" }, { path = dir, upward = true })[1]
              if pkg_root then
                local root_dir = vim.fs.dirname(pkg_root)
                if vim.fn.filereadable(root_dir .. "/pnpm-lock.yaml") == 1 then
                  return "pnpm test --"
                elseif vim.fn.filereadable(root_dir .. "/yarn.lock") == 1 then
                  return "yarn test --"
                end
              end
              return "npm test --"
            end,
            jestConfigFile = "jest.config.js",
            env = { CI = true },
            cwd = function(path)
              -- Find nearest Jest-configured package root (monorepo-safe)
              local dir = vim.fs.dirname(path)
              local root = vim.fs.find(
                { "jest.config.js", "jest.config.ts", "jest.config.mjs", "jest.config.cjs", "package.json" },
                { path = dir, upward = true }
              )[1]
              return root and vim.fs.dirname(root) or vim.fn.getcwd()
            end,
          }),
          -- Python/pytest (auto-detects venv)
          require("neotest-python")({
            runner = "pytest",
          }),
          -- Kotlin (Gradle only)
          require("neotest-kotlin"),
        },
        status = { virtual_text = true },
        output = { open_on_run = true },
        quickfix = {
          open = function()
            -- Safely try trouble, fallback to built-in quickfix
            local ok, trouble = pcall(require, "trouble")
            if ok then
              trouble.open({ mode = "quickfix", focus = false })
            else
              vim.cmd("copen")
            end
          end,
        },
      })
    end,
  },

  -- Test coverage
  {
    "andythigpen/nvim-coverage",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("coverage").setup({
        commands = true,
        highlights = {
          covered = { fg = "#C3E88D" },
          uncovered = { fg = "#F07178" },
        },
        signs = {
          covered = { hl = "CoverageCovered", text = "▎" },
          uncovered = { hl = "CoverageUncovered", text = "▎" },
        },
        summary = {
          min_coverage = 80,
        },
      })
    end,
  },
}