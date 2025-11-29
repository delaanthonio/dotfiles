-- Consolidated treesitter configuration
return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        -- TypeScript/JavaScript
        "typescript",
        "tsx",
        "javascript",
        "jsx",
        "json",
        "jsonc",
        "css",
        "scss",
        "html",
        -- Python
        "python",
        "rst",
        -- Terraform
        "terraform",
        "hcl",
        -- Markdown
        "markdown",
        "markdown_inline",
        -- YAML/TOML
        "yaml",
        "toml",
        -- Shell
        "bash",
        -- Lua
        "lua",
        -- Git
        "git_config",
        "gitcommit",
        "gitignore",
        -- Other
        "regex",
        "query",
        "vim",
        "vimdoc",
      },
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
      },
    },
  },
}
