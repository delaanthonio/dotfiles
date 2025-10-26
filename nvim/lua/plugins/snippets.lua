return {
  -- Snippets for TypeScript/React
  {
    "L3MON4D3/LuaSnip",
    dependencies = {
      "rafamadriz/friendly-snippets",
    },
    config = function()
      local luasnip = require("luasnip")
      local types = require("luasnip.util.types")

      luasnip.config.setup({
        history = true,
        updateevents = "TextChanged,TextChangedI",
        enable_autosnippets = true,
        ext_opts = {
          [types.choiceNode] = {
            active = {
              virt_text = { { "choiceNode", "Comment" } },
            },
          },
        },
      })

      -- Load friendly-snippets
      require("luasnip.loaders.from_vscode").lazy_load()

      -- Custom snippets for React/TypeScript
      local s = luasnip.snippet
      local t = luasnip.text_node
      local i = luasnip.insert_node
      local f = luasnip.function_node
      local c = luasnip.choice_node
      local d = luasnip.dynamic_node

      luasnip.add_snippets("typescriptreact", {
        -- React component
        s("rfc", {
          t("import React from 'react';\n\ninterface "),
          i(1, "ComponentName"),
          t("Props {\n  "),
          i(2, "children?: React.ReactNode;\n"),
          t("}\n\nconst "),
          f(function(args)
            return args[1][1]
          end, { 1 }),
          t(": React.FC<"),
          f(function(args)
            return args[1][1]
          end, { 1 }),
          t("Props> = ({ "),
          i(3, "children"),
          t(" }) => {\n  return (\n    <div>\n      "),
          i(4, "// Component content"),
          t("\n    </div>\n  );\n};\n\nexport default "),
          f(function(args)
            return args[1][1]
          end, { 1 }),
          t(";"),
        }),

        -- React hook
        s("rh", {
          t("const "),
          i(1, "useHook"),
          t(" = () => {\n  "),
          i(2, "// Hook logic"),
          t("\n  return {\n    "),
          i(3, "// Return values"),
          t("\n  };\n};"),
        }),

        -- useEffect
        s("ue", {
          t("useEffect(() => {\n  "),
          i(1, "// Effect logic"),
          t("\n}, ["),
          i(2, "dependencies"),
          t("]);"),
        }),

        -- useState
        s("us", {
          t("const ["),
          i(1, "state"),
          t(", set"),
          f(function(args)
            return args[1][1]:gsub("^%l", string.upper)
          end, { 1 }),
          t("] = useState<"),
          i(2, "string"),
          t(">("),
          i(3, '""'),
          t(");"),
        }),

        -- TypeScript interface
        s("ti", {
          t("interface "),
          i(1, "InterfaceName"),
          t(" {\n  "),
          i(2, "property"),
          t(": "),
          i(3, "string"),
          t(";\n}"),
        }),

        -- TypeScript type
        s("tt", {
          t("type "),
          i(1, "TypeName"),
          t(" = "),
          i(2, "string"),
          t(";"),
        }),

        -- Console log
        s("cl", {
          t("console.log("),
          i(1, "value"),
          t(");"),
        }),

        -- Arrow function
        s("af", {
          t("const "),
          i(1, "functionName"),
          t(" = ("),
          i(2, "params"),
          t(") => {\n  "),
          i(3, "// Function body"),
          t("\n};"),
        }),

        -- Jest test
        s("test", {
          t("test('"),
          i(1, "should do something"),
          t("', () => {\n  "),
          i(2, "// Test implementation"),
          t("\n});"),
        }),

        -- Jest describe
        s("describe", {
          t("describe('"),
          i(1, "Component"),
          t("', () => {\n  "),
          i(2, "// Tests"),
          t("\n});"),
        }),

        -- Jest it
        s("it", {
          t("it('"),
          i(1, "should do something"),
          t("', () => {\n  "),
          i(2, "// Test implementation"),
          t("\n});"),
        }),

        -- Jest expect
        s("expect", {
          t("expect("),
          i(1, "value"),
          t(")."),
          i(2, "toBe"),
          t("("),
          i(3, "expected"),
          t(");"),
        }),

        -- React Testing Library render
        s("rtl", {
          t("import { render, screen } from '@testing-library/react';\nimport "),
          i(1, "Component"),
          t(" from './"),
          i(1, "Component"),
          t("';\n\nit('"),
          i(2, "renders correctly"),
          t("', () => {\n  render(<"),
          i(1, "Component"),
          t(" />);\n  "),
          i(3, "// Test assertions"),
          t("\n});"),
        }),
      })

      luasnip.add_snippets("typescript", {
        -- TypeScript interface
        s("ti", {
          t("interface "),
          i(1, "InterfaceName"),
          t(" {\n  "),
          i(2, "property"),
          t(": "),
          i(3, "string"),
          t(";\n}"),
        }),

        -- TypeScript type
        s("tt", {
          t("type "),
          i(1, "TypeName"),
          t(" = "),
          i(2, "string"),
          t(";"),
        }),

        -- Console log
        s("cl", {
          t("console.log("),
          i(1, "value"),
          t(");"),
        }),

        -- Arrow function
        s("af", {
          t("const "),
          i(1, "functionName"),
          t(" = ("),
          i(2, "params"),
          t(") => {\n  "),
          i(3, "// Function body"),
          t("\n};"),
        }),

        -- Jest test
        s("test", {
          t("test('"),
          i(1, "should do something"),
          t("', () => {\n  "),
          i(2, "// Test implementation"),
          t("\n});"),
        }),

        -- Jest describe
        s("describe", {
          t("describe('"),
          i(1, "Component"),
          t("', () => {\n  "),
          i(2, "// Tests"),
          t("\n});"),
        }),

        -- Jest it
        s("it", {
          t("it('"),
          i(1, "should do something"),
          t("', () => {\n  "),
          i(2, "// Test implementation"),
          t("\n});"),
        }),

        -- Jest expect
        s("expect", {
          t("expect("),
          i(1, "value"),
          t(")."),
          i(2, "toBe"),
          t("("),
          i(3, "expected"),
          t(");"),
        }),
      })
    end,
  },

  -- CMP LuaSnip integration (loads after nvim-cmp)
  {
    "saadparwaiz1/cmp_luasnip",
    dependencies = {
      "L3MON4D3/LuaSnip",
      "hrsh7th/nvim-cmp",
    },
    lazy = true,
    event = "InsertEnter",
  },
}