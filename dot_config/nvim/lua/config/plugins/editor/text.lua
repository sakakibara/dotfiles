-- lua/config/plugins/editor/text.lua
-- Text manipulation: surround, text objects, autopair, yank ring, smart
-- inc/dec, auto-indent, autoclose keywords, extended matchpair, split/join,
-- trailing whitespace, case conversion, alignment, better-escape, readline
-- bindings, comments, todo highlighting, incremental rename.

return {
  -- Surround operators: gsa / gsd / gsr / gsf / gsF / gsh
  {
    "echasnovski/mini.surround",
    name = "mini.surround",
    keys = {
      { "gsa", desc = "Add surround",         mode = { "n", "v" } },
      { "gsd", desc = "Delete surround" },
      { "gsr", desc = "Replace surround" },
      { "gsf", desc = "Find surround right" },
      { "gsF", desc = "Find surround left" },
      { "gsh", desc = "Highlight surround" },
    },
    opts = {
      mappings = {
        add            = "gsa",
        delete         = "gsd",
        find           = "gsf",
        find_left      = "gsF",
        highlight      = "gsh",
        replace        = "gsr",
        update_n_lines = "gsn",
      },
    },
  },

  -- Better text objects — treesitter-aware function / class / block regions
  {
    "echasnovski/mini.ai",
    name = "mini.ai",
    event = "VeryLazy",
    dependencies = { "nvim-treesitter-textobjects" },
    config = function()
      local ai = require("mini.ai")
      ai.setup({
        n_lines = 500,
        custom_textobjects = {
          o = ai.gen_spec.treesitter({
            a = { "@block.outer", "@conditional.outer", "@loop.outer" },
            i = { "@block.inner", "@conditional.inner", "@loop.inner" },
          }),
          f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }),
          c = ai.gen_spec.treesitter({ a = "@class.outer",    i = "@class.inner" }),
          t = { "<([%p%w]-)%f[^<%w][^<>]->.-</%1>", "^<.->().-()</[^/]->$" },
          d = { "%f[%d]%d+" },                                -- digits
          e = {                                               -- CamelCase / snake_case parts
            { "%u[%l%d]+%f[^%l%d]", "%f[%S][%l%d]+%f[^%l%d]", "%f[%P][%l%d]+%f[^%l%d]", "^[%l%d]+%f[^%l%d]" },
            "^().*()$",
          },
          g = function() -- whole buffer
            local from = { line = 1, col = 1 }
            local to = { line = vim.fn.line("$"), col = math.max(vim.fn.getline("$"):len(), 1) }
            return { from = from, to = to }
          end,
          u = ai.gen_spec.function_call(),                               -- u = "usage"
          U = ai.gen_spec.function_call({ name_pattern = "[%w_]" }),     -- include dot.method

          -- i = indentation block (essential for Python / YAML / fish).
          -- `ii` selects the contiguous block at the cursor's indent; `ai`
          -- extends to the enclosing lines (one above, one below).
          i = function(ai_type)
            local spaces = (" "):rep(vim.o.tabstop)
            local lines  = vim.api.nvim_buf_get_lines(0, 0, -1, false)
            local rows   = {}
            for l, line in ipairs(lines) do
              if not line:find("^%s*$") then
                rows[#rows + 1] = {
                  line   = l,
                  indent = #line:gsub("\t", spaces):match("^%s*"),
                  text   = line,
                }
              end
            end
            local ret = {}
            for i = 1, #rows do
              if i == 1 or rows[i - 1].indent < rows[i].indent then
                local from, to = i, i
                for j = i + 1, #rows do
                  if rows[j].indent < rows[i].indent then break end
                  to = j
                end
                from = ai_type == "a" and from > 1      and from - 1 or from
                to   = ai_type == "a" and to   < #rows  and to   + 1 or to
                ret[#ret + 1] = {
                  indent = rows[i].indent,
                  from   = { line = rows[from].line, col = ai_type == "a" and 1 or rows[from].indent + 1 },
                  to     = { line = rows[to].line,   col = #rows[to].text },
                }
              end
            end
            return ret
          end,
        },
      })
    end,
  },

  -- Autopair brackets / quotes / backticks. nvim-autopairs over mini.pairs
  -- for richer rules (jsx, markdown fences) and fast-wrap (<M-e> wraps the
  -- word after cursor). Treesitter-aware via check_ts: skips pairing inside
  -- strings / comments when a parser is available, prevents annoying
  -- double-quote inside docstrings, etc.
  {
    "windwp/nvim-autopairs",
    name = "nvim-autopairs",
    event = "InsertEnter",
    opts = {
      check_ts        = true,
      ts_config       = {
        lua        = { "string" },
        javascript = { "template_string" },
      },
      disable_filetype = { "TelescopePrompt", "vim", "spectre_panel" },
      fast_wrap       = { map = "<M-e>" },
    },
  },

  -- Extended matchpair: `%` jumps if/else/end, function/end, HTML tags
  {
    "andymass/vim-matchup",
    name = "vim-matchup",
    event = "LazyFile",
    init = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
      vim.g.matchup_matchparen_deferred = 1
    end,
  },

  -- Yank ring + paste cycling (<C-n>/<C-p> after paste).
  -- IMPORTANT: we deliberately DON'T remap `y`. Yanky's YankyYank plug is
  -- an expr-operator that re-emits "y"; it composes badly with mini.ai's
  -- operator-pending text objects (yag / yaf / etc. silently no-op when
  -- both are active). Yanky tracks all yanks via TextYankPost regardless,
  -- so the ring still fills up from regular `y`. We only override paste
  -- and cycling.
  {
    "gbprod/yanky.nvim",
    name = "yanky.nvim",
    event = "LazyFile",
    keys = {
      { "p",          "<Plug>(YankyPutAfter)",                           mode = { "n", "x" }, desc = "Put after" },
      { "P",          "<Plug>(YankyPutBefore)",                          mode = { "n", "x" }, desc = "Put before" },
      { "gp",         "<Plug>(YankyGPutAfter)",                          mode = { "n", "x" }, desc = "Put after + cursor" },
      { "gP",         "<Plug>(YankyGPutBefore)",                         mode = { "n", "x" }, desc = "Put before + cursor" },
      { "]p",         "<Plug>(YankyPutIndentAfterLinewise)",                                  desc = "Put after (indent)" },
      { "[p",         "<Plug>(YankyPutIndentBeforeLinewise)",                                 desc = "Put before (indent)" },
      { "<C-n>",      "<Plug>(YankyNextEntry)",                                               desc = "Next yank entry" },
      { "<C-p>",      "<Plug>(YankyPreviousEntry)",                                           desc = "Prev yank entry" },
      { "]y",         "<Plug>(YankyNextEntry)",                                               desc = "Next yank entry" },
      { "[y",         "<Plug>(YankyPreviousEntry)",                                           desc = "Prev yank entry" },
      { "<Leader>sy", function() vim.cmd("YankyRingHistory") end,                             desc = "Yank history" },
    },
    opts = {
      ring             = { storage = "shada", history_length = 200 },
      highlight        = { timer = 200 },
      system_clipboard = { sync_with_ring = true },
    },
  },

  -- Smart <C-a>/<C-x>: integers, hex, dates, booleans, semver, and/or, &&/||
  {
    "monaqa/dial.nvim",
    name = "dial.nvim",
    keys = {
      { "<C-a>",  function() return require("dial.map").inc_normal()  end, mode = "n", expr = true, desc = "Increment" },
      { "<C-x>",  function() return require("dial.map").dec_normal()  end, mode = "n", expr = true, desc = "Decrement" },
      { "<C-a>",  function() return require("dial.map").inc_visual()  end, mode = "v", expr = true, desc = "Increment" },
      { "<C-x>",  function() return require("dial.map").dec_visual()  end, mode = "v", expr = true, desc = "Decrement" },
      { "g<C-a>", function() return require("dial.map").inc_gvisual() end, mode = "v", expr = true, desc = "Inc sequence" },
      { "g<C-x>", function() return require("dial.map").dec_gvisual() end, mode = "v", expr = true, desc = "Dec sequence" },
    },
    config = function()
      local augend       = require("dial.augend")
      local color_augend = require("lib.colors.augends")
      require("dial.config").augends:register_group({
        default = {
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.date.alias["%Y-%m-%d"],
          augend.date.alias["%Y/%m/%d"],
          augend.constant.alias.bool,
          augend.semver.alias.semver,
          augend.constant.new({ elements = { "and", "or" }, word = true,  cyclic = true }),
          augend.constant.new({ elements = { "&&",  "||" }, word = false, cyclic = true }),
          -- Color channel bumping in OKLCH space (preserves source syntax).
          color_augend.lightness({ step = 0.05 }),
          color_augend.chroma   ({ step = 0.02 }),
          color_augend.hue      ({ step = 5    }),
        },
      })
    end,
  },

  -- Auto-detect buffer indentation (tabstop, expandtab) from file content
  { "tpope/vim-sleuth", name = "vim-sleuth", event = "LazyFile" },

  -- Auto-insert `end` / `endif` / `fi` after the opening keyword
  {
    "RRethy/nvim-treesitter-endwise",
    name = "nvim-treesitter-endwise",
    event = "InsertEnter",
    dependencies = { "nvim-treesitter" },
  },

  -- Incremental rename preview — see the rename happen while you type.
  -- The `grn` key is wired in lib/lsp.lua on LspAttach, overriding Neovim
  -- 0.11+'s default plain-rename with this preview variant. This spec just
  -- declares the plugin + its loader; the keymap lives with the LSP setup.
  {
    "smjonas/inc-rename.nvim",
    name = "inc-rename.nvim",
    cmd  = "IncRename",
    opts = {},
  },

  -- Case conversion: gb{u,l,s,d,n,a,c,p,t,f} for current word;
  -- uppercase variants rename via LSP across workspace.
  {
    "johmsalas/text-case.nvim",
    name = "text-case.nvim",
    dependencies = { "plenary.nvim" },
    cmd  = { "TextCaseOpenTelescope", "Subs" },
    keys = (function()
      local cases = {
        { "u", "to_upper_case",    "upper" },
        { "l", "to_lower_case",    "lower" },
        { "s", "to_snake_case",    "snake" },
        { "d", "to_dash_case",     "dash" },
        { "n", "to_constant_case", "constant" },
        { "a", "to_phrase_case",   "phrase" },
        { "c", "to_camel_case",    "camel" },
        { "p", "to_pascal_case",   "pascal" },
        { "t", "to_title_case",    "title" },
        { "f", "to_path_case",     "path" },
      }
      local keys = {}
      for _, c in ipairs(cases) do
        keys[#keys + 1] = { "gb" .. c[1],         function() require("textcase").current_word(c[2]) end, mode = { "n", "x" }, desc = "Case: " .. c[3] }
        keys[#keys + 1] = { "gb" .. c[1]:upper(), function() require("textcase").lsp_rename(c[2])   end, mode = { "n", "x" }, desc = "LSP rename: " .. c[3] }
      end
      return keys
    end)(),
    config = function() require("textcase").setup({}) end,
  },

  -- Align operators (mini.align defaults: gai / gaw / etc.)
  { "echasnovski/mini.align", name = "mini.align", event = "VeryLazy", opts = {} },

  -- Split/join structures (arg lists, tables) across lines: gS
  {
    "echasnovski/mini.splitjoin",
    name = "mini.splitjoin",
    keys = { { "gS", desc = "Split/join toggle", mode = { "n", "x" } } },
    opts = { mappings = { toggle = "gS" } },
  },

  -- Trim trailing whitespace
  {
    "echasnovski/mini.trailspace",
    name = "mini.trailspace",
    event = "LazyFile",
    keys = {
      { "<Leader>uw", function() require("mini.trailspace").trim()            end, desc = "Trim trailing whitespace" },
      { "<Leader>uW", function() require("mini.trailspace").trim_last_lines() end, desc = "Trim trailing empty lines" },
    },
    opts = {},
  },

  -- jk chord → <Esc> in insert + cmdline
  {
    "max397574/better-escape.nvim",
    name = "better-escape.nvim",
    event = { "InsertEnter", "CmdlineEnter" },
    opts = {
      default_mappings = false,
      mappings = {
        i = { j = { k = "<Esc>" } },
        c = { j = { k = "<Esc>" } },
      },
    },
  },

  -- Readline bindings (C-a/C-e/C-w/M-b/M-f) in insert + cmdline
  { "tpope/vim-rsi", name = "vim-rsi", event = { "InsertEnter", "CmdlineEnter" } },

  -- Treesitter-aware commentstring (JSX/Vue embedded langs pick right syntax)
  { "folke/ts-comments.nvim", name = "ts-comments.nvim", event = "VeryLazy", opts = {} },

  -- TODO / FIXME / NOTE / HACK highlight + navigation
  {
    "folke/todo-comments.nvim",
    name = "todo-comments.nvim",
    event = "LazyFile",
    cmd  = { "TodoTrouble", "TodoTelescope", "TodoQuickFix", "TodoLocList" },
    opts = {},
    keys = {
      { "]t",         function() require("todo-comments").jump_next() end,            desc = "Next todo" },
      { "[t",         function() require("todo-comments").jump_prev() end,            desc = "Prev todo" },
      { "<Leader>st", "<Cmd>TodoTrouble<CR>",                                         desc = "Todo (Trouble)" },
      { "<Leader>sT", "<Cmd>TodoTrouble keywords=TODO,FIX,FIXME<CR>",                 desc = "Todo/Fix (Trouble)" },
    },
  },
}
