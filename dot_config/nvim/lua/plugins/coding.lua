return {
  {
    "echasnovski/mini.surround",
    version = "*",
    event = "VeryLazy",
    opts = {
      mappings = {
        add = "gsa",
        delete = "gsd",
        find = "gsf",
        find_left = "gsF",
        highlight = "gsh",
        replace = "gsr",
        update_n_lines = "gsn",
      },
    },
  },

  {
    "folke/ts-comments.nvim",
    event = "VeryLazy",
    opts = {},
  },

  {
    "altermo/ultimate-autopair.nvim",
    event = {
      "InsertEnter",
      "CmdlineEnter",
    },
    branch = "v0.6",
    opts = {},
  },

  {
    "RRethy/nvim-treesitter-endwise",
    event = "InsertEnter",
  },

  {
    "andymass/vim-matchup",
    event = "LazyFile",
    init = function()
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_matchparen_nomode = "vV<c-v>"
      vim.g.matchup_matchparen_offscreen = {}
    end,
  },

  {
    "L3MON4D3/LuaSnip",
    dependencies = {
      "rafamadriz/friendly-snippets",
      config = function()
        require("luasnip.loaders.from_vscode").lazy_load()
      end,
    },
    opts = {
      history = true,
      delete_check_events = "TextChanged",
    },
    keys = {
      {
        "<Tab>",
        function()
          local copilot_suggestion = require("copilot.suggestion")
          if copilot_suggestion.is_visible() then
            copilot_suggestion.accept()
          elseif require("luasnip").jumpable(1) then
            return "<Plug>luasnip-jump-next"
          else
            return "<Tab>"
          end
        end,
        expr = true,
        silent = true,
        mode = "i",
      },
      {
        "<Tab>",
        function()
          require("luasnip").jump(1)
        end,
        mode = "s",
      },
      {
        "<S-Tab>",
        function()
          require("luasnip").jump(-1)
        end,
        mode = { "i", "s" },
      },
    },
    config = function(_, opts)
      local ls = require("luasnip")
      vim.api.nvim_create_autocmd("CursorMovedI", {
        group = vim.api.nvim_create_augroup("userconf_unlink_luasnip", { clear = true }),
        pattern = "*",
        callback = function(ev)
          if not ls.session or not ls.session.current_nodes[ev.buf] or ls.session.jump_active then
            return
          end

          local current_node = ls.session.current_nodes[ev.buf]
          local current_start, current_end = current_node:get_buf_position()
          current_start[1] = current_start[1] + 1
          current_end[1] = current_end[1] + 1
          local cursor = vim.api.nvim_win_get_cursor(0)

          if
            cursor[1] < current_start[1]
            or cursor[1] > current_end[1]
            or cursor[2] < current_start[2]
            or cursor[2] > current_end[2]
          then
            ls.unlink_current()
          end
        end,
      })
    end,
  },

  {
    "hrsh7th/nvim-cmp",
    version = false,
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "saadparwaiz1/cmp_luasnip",
    },
    opts = function()
      vim.api.nvim_set_hl(0, "CmpGhostText", { link = "Comment", default = true })
      local cmp = require("cmp")
      local defaults = require("cmp.config.default")()
      local auto_select = true
      return {
        completion = {
          autocomplete = false,
          completeopt = "menu,menuone,noinsert",
        },
        snippet = {
          expand = function(args)
            require("luasnip").lsp_expand(args.body)
          end,
        },
        preselect = auto_select and cmp.PreselectMode.Item or cmp.PreselectMode.None,
        mapping = cmp.mapping.preset.insert({
          ["<C-b>"] = cmp.mapping.scroll_docs(-4),
          ["<C-f>"] = cmp.mapping.scroll_docs(4),
          ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
          ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.abort(),
          ["<CR>"] = Util.cmp.confirm({ select = auto_select }),
          ["<C-y>"] = Util.cmp.confirm({ select = true }),
          ["<S-CR>"] = Util.cmp.confirm({ behavior = cmp.ConfirmBehavior.Replace }),
          ["<C-CR>"] = function(fallback)
            cmp.abort()
            fallback()
          end,
        }),
        sources = cmp.config.sources(
          { { name = "nvim_lsp" }, { name = "luasnip" }, { name = "path" } },
          { { name = "buffer" } }
        ),
        formatting = {
          format = function(_, item)
            local icons = Util.config.icons.kinds
            if icons[item.kind] then
              item.kind = icons[item.kind] .. item.kind
            end

            local widths = {
              abbr = 40,
              menu = 30,
            }

            for key, width in pairs(widths) do
              if item[key] and vim.fn.strdisplaywidth(item[key]) > width then
                item[key] = vim.fn.strcharpart(item[key], 0, width - 1) .. "…"
              end
            end

            return item
          end,
        },
        experimental = {
          ghost_text = {
            hl_group = "CmpGhostText",
          },
        },
        sorting = defaults.sorting,
      }
    end,
    config = function(_, opts)
      for _, source in ipairs(opts.sources) do
        source.group_index = source.group_index or 1
      end
      require("cmp").setup(opts)
    end,
  },

  {
    "echasnovski/mini.ai",
    event = "VeryLazy",
    dependencies = { "nvim-treesitter-textobjects" },
    opts = function()
      local ai = require("mini.ai")
      return {
        n_lines = 500,
        custom_textobjects = {
          o = ai.gen_spec.treesitter({
            a = { "@block.outer", "@conditional.outer", "@loop.outer" },
            i = { "@block.inner", "@conditional.inner", "@loop.inner" },
          }, {}),
          f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }, {}),
          c = ai.gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }, {}),
          t = { "<([%p%w]-)%f[^<%w][^<>]->.-</%1>", "^<.->().*()</[^/]->$" },
          d = { "%f[%d]%d+" },
          e = {
            { "%u[%l%d]+%f[^%l%d]", "%f[%S][%l%d]+%f[^%l%d]", "%f[%P][%l%d]+%f[^%l%d]", "^[%l%d]+%f[^%l%d]" },
            "^().*()$",
          },
          i = function(ai_type)
            local spaces = (" "):rep(vim.o.tabstop)
            local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
            local indents = {}

            for l, line in ipairs(lines) do
              if not line:find("^%s*$") then
                indents[#indents + 1] = { line = l, indent = #line:gsub("\t", spaces):match("^%s*"), text = line }
              end
            end

            local ret = {}

            for i = 1, #indents do
              if i == 1 or indents[i - 1].indent < indents[i].indent then
                local from, to = i, i
                for j = i + 1, #indents do
                  if indents[j].indent < indents[i].indent then
                    break
                  end
                  to = j
                end
                from = ai_type == "a" and from > 1 and from - 1 or from
                to = ai_type == "a" and to < #indents and to + 1 or to
                ret[#ret + 1] = {
                  indent = indents[i].indent,
                  from = { line = indents[from].line, col = ai_type == "a" and 1 or indents[from].indent + 1 },
                  to = { line = indents[to].line, col = #indents[to].text },
                }
              end
            end

            return ret
          end,
          g = function(ai_type)
            local start_line, end_line = 1, vim.fn.line("$")
            if ai_type == "i" then
              local first_nonblank, last_nonblank = vim.fn.nextnonblank(start_line), vim.fn.prevnonblank(end_line)
              if first_nonblank == 0 or last_nonblank == 0 then
                return { from = { line = start_line, col = 1 } }
              end
              start_line, end_line = first_nonblank, last_nonblank
            end

            local to_col = math.max(vim.fn.getline(end_line):len(), 1)
            return { from = { line = start_line, col = 1 }, to = { line = end_line, col = to_col } }
          end,
          u = ai.gen_spec.function_call(),
          U = ai.gen_spec.function_call({ name_pattern = "[%w_]" }),
        },
      }
    end,
  },

  {
    "echasnovski/mini.align",
    event = "VeryLazy",
    opts = {},
  },

  {
    "gbprod/yanky.nvim",
    dependencies = { { "kkharji/sqlite.lua", enabled = not Util.is_win } },
    event = "LazyFile",
    opts = {
      highlight = { timer = 200 },
      ring = { storage = Util.is_win and "shada" or "sqlite" },
    },
    keys = {
      {
        "<Leader>sy",
        function()
          if Util.pick.picker.name == "telescope" then
            require("telescope").extensions.yank_history.yank_history({})
          else
            vim.cmd([[YankyRingHistory]])
          end
        end,
        desc = "Open yank history",
      },
      { "y", "<Plug>(YankyYank)", mode = { "n", "x" }, desc = "Yank text" },
      { "p", "<Plug>(YankyPutAfter)", mode = { "n", "x" }, desc = "Put yanked text after cursor" },
      { "P", "<Plug>(YankyPutBefore)", mode = { "n", "x" }, desc = "Put yanked text before cursor" },
      { "gp", "<Plug>(YankyGPutAfter)", mode = { "n", "x" }, desc = "Put yanked text after selection" },
      { "gP", "<Plug>(YankyGPutBefore)", mode = { "n", "x" }, desc = "Put yanked text before selection" },
      { "<C-n>", "<Plug>(YankyCycleForward)", desc = "Cycle forward through yank history" },
      { "<C-p>", "<Plug>(YankyCycleBackward)", desc = "Cycle backward through yank history" },
      { "[y", "<Plug>(YankyCycleForward)", desc = "Cycle forward through yank history" },
      { "]y", "<Plug>(YankyCycleBackward)", desc = "Cycle backward through yank history" },
      { "]p", "<Plug>(YankyPutIndentAfterLinewise)", desc = "Put indented after cursor (linewise)" },
      { "[p", "<Plug>(YankyPutIndentBeforeLinewise)", desc = "Put indented before cursor (linewise)" },
      { "]P", "<Plug>(YankyPutIndentAfterLinewise)", desc = "Put indented after cursor (linewise)" },
      { "[P", "<Plug>(YankyPutIndentBeforeLinewise)", desc = "Put indented before cursor (linewise)" },
      { ">p", "<Plug>(YankyPutIndentAfterShiftRight)", desc = "Put and indent right" },
      { "<p", "<Plug>(YankyPutIndentAfterShiftLeft)", desc = "Put and indent left" },
      { ">P", "<Plug>(YankyPutIndentBeforeShiftRight)", desc = "Put before and indent right" },
      { "<P", "<Plug>(YankyPutIndentBeforeShiftLeft)", desc = "Put before and indent left" },
      { "=p", "<Plug>(YankyPutAfterFilter)", desc = "Put after applying a filter" },
      { "=P", "<Plug>(YankyPutBeforeFilter)", desc = "Put before applying a filter" },
    },
  },

  {
    "akinsho/toggleterm.nvim",
    version = "*",
    opts = {},
    keys = {
      { "<F7>", mode = { "n", "x", "o", "t" }, "<Cmd>ToggleTerm<CR>", desc = "Toggle terminal" },
      { "<Leader>utf", "<Cmd>ToggleTerm direction=float<CR>", desc = "Toggle terminal (float)" },
      { "<Leader>uth", "<Cmd>ToggleTerm size=10 direction=horizontal<CR>", desc = "Toggle terminal (horizontal)" },
      { "<Leader>utv", "<Cmd>ToggleTerm size=80 direction=vertical<CR>", desc = "Toggle terminal (vertical)" },
      { "<Leader>gl", Util.terminal.lazygit_toggle, desc = "Lazygit" },
    },
  },

  {
    "brenoprata10/nvim-highlight-colors",
    event = "LazyFile",
    opts = {
      render = "virtual",
      enable_tailwind = true,
    },
  },

  {
    "uga-rosa/ccc.nvim",
    cmd = {
      "CccConvert",
      "CccHighlighterDisable",
      "CccHighlighterEnable",
      "CccHighlighterToggle",
      "CccPick",
    },
    opts = {},
  },

  {
    "tpope/vim-sleuth",
    event = "LazyFile",
    cmd = "Sleuth",
    init = function()
      vim.g.sleuth_heuristics = 0
    end,
  },

  {
    "folke/lazydev.nvim",
    ft = "lua",
    opts = function()
      return {
        library = {
          uv = "luvit-meta/library",
        },
      }
    end,
  },

  { "Bilal2453/luvit-meta", lazy = true },

  {
    "hrsh7th/nvim-cmp",
    opts = function(_, opts)
      table.insert(opts.sources, { name = "lazydev", group_index = 0 })
    end,
  },
}
