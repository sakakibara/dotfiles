local picker = {
  name = "fzf",
  commands = {
    files = "files",
  },

  open = function(command, opts)
    opts = opts or {}
    if opts.cmd == nil and command == "git_files" and opts.show_untracked then
      opts.cmd = "git ls-files --exclude-standard --cached --others"
    end
    return require("fzf-lua")[command](opts)
  end,
}
if not Util.pick.register(picker) then
  return {}
end

local function symbols_filter(entry, ctx)
  if ctx.symbols_filter == nil then
    ctx.symbols_filter = Util.config.get_kind_filter(ctx.bufnr) or false
  end
  if ctx.symbols_filter == false then
    return true
  end
  return vim.tbl_contains(ctx.symbols_filter, entry.kind)
end

return {
  {
    "ibhagwan/fzf-lua",
    cmd = "FzfLua",
    opts = function()
      local config = require("fzf-lua.config")
      local actions = require("fzf-lua.actions")

      config.defaults.keymap.fzf["ctrl-q"] = "select-all+accept"
      config.defaults.keymap.fzf["ctrl-x"] = "jump"
      config.defaults.keymap.fzf["ctrl-b"] = "backward-char"
      config.defaults.keymap.fzf["ctrl-f"] = "forward-char"
      config.defaults.keymap.fzf["ctrl-u"] = "unix-line-discard"
      config.defaults.keymap.fzf["ctrl-d"] = "delete-char/eof"

      if Util.plugin.has("trouble.nvim") then
        config.defaults.actions.files["ctrl-t"] = require("trouble.sources.fzf").actions.open
      end

      config.defaults.actions.files["ctrl-r"] = function(_, ctx)
        local o = vim.deepcopy(ctx.__call_opts)
        o.root = o.root == false
        o.cwd = nil
        o.buf = ctx.__CTX.bufnr
        Util.pick.open(ctx.__INFO.cmd, o)
      end
      config.defaults.actions.files["alt-c"] = config.defaults.actions.files["ctrl-r"]
      config.set_action_helpstr(config.defaults.actions.files["ctrl-r"], "toggle-root-dir")

      -- use the same prompt for all
      local defaults = require("fzf-lua.profiles.default-title")
      local function fix(t)
        t.prompt = t.prompt ~= nil and " " or nil
        for _, v in pairs(t) do
          if type(v) == "table" then
            fix(v)
          end
        end
      end
      fix(defaults)

      local img_previewer ---@type string[]?
      for _, v in ipairs({
        { cmd = "ueberzug", args = {} },
        { cmd = "chafa", args = { "{file}", "--format=symbols" } },
        { cmd = "viu", args = { "-b" } },
      }) do
        if vim.fn.executable(v.cmd) == 1 then
          img_previewer = vim.list_extend({ v.cmd }, v.args)
          break
        end
      end

      return vim.tbl_deep_extend("force", defaults, {
        fzf_colors = true,
        fzf_opts = {
          ["--no-scrollbar"] = true,
        },
        defaults = {
          -- formatter = "path.filename_first",
          formatter = "path.dirname_first",
        },
        previewers = {
          builtin = {
            extensions = {
              ["png"] = img_previewer,
              ["jpg"] = img_previewer,
              ["jpeg"] = img_previewer,
              ["gif"] = img_previewer,
              ["webp"] = img_previewer,
            },
            ueberzug_scaler = "fit_contain",
          },
        },
        ui_select = function(fzf_opts, items)
          return vim.tbl_deep_extend("force", fzf_opts, {
            prompt = " ",
            winopts = {
              title = " " .. vim.trim((fzf_opts.prompt or "Select"):gsub("%s*:%s*$", "")) .. " ",
              title_pos = "center",
            },
          }, fzf_opts.kind == "codeaction" and {
            winopts = {
              layout = "vertical",
              -- height is number of items minus 15 lines for the preview, with a max of 80% screen height
              height = math.floor(math.min(vim.o.lines * 0.8 - 16, #items + 2) + 0.5) + 16,
              width = 0.5,
              preview = not vim.tbl_isempty(Util.lsp.get_clients({ bufnr = 0, name = "vtsls" })) and {
                layout = "vertical",
                vertical = "down:15,border-top",
                hidden = "hidden",
              } or {
                layout = "vertical",
                vertical = "down:15,border-top",
              },
            },
          } or {
            winopts = {
              width = 0.5,
              -- height is number of items, with a max of 80% screen height
              height = math.floor(math.min(vim.o.lines * 0.8, #items + 2) + 0.5),
            },
          })
        end,
        winopts = {
          width = 0.8,
          height = 0.8,
          row = 0.5,
          col = 0.5,
          preview = {
            scrollchars = { "┃", "" },
          },
        },
        files = {
          cwd_prompt = false,
          actions = {
            ["alt-i"] = { actions.toggle_ignore },
            ["alt-h"] = { actions.toggle_hidden },
          },
        },
        grep = {
          actions = {
            ["alt-i"] = { actions.toggle_ignore },
            ["alt-h"] = { actions.toggle_hidden },
          },
        },
        lsp = {
          symbols = {
            symbol_hl = function(s)
              return "TroubleIcon" .. s
            end,
            symbol_fmt = function(s)
              return s:lower() .. "\t"
            end,
            child_prefix = false,
          },
          code_actions = {
            previewer = vim.fn.executable("delta") == 1 and "codeaction_native" or nil,
          },
        },
      })
    end,
    config = function(_, opts)
      require("fzf-lua").setup(opts)
    end,
    init = function()
      Util.plugin.on_very_lazy(function()
        ---@diagnostic disable-next-line: duplicate-set-field
        vim.ui.select = function(...)
          require("lazy").load({ plugins = { "fzf-lua" } })
          local opts = Util.plugin.opts("fzf-lua") or {}
          require("fzf-lua").register_ui_select(opts.ui_select or nil)
          return vim.ui.select(...)
        end
      end)
    end,
    keys = {
      { "<C-j>", "<C-j>", ft = "fzf", mode = "t", nowait = true },
      { "<C-k>", "<C-k>", ft = "fzf", mode = "t", nowait = true },
      {
        "<Leader>,",
        "<Cmd>FzfLua buffers sort_mru=true sort_lastused=true<CR>",
        desc = "Switch Buffer",
      },
      { "<Leader>/", Util.pick("live_grep"), desc = "Grep (root)" },
      { "<Leader>:", "<Cmd>FzfLua command_history<CR>", desc = "Command history" },
      { "<Leader><space>", Util.pick("auto"), desc = "Find files (root)" },
      { "<Leader>fb", "<Cmd>FzfLua buffers sort_mru=true sort_lastused=true<CR>", desc = "Buffers" },
      { "<Leader>fc", Util.pick.config_files(), desc = "Find config file" },
      { "<Leader>ff", Util.pick("auto"), desc = "Find files (root)" },
      { "<Leader>fF", Util.pick("auto", { root = false }), desc = "Find files (cwd)" },
      { "<Leader>fg", "<Cmd>FzfLua git_files<CR>", desc = "Find files (git files)" },
      { "<Leader>fr", "<Cmd>FzfLua oldfiles<CR>", desc = "Recent" },
      { "<Leader>fR", Util.pick("oldfiles", { cwd = vim.uv.cwd() }), desc = "Recent (cwd)" },
      { "<Leader>gc", "<Cmd>FzfLua git_commits<CR>", desc = "Commits" },
      { "<Leader>gs", "<Cmd>FzfLua git_status<CR>", desc = "Status" },
      { '<Leader>s"', "<Cmd>FzfLua registers<CR>", desc = "Registers" },
      { "<Leader>sa", Util.pick("live_grep_glob"), desc = "Grep (root)" },
      { "<Leader>sA", Util.pick("live_grep_glob", { root = false }), desc = "Grep (cwd)" },
      { "<Leader>sb", "<Cmd>FzfLua grep_curbuf<CR>", desc = "Buffer" },
      { "<Leader>sc", "<Cmd>FzfLua command_history<CR>", desc = "Command history" },
      { "<Leader>sC", "<Cmd>FzfLua commands<CR>", desc = "Commands" },
      { "<Leader>sd", "<Cmd>FzfLua diagnostics_document<CR>", desc = "Document diagnostics" },
      { "<Leader>sD", "<Cmd>FzfLua diagnostics_workspace<CR>", desc = "Workspace diagnostics" },
      { "<Leader>sg", Util.pick("live_grep"), desc = "Grep (root)" },
      { "<Leader>sG", Util.pick("live_grep", { root = false }), desc = "Grep (cwd)" },
      { "<Leader>sh", "<Cmd>FzfLua help_tags<CR>", desc = "Help pages" },
      { "<Leader>sH", "<Cmd>FzfLua highlights<CR>", desc = "Search highlight groups" },
      { "<Leader>sj", "<Cmd>FzfLua jumps<CR>", desc = "Jumplist" },
      { "<Leader>sk", "<Cmd>FzfLua keymaps<CR>", desc = "Key maps" },
      { "<Leader>sl", "<Cmd>FzfLua loclist<CR>", desc = "Location list" },
      { "<Leader>sM", "<Cmd>FzfLua man_pages<CR>", desc = "Man pages" },
      { "<Leader>sm", "<Cmd>FzfLua marks<CR>", desc = "Jump to mark" },
      { "<Leader>sR", "<Cmd>FzfLua resume<CR>", desc = "Resume" },
      { "<Leader>sq", "<Cmd>FzfLua quickfix<CR>", desc = "Quickfix list" },
      { "<Leader>sw", Util.pick("grep_cword"), desc = "Word (root)" },
      { "<Leader>sW", Util.pick("grep_cword", { root = false }), desc = "Word (cwd)" },
      { "<Leader>sw", Util.pick("grep_visual"), mode = "v", desc = "Selection (root)" },
      { "<Leader>sW", Util.pick("grep_visual", { root = false }), mode = "v", desc = "Selection (cwd)" },
      { "<Leader>uC", Util.pick("colorschemes"), desc = "Colorscheme with preview" },
      {
        "<Leader>ss",
        function()
          require("fzf-lua").lsp_document_symbols({
            regex_filter = symbols_filter,
          })
        end,
        desc = "Goto Symbol",
      },
      {
        "<Leader>sS",
        function()
          require("fzf-lua").lsp_live_workspace_symbols({
            regex_filter = symbols_filter,
          })
        end,
        desc = "Goto Symbol (Workspace)",
      },
    },
  },

  {
    "folke/todo-comments.nvim",
    optional = true,
    keys = {
      {
        "<Leader>st",
        function()
          require("todo-comments.fzf").todo()
        end,
        desc = "Todo",
      },
      {
        "<Leader>sT",
        function()
          require("todo-comments.fzf").todo({ keywords = { "TODO", "FIX", "FIXME" } })
        end,
        desc = "Todo/Fix/Fixme",
      },
    },
  },

  {
    "neovim/nvim-lspconfig",
    opts = function()
      local Keys = require("plugins.lsp.keymaps").get()
      vim.list_extend(Keys, {
        {
          "gd",
          "<Cmd>FzfLua lsp_definitions jump_to_single_result=true ignore_current_line=true<CR>",
          desc = "Goto definition",
          has = "definition",
        },
        {
          "gr",
          "<Cmd>FzfLua lsp_references jump_to_single_result=true ignore_current_line=true<CR>",
          desc = "References",
          nowait = true,
        },
        {
          "gI",
          "<Cmd>FzfLua lsp_implementations jump_to_single_result=true ignore_current_line=true<CR>",
          desc = "Goto implementation",
        },
        {
          "gy",
          "<Cmd>FzfLua lsp_typedefs jump_to_single_result=true ignore_current_line=true<CR>",
          desc = "Goto t[y]pe definition",
        },
      })
    end,
  },
}
