-- lua/config/plugins/treesitter.lua
--
-- nvim-treesitter main branch (active, Nvim 0.12+). master is the locked
-- legacy branch for Nvim 0.11 — it has the iter_matches API mismatch.
-- main dropped the configs module; highlighting/folds/indent are wired
-- per-filetype via native vim.treesitter APIs.

return {
  {
    "nvim-treesitter/nvim-treesitter",
    name = "nvim-treesitter",
    branch = "main",
    lazy = false,  -- main explicitly does NOT support lazy loading
    -- No `build`: `require("nvim-treesitter").install(...)` in config
    -- below already handles install/update on every nvim launch.
    init = function()
      -- main branch compiles parsers on-demand and requires tree-sitter-cli
      -- (>= 0.26.1) on PATH. mason's registry has it.
      Lib.mason.add("tree-sitter-cli")
      -- Clean stale tree-sitter-<lang>-tmp dirs from interrupted prior
      -- installs. nvim-treesitter "main" doesn't clean these on startup,
      -- so a previously-interrupted install leaves EEXIST + truncated-
      -- gzip errors that make every subsequent launch retry-and-fail.
      -- vim.fn.delete doesn't take globs; expand first, delete each.
      local cache = vim.fn.stdpath("cache")
      for _, dir in ipairs(vim.fn.glob(cache .. "/tree-sitter-*-tmp", false, true)) do
        vim.fn.delete(dir, "rf")
      end
    end,
    config = function()
      -- Reroute nvim-treesitter's info-level logger from nvim_echo
      -- (which bursts to the cmdline and triggers press-enter when
      -- the install queues many parsers in succession) to whatever
      -- surface is most appropriate:
      --   - Splash open  → splash status line (cold-install context)
      --   - Splash closed → vim.notify (toast) so on-demand parser
      --     installs (e.g., first time opening a Python buffer)
      --     give visible feedback
      -- warn/error are untouched so real failures still surface via
      -- their normal echo path.
      local ts_log = require("nvim-treesitter.log")
      local UI = require("core.pack.ui")
      ts_log.Logger.info = function(self, m, ...)
        local text = ("[%s] %s"):format(self.ctx or "?", m:format(...))
        if UI._active_splash then
          UI._active_splash:set_status_text(text)
        else
          vim.notify(text, vim.log.levels.INFO)
        end
      end

      require("nvim-treesitter").setup({
        install_dir = vim.fn.stdpath("data") .. "/site",
      })

      -- Parsers we care about. Treated as a whitelist: a buffer with
      -- filetype X gets treesitter only if X is in this list. Restricts
      -- on-demand install to languages we actually use.
      local WANTED = {
        bash = true, c = true, cpp = true, css = true, html = true,
        javascript = true, json = true, lua = true, luadoc = true,
        markdown = true, markdown_inline = true, python = true,
        query = true, regex = true, rust = true, styled = true,
        toml = true, tsx = true, typescript = true, vim = true,
        vimdoc = true, yaml = true,
      }

      -- Per-lang install state to avoid duplicate concurrent installs.
      -- "installing": install task started, not yet known to be done
      -- "installed":  parser file exists on disk, ready to use
      -- nil:          not yet attempted
      local install_state = {}
      do
        local ts_config = require("nvim-treesitter.config")
        for _, lang in ipairs(ts_config.get_installed()) do
          install_state[lang] = "installed"
        end
      end

      -- Start treesitter on the buffer. Safe to call multiple times.
      local function start_for_buf(bufnr)
        if not vim.api.nvim_buf_is_valid(bufnr) then return end
        if vim.bo[bufnr].filetype == "bigfile" then return end
        pcall(vim.treesitter.start, bufnr)
        pcall(function()
          vim.wo[0][0].foldexpr   = "v:lua.vim.treesitter.foldexpr()"
          vim.wo[0][0].foldmethod = "expr"
        end)
        pcall(function()
          vim.bo[bufnr].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
        end)
      end

      -- On every FileType, install the parser if needed (async), and
      -- start treesitter when the parser becomes available. The first
      -- buffer of a given filetype triggers the install; subsequent
      -- buffers either start immediately (already installed) or wait
      -- for the in-flight install via state tracking.
      vim.api.nvim_create_autocmd("FileType", {
        group = vim.api.nvim_create_augroup("Lib.treesitter.highlight", { clear = true }),
        callback = function(args)
          local bufnr = args.buf
          local ft = vim.bo[bufnr].filetype
          if ft == "" or ft == "bigfile" or not WANTED[ft] then return end

          local state = install_state[ft]
          if state == "installed" then
            start_for_buf(bufnr)
            return
          end
          if state == "installing" then
            -- An earlier buffer kicked off the install; the completion
            -- callback already has logic to reapply to all matching
            -- bufs. Nothing to do here.
            return
          end

          install_state[ft] = "installing"
          local task = require("nvim-treesitter").install({ ft })
          if task and type(task.await) == "function" then
            task:await(function()
              vim.schedule(function()
                install_state[ft] = "installed"
                -- Apply to every loaded buffer of this filetype, not
                -- just the one that triggered (more buffers may have
                -- been opened while we were installing).
                for _, b in ipairs(vim.api.nvim_list_bufs()) do
                  if vim.api.nvim_buf_is_loaded(b) and vim.bo[b].filetype == ft then
                    start_for_buf(b)
                  end
                end
              end)
            end)
          else
            -- Older/unexpected API shape: assume sync, mark installed.
            install_state[ft] = "installed"
            start_for_buf(bufnr)
          end
        end,
      })
    end,
  },

  -- Textobjects — has a main branch that pairs with nvim-treesitter main
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    name = "nvim-treesitter-textobjects",
    branch = "main",
    lazy = false,
    dependencies = { "nvim-treesitter" },
    -- Note: af/if/ac/ic/aa/ia are provided by mini.ai (see editor.lua)
    -- via treesitter gen_spec; only jump motions live here.
    keys = {
      { "]f", function() require("nvim-treesitter-textobjects.move").goto_next_start("@function.outer", "textobjects") end,     desc = "Next function start" },
      { "[f", function() require("nvim-treesitter-textobjects.move").goto_previous_start("@function.outer", "textobjects") end, desc = "Prev function start" },
      { "]c", function() require("nvim-treesitter-textobjects.move").goto_next_start("@class.outer", "textobjects") end,        desc = "Next class start" },
      { "[c", function() require("nvim-treesitter-textobjects.move").goto_previous_start("@class.outer", "textobjects") end,    desc = "Prev class start" },
    },
  },

  {
    "nvim-treesitter/nvim-treesitter-context",
    name = "nvim-treesitter-context",
    event = "LazyFile",
    opts = {
      max_lines = 3,
      trim_scope = "outer",
      mode = "cursor",
    },
  },

  {
    "windwp/nvim-ts-autotag",
    name = "nvim-ts-autotag",
    ft = { "html", "xml", "jsx", "tsx", "vue", "svelte", "astro", "markdown" },
    opts = {},
  },
}
