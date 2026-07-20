local T = require("tests.helpers")

-- Snapshot the real Lib submodules so the suite can install mocks per test
-- and restore afterward. Each test resets the lib.lang module so the
-- closures inside setup() see the current mocks.
local real = {
  mason   = Lib.mason,
  parsers = Lib.parsers,
  neotest = Lib.neotest,
  plugin  = Lib.plugin,
  lsp     = Lib.lsp,
}

local function with_mocks(fn)
  local recorded = {
    mason       = {},   -- list of tool names
    parsers     = {},   -- list of parser names
    neotest     = {},   -- {name=, factory=}
    on_load     = {},   -- {plugin=, fn=}
    lsp_cfg     = {},   -- {name=, cfg=}
    lsp_enable  = {},   -- {name=, opts=}
    lsp_attach  = {},   -- {fn=}
  }

  Lib.mason = {
    -- New API: variadic names, trailing opts table with ft.
    -- Record only the string names so existing assertions (which check
    -- recorded.mason against a list of names) keep passing.
    add = function(...)
      for _, t in ipairs({ ... }) do
        if type(t) == "string" then
          recorded.mason[#recorded.mason + 1] = t
        end
      end
    end,
  }
  Lib.parsers = {
    add = function(...)
      for _, t in ipairs({ ... }) do
        if type(t) == "string" then
          recorded.parsers[#recorded.parsers + 1] = t
        end
      end
    end,
  }
  Lib.neotest = {
    add = function(name, factory)
      recorded.neotest[#recorded.neotest + 1] = { name = name, factory = factory }
    end,
  }
  Lib.plugin = {
    on_load = function(name, cb)
      recorded.on_load[#recorded.on_load + 1] = { plugin = name, fn = cb }
    end,
  }
  Lib.lsp = {
    capabilities = function() return { stub = true } end,
    enable = function(name, opts)
      recorded.lsp_enable[#recorded.lsp_enable + 1] = { name = name, opts = opts }
    end,
    on_attach = function(name_or_fn, fn)
      if type(name_or_fn) == "function" then
        recorded.lsp_attach[#recorded.lsp_attach + 1] = { fn = name_or_fn }
      else
        recorded.lsp_attach[#recorded.lsp_attach + 1] = { name = name_or_fn, fn = fn }
      end
    end,
  }

  -- Stub vim.lsp.config so the on_load callback can drive it. nvim-lspconfig
  -- isn't loaded in the test runner, so the real config() would error.
  local real_lsp_config = vim.lsp.config
  vim.lsp.config = setmetatable({}, {
    __call = function(_, name, cfg)
      recorded.lsp_cfg[#recorded.lsp_cfg + 1] = { name = name, cfg = cfg }
    end,
  })

  -- Stub require("conform") and require("lint") so on_load callbacks for
  -- those plugins can run without the actual plugins being installed.
  local conform_stub = { formatters_by_ft = {}, formatters = {} }
  local lint_stub = { linters_by_ft = {} }
  local prev_conform = package.loaded["conform"]
  local prev_lint = package.loaded["lint"]
  package.loaded["conform"] = conform_stub
  package.loaded["lint"] = lint_stub
  recorded.conform = conform_stub
  recorded.lint = lint_stub

  -- Helper: drive a recorded on_load callback by plugin name.
  function recorded:drive(plugin)
    for _, e in ipairs(self.on_load) do
      if e.plugin == plugin then e.fn() end
    end
  end

  package.loaded["lib.lang"] = nil
  local ok, err = pcall(fn, recorded, require("lib.lang"))

  package.loaded["conform"] = prev_conform
  package.loaded["lint"] = prev_lint
  vim.lsp.config = real_lsp_config
  Lib.mason = real.mason
  Lib.parsers = real.parsers
  Lib.neotest = real.neotest
  Lib.plugin = real.plugin
  Lib.lsp = real.lsp

  if not ok then error(err) end
end

T.describe("lib.lang.setup", function()
  T.it("returns {} and skips registration when cmd is not executable", function()
    with_mocks(function(rec, lang)
      local plugins = lang.setup({
        cmd = "definitely-not-a-real-binary-9f3c",
        mason = { "x" },
        parsers = { "x" },
        servers = { x = {} },
        plugins = { { "user/plug" } },
      })
      T.eq(plugins, {})
      T.eq(rec.mason, {})
      T.eq(#rec.on_load, 0)
    end)
  end)

  T.it("registers mason tools, parsers, servers, formatters, and neotest", function()
    with_mocks(function(rec, lang)
      lang.setup({
        ft      = "lang1",
        mason   = { "tool1", "tool2" },
        parsers = { "lang1", "lang2" },
        servers = { server1 = { settings = { x = 1 } } },
        formatters = { lang1 = { "fmt1" } },
        neotest = { ["adapter1"] = function() return "stub" end },
      })

      T.eq(rec.mason, { "tool1", "tool2" })
      T.eq(rec.parsers, { "lang1", "lang2" })
      T.eq(#rec.neotest, 1)
      T.eq(rec.neotest[1].name, "adapter1")

      -- on_load entries: nvim-lspconfig, conform.nvim. nvim-treesitter
      -- is only registered when parsers_setup is set (covered by a
      -- separate test); plain parsers list goes to Lib.parsers.add.
      local plugins_seen = {}
      for _, e in ipairs(rec.on_load) do plugins_seen[e.plugin] = e.fn end
      T.truthy(plugins_seen["nvim-lspconfig"], "missing nvim-lspconfig on_load")
      T.truthy(plugins_seen["conform.nvim"], "missing conform.nvim on_load")

      -- Drive the lspconfig callback and confirm capabilities + settings merge
      plugins_seen["nvim-lspconfig"]()
      T.eq(#rec.lsp_cfg, 1)
      T.eq(rec.lsp_cfg[1].name, "server1")
      T.eq(rec.lsp_cfg[1].cfg.capabilities, { stub = true })
      T.eq(rec.lsp_cfg[1].cfg.settings, { x = 1 })
      T.eq(rec.lsp_enable, { { name = "server1", opts = nil } })
    end)
  end)

  T.it("returns spec.plugins verbatim", function()
    with_mocks(function(_, lang)
      local specs = { { "owner/a" }, { "owner/b" } }
      local plugins = lang.setup({ plugins = specs })
      T.eq(plugins, specs)
    end)
  end)

  -- Regression: lang/<x>.lua files using `return Lib.lang.setup(...)`
  -- get tail-call-eliminated by Lua, removing the chunk frame from the
  -- stack. caller_ft must still derive ft via the surviving
  -- config/plugins.lua loader frame.
  T.it("derives ft from config/plugins.lua loader name when chunk frame is TCO'd", function()
    with_mocks(function(rec, lang)
      -- Build the real-world stack shape:
      --   * `append(name)` lives in config/plugins.lua (frame survives —
      --     pcall is not in tail position so caller_ft can find it
      --     and read the `name` local).
      --   * The chunk it pcalls comes from lang/<x>.lua and tail-calls
      --     lang.setup, which is what eliminates the chunk frame.
      local loader_src = "@" .. vim.fn.stdpath("config") .. "/lua/config/plugins.lua"
      local chunk_src  = "@" .. vim.fn.stdpath("config") .. "/lua/config/plugins/lang/x_test.lua"
      local lang_chunk = assert(load(
        "local setup = ...; return setup({ mason = { 'tool-x' } })",
        chunk_src
      ))
      local make_append = assert(load([[
        local lang_chunk, lang_setup = ...
        return function(name)
          local _ok = pcall(lang_chunk, lang_setup)
          return _ok
        end
      ]], loader_src))
      local append = make_append(lang_chunk, lang.setup)
      append("lang.x_test")
      T.truthy(vim.tbl_contains(rec.mason, "tool-x"))
    end)
  end)

  T.it("server config supplied capabilities are overridden by Lib.lsp.capabilities()", function()
    with_mocks(function(rec, lang)
      lang.setup({
        servers = { srv = { capabilities = { user = "value" } } },
      })
      rec:drive("nvim-lspconfig")
      -- vim.tbl_deep_extend("force", caps_from_lib, user_cfg) — user wins
      T.eq(rec.lsp_cfg[1].cfg.capabilities, { stub = true, user = "value" })
    end)
  end)

  T.it("binary is forwarded to Lib.lsp.enable and stripped from vim.lsp.config", function()
    with_mocks(function(rec, lang)
      lang.setup({
        servers = { srv = { binary = "real-binary", settings = { x = 1 } } },
      })
      rec:drive("nvim-lspconfig")
      T.eq(rec.lsp_enable, { { name = "srv", opts = { cmd = "real-binary" } } })
      T.eq(rec.lsp_cfg[1].cfg.binary, nil, "binary should be stripped from lsp config")
      T.eq(rec.lsp_cfg[1].cfg.settings, { x = 1 })
    end)
  end)

  T.it("on_attach is registered name-keyed and stripped from lsp config", function()
    local fired_for = {}
    with_mocks(function(rec, lang)
      lang.setup({
        servers = {
          srv1 = {
            on_attach = function(_, client) fired_for[#fired_for + 1] = client.name end,
          },
        },
      })
      rec:drive("nvim-lspconfig")
      T.eq(rec.lsp_cfg[1].cfg.on_attach, nil, "on_attach should be stripped")
      T.eq(#rec.lsp_attach, 1, "should register one LspAttach callback")
      T.eq(rec.lsp_attach[1].name, "srv1", "callback should be name-keyed for filter dispatch")
      -- Drive the registered callback to verify it forwards (args, client)
      -- to the user's on_attach. Name filtering itself is the dispatcher's
      -- job (Lib.lsp._fire_attach), tested in lib/lsp_spec.
      rec.lsp_attach[1].fn({ data = { client_id = 1 }, buf = 0 }, { name = "srv1" })
      T.eq(fired_for, { "srv1" })
    end)
  end)

  T.it("formatters_setup runs inside the conform.nvim on_load with the conform module", function()
    local conform_seen
    with_mocks(function(rec, lang)
      lang.setup({
        formatters = { foo = { "fmt-foo" } },
        formatters_setup = function(conform)
          conform_seen = conform
          conform.formatters["custom"] = { command = "x" }
        end,
      })
      rec:drive("conform.nvim")
      T.truthy(conform_seen, "formatters_setup should have been called")
      T.eq(rec.conform.formatters_by_ft.foo, { "fmt-foo" })
      T.eq(rec.conform.formatters.custom, { command = "x" })
    end)
  end)

  T.it("parsers list goes to Lib.parsers, parsers_setup runs on nvim-treesitter load", function()
    with_mocks(function(rec, lang)
      local setup_fired = false
      lang.setup({
        ft            = "nu",
        parsers       = { "nu" },
        parsers_setup = function() setup_fired = true end,
      })
      -- Parsers list went to Lib.parsers (recorded by mock), not to install.
      T.eq(rec.parsers, { "nu" })
      -- parsers_setup is registered as a nvim-treesitter on_load callback.
      rec:drive("nvim-treesitter")
      T.eq(setup_fired, true)
    end)
  end)

  T.it("parsers_setup runs even when no parsers list is provided", function()
    local fired = false
    with_mocks(function(rec, lang)
      lang.setup({ parsers_setup = function() fired = true end })
      local prev_ts = package.loaded["nvim-treesitter"]
      package.loaded["nvim-treesitter"] = {
        install = function() error("install should not be called without parsers") end,
      }
      rec:drive("nvim-treesitter")
      package.loaded["nvim-treesitter"] = prev_ts
      T.truthy(fired, "parsers_setup should fire")
    end)
  end)

  T.it("server cfg can be a function evaluated lazily inside lspconfig on_load", function()
    with_mocks(function(rec, lang)
      local resolved_at_eval_time = false
      lang.setup({
        servers = {
          srv = function()
            resolved_at_eval_time = true
            return { settings = { dynamic = "ok" }, binary = "x" }
          end,
        },
      })
      T.eq(resolved_at_eval_time, false, "server fn should not run at setup time")
      rec:drive("nvim-lspconfig")
      T.eq(resolved_at_eval_time, true, "server fn should run inside on_load")
      T.eq(rec.lsp_cfg[1].cfg.settings, { dynamic = "ok" })
      T.eq(rec.lsp_cfg[1].cfg.binary, nil)
      T.eq(rec.lsp_enable, { { name = "srv", opts = { cmd = "x" } } })
    end)
  end)

  T.it("linters register linters_by_ft via nvim-lint on_load", function()
    with_mocks(function(rec, lang)
      lang.setup({
        linters = {
          markdown = { "markdownlint-cli2" },
          ["yaml.ansible"] = { "ansible_lint" },
        },
      })
      local linter_load
      for _, e in ipairs(rec.on_load) do
        if e.plugin == "nvim-lint" then linter_load = e.fn end
      end
      T.truthy(linter_load, "missing nvim-lint on_load")
      linter_load()
      T.eq(rec.lint.linters_by_ft.markdown, { "markdownlint-cli2" })
      T.eq(rec.lint.linters_by_ft["yaml.ansible"], { "ansible_lint" })
    end)
  end)
end)
