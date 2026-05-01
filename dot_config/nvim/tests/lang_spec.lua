local T = require("tests.helpers")

-- Snapshot the real Lib submodules so the suite can install mocks per test
-- and restore afterward. Each test resets the lib.lang module so the
-- closures inside setup() see the current mocks.
local real = {
  mason   = Lib.mason,
  neotest = Lib.neotest,
  plugin  = Lib.plugin,
  lsp     = Lib.lsp,
}

local function with_mocks(fn)
  local recorded = {
    mason       = {},
    neotest     = {},   -- {name=, factory=}
    on_load     = {},   -- {plugin=, fn=}
    lsp_cfg     = {},   -- {name=, cfg=}
    lsp_enable  = {},   -- {name=, opts=}
    lsp_attach  = {},   -- {fn=}
  }

  Lib.mason = {
    -- New API: variadic names, optional trailing opts table.
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
    on_attach = function(cb)
      recorded.lsp_attach[#recorded.lsp_attach + 1] = cb
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
      T.eq(#rec.neotest, 1)
      T.eq(rec.neotest[1].name, "adapter1")

      -- on_load entries: nvim-treesitter, nvim-lspconfig, conform.nvim
      local plugins_seen = {}
      for _, e in ipairs(rec.on_load) do plugins_seen[e.plugin] = e.fn end
      T.truthy(plugins_seen["nvim-treesitter"], "missing nvim-treesitter on_load")
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

  T.it("_enable is forwarded to Lib.lsp.enable and stripped from vim.lsp.config", function()
    with_mocks(function(rec, lang)
      lang.setup({
        servers = { srv = { _enable = { cmd = "real-binary" }, settings = { x = 1 } } },
      })
      rec:drive("nvim-lspconfig")
      T.eq(rec.lsp_enable, { { name = "srv", opts = { cmd = "real-binary" } } })
      T.eq(rec.lsp_cfg[1].cfg._enable, nil, "_enable should be stripped from lsp config")
      T.eq(rec.lsp_cfg[1].cfg.settings, { x = 1 })
    end)
  end)

  T.it("_on_attach fires only for the matching server and is stripped from lsp config", function()
    local fired_for = {}
    with_mocks(function(rec, lang)
      lang.setup({
        servers = {
          srv1 = {
            _on_attach = function(_, client) fired_for[#fired_for + 1] = client.name end,
          },
        },
      })
      rec:drive("nvim-lspconfig")
      T.eq(rec.lsp_cfg[1].cfg._on_attach, nil, "_on_attach should be stripped")
      T.eq(#rec.lsp_attach, 1, "should register one LspAttach callback")

      -- Stub vim.lsp.get_client_by_id, then drive the attach callback twice:
      -- once with a matching client, once with a different client name.
      local real_get = vim.lsp.get_client_by_id
      local current_client
      vim.lsp.get_client_by_id = function() return current_client end

      current_client = { name = "srv1" }
      rec.lsp_attach[1]({ data = { client_id = 1 }, buf = 0 })

      current_client = { name = "other" }
      rec.lsp_attach[1]({ data = { client_id = 2 }, buf = 0 })

      vim.lsp.get_client_by_id = real_get
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

  T.it("parsers_setup runs before install inside the nvim-treesitter on_load", function()
    local order = {}
    with_mocks(function(rec, lang)
      lang.setup({
        parsers = { "nu" },
        parsers_setup = function() order[#order + 1] = "setup" end,
      })
      -- Stub require("nvim-treesitter") so .install() can be observed.
      local prev_ts = package.loaded["nvim-treesitter"]
      package.loaded["nvim-treesitter"] = {
        install = function(p)
          order[#order + 1] = "install"
          T.eq(p, { "nu" })
        end,
      }
      rec:drive("nvim-treesitter")
      package.loaded["nvim-treesitter"] = prev_ts
      T.eq(order, { "setup", "install" })
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
            return { settings = { dynamic = "ok" }, _enable = { cmd = "x" } }
          end,
        },
      })
      T.eq(resolved_at_eval_time, false, "server fn should not run at setup time")
      rec:drive("nvim-lspconfig")
      T.eq(resolved_at_eval_time, true, "server fn should run inside on_load")
      T.eq(rec.lsp_cfg[1].cfg.settings, { dynamic = "ok" })
      T.eq(rec.lsp_cfg[1].cfg._enable, nil)
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
