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
    mason     = {},
    neotest   = {},   -- {name=, factory=}
    on_load   = {},   -- {plugin=, fn=}
    lsp_cfg   = {},   -- {name=, cfg=}
    lsp_enable = {},  -- {name=}
  }

  Lib.mason = {
    add = function(...)
      for _, t in ipairs({ ... }) do recorded.mason[#recorded.mason + 1] = t end
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
    enable = function(name) recorded.lsp_enable[#recorded.lsp_enable + 1] = name end,
  }

  -- Stub vim.lsp.config so the on_load callback can drive it. nvim-lspconfig
  -- isn't loaded in the test runner, so the real config() would error.
  local real_lsp_config = vim.lsp.config
  vim.lsp.config = setmetatable({}, {
    __call = function(_, name, cfg)
      recorded.lsp_cfg[#recorded.lsp_cfg + 1] = { name = name, cfg = cfg }
    end,
  })

  package.loaded["lib.lang"] = nil
  local ok, err = pcall(fn, recorded, require("lib.lang"))

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
        mason = { "tool1", "tool2" },
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
      T.eq(rec.lsp_enable, { "server1" })
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
      local on_load_fn
      for _, e in ipairs(rec.on_load) do
        if e.plugin == "nvim-lspconfig" then on_load_fn = e.fn end
      end
      on_load_fn()
      -- vim.tbl_deep_extend("force", caps_from_lib, user_cfg) — user wins
      T.eq(rec.lsp_cfg[1].cfg.capabilities, { stub = true, user = "value" })
    end)
  end)
end)
