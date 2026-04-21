local T = require("tests.helpers")

local function reset_format()
  package.loaded["lib.format"] = nil
  vim.g.autoformat = nil
  local fmt = require("lib.format")
  return fmt
end

local function clear_buffer_var(buf)
  pcall(function() vim.b[buf or 0].autoformat = nil end)
end

T.describe("lib.format — registration + resolution", function()
  T.it("register adds a formatter source", function()
    local fmt = reset_format()
    fmt.register({
      name = "t1", primary = true, priority = 100,
      format = function() end,
      sources = function() return { "t1-src" } end,
    })
    local resolved = fmt.resolve(vim.api.nvim_get_current_buf())
    local found = false
    for _, f in ipairs(resolved) do if f.name == "t1" then found = true end end
    T.truthy(found, "registered formatter not in resolve()")
  end)

  T.it("only one primary formatter is active at a time", function()
    local fmt = reset_format()
    fmt.register({
      name = "hi",  primary = true,  priority = 200,
      format = function() end,
      sources = function() return { "hi-src" } end,
    })
    fmt.register({
      name = "lo",  primary = true,  priority = 50,
      format = function() end,
      sources = function() return { "lo-src" } end,
    })
    local active = {}
    for _, f in ipairs(fmt.resolve(0)) do
      if f.active then active[#active + 1] = f.name end
    end
    T.eq(active, { "hi" })
  end)

  T.it("non-primary formatters can both be active", function()
    local fmt = reset_format()
    fmt.register({
      name = "p", primary = true, priority = 100,
      format = function() end,
      sources = function() return { "p-src" } end,
    })
    fmt.register({
      name = "np1", primary = false, priority = 50,
      format = function() end,
      sources = function() return { "np1-src" } end,
    })
    fmt.register({
      name = "np2", primary = false, priority = 10,
      format = function() end,
      sources = function() return { "np2-src" } end,
    })
    local active = {}
    for _, f in ipairs(fmt.resolve(0)) do
      if f.active then active[#active + 1] = f.name end
    end
    table.sort(active)
    T.eq(active, { "np1", "np2", "p" })
  end)

  T.it("formatter with empty sources is not active", function()
    local fmt = reset_format()
    fmt.register({
      name = "empty", primary = true, priority = 100,
      format = function() end,
      sources = function() return {} end,
    })
    for _, f in ipairs(fmt.resolve(0)) do
      if f.name == "empty" then T.eq(f.active, false) end
    end
  end)
end)

T.describe("lib.format — enable/disable + toggles", function()
  T.it("enabled defaults to true", function()
    local fmt = reset_format()
    T.eq(fmt.enabled(0), true)
  end)

  T.it("vim.g.autoformat = false disables globally", function()
    local fmt = reset_format()
    vim.g.autoformat = false
    T.eq(fmt.enabled(0), false)
    vim.g.autoformat = nil
  end)

  T.it("vim.b.autoformat overrides global", function()
    local fmt = reset_format()
    vim.g.autoformat = false
    vim.b[0].autoformat = true
    T.eq(fmt.enabled(0), true)
    vim.g.autoformat = nil
    clear_buffer_var(0)
  end)

  T.it("enable(false) sets global off", function()
    local fmt = reset_format()
    fmt.enable(false)
    T.eq(vim.g.autoformat, false)
    fmt.enable(true)
  end)

  T.it("enable(false, true) sets buffer off without touching global", function()
    local fmt = reset_format()
    fmt.enable(false, true)
    T.eq(vim.b[0].autoformat, false)
    T.eq(vim.g.autoformat == nil or vim.g.autoformat == true, true)
    clear_buffer_var(0)
  end)
end)

T.describe("lib.format — format() semantics", function()
  T.it("no formatters + no force: silent, no warning", function()
    local fmt = reset_format()
    local warned = false
    local orig = vim.notify
    vim.notify = function(_, lvl)
      if lvl == vim.log.levels.WARN then warned = true end
    end
    fmt.format({ buf = 0 })
    vim.notify = orig
    T.eq(warned, false, "unexpected warning on silent format")
  end)

  T.it("no formatters + force=true: warns", function()
    local fmt = reset_format()
    local warned = false
    local orig = vim.notify
    vim.notify = function(msg, lvl)
      if lvl == vim.log.levels.WARN and msg:match("[Nn]o formatter") then warned = true end
    end
    fmt.format({ buf = 0, force = true })
    vim.notify = orig
    T.truthy(warned, "force=true with no formatters should warn")
  end)

  T.it("disabled + no force: format() does not call formatter", function()
    local fmt = reset_format()
    local ran = false
    fmt.register({
      name = "dis", primary = true, priority = 100,
      format = function() ran = true end,
      sources = function() return { "dis-src" } end,
    })
    vim.g.autoformat = false
    fmt.format({ buf = 0 })
    vim.g.autoformat = nil
    T.eq(ran, false)
  end)

  T.it("disabled + force=true: format() runs formatter", function()
    local fmt = reset_format()
    local ran = false
    fmt.register({
      name = "dis-force", primary = true, priority = 100,
      format = function() ran = true end,
      sources = function() return { "dis-force-src" } end,
    })
    vim.g.autoformat = false
    fmt.format({ buf = 0, force = true })
    vim.g.autoformat = nil
    T.truthy(ran, "force=true should bypass the enabled check")
  end)

  T.it("enabled + sources present: format() runs primary formatter once", function()
    local fmt = reset_format()
    local ran = 0
    fmt.register({
      name = "primary", primary = true, priority = 100,
      format = function() ran = ran + 1 end,
      sources = function() return { "primary-src" } end,
    })
    fmt.format({ buf = 0 })
    T.eq(ran, 1)
  end)
end)
