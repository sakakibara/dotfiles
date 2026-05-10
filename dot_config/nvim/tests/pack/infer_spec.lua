-- tests/pack/infer_spec.lua
local T = require("tests.helpers")

local function fresh()
  package.loaded["core.pack.infer"] = nil
  return require("core.pack.infer")
end

local function tmp_plugin(layout)
  local root = vim.fn.tempname() .. "-plug"
  vim.fn.mkdir(root, "p")
  for path, contents in pairs(layout) do
    local full = root .. "/" .. path
    vim.fn.mkdir(vim.fn.fnamemodify(full, ":h"), "p")
    vim.fn.writefile(vim.split(contents, "\n", { plain = true }), full)
  end
  return root
end

T.describe("core.pack.infer.scan", function()
  T.it("extracts vimscript user-command names", function()
    local I = fresh()
    local dir = tmp_plugin({
      ["plugin/x.vim"] = "command! -bang -nargs=? Foo call s:foo()\ncommand! Bar call s:bar()",
    })
    local result = I.scan(dir)
    T.eq(result.cmds, { "Bar", "Foo" })
  end)

  T.it("extracts lua nvim_create_user_command names", function()
    local I = fresh()
    local dir = tmp_plugin({
      ["plugin/y.lua"] = [[vim.api.nvim_create_user_command("Telescope", function() end, {})]],
    })
    local result = I.scan(dir)
    T.eq(result.cmds, { "Telescope" })
  end)

  T.it("extracts filetypes from ftdetect/ and ftplugin/", function()
    local I = fresh()
    local dir = tmp_plugin({
      ["ftdetect/rust.lua"] = "vim.filetype.add({})",
      ["ftplugin/rust.vim"] = '" rust ftplugin',
      ["ftplugin/toml.lua"] = "-- toml ftplugin",
    })
    local result = I.scan(dir)
    T.eq(result.fts, { "rust", "toml" })
  end)

  T.it("ignores command-like strings inside lua string literals", function()
    local I = fresh()
    local dir = tmp_plugin({
      ["plugin/y.lua"] = [[
        local doc = "see :command! NotReal in the manual"
        vim.api.nvim_create_user_command("Real", function() end, {})
      ]],
    })
    local result = I.scan(dir)
    T.eq(result.cmds, { "Real" })
  end)

  T.it("ignores command-like strings inside vimscript comments", function()
    local I = fresh()
    local dir = tmp_plugin({
      ["plugin/x.vim"] = '" command! Bogus from a comment\ncommand! Real call s:r()',
    })
    local result = I.scan(dir)
    T.eq(result.cmds, { "Real" })
  end)

  T.it("returns empty when no plugin/, ftdetect/, or ftplugin/", function()
    local I = fresh()
    local dir = tmp_plugin({ ["lua/foo.lua"] = "return {}" })
    local result = I.scan(dir)
    T.eq(result.cmds, {})
    T.eq(result.fts, {})
  end)
end)

T.describe("core.pack.infer.apply", function()
  T.it("populates spec.cmd and sets lazy when commands are found", function()
    local I = fresh()
    local dir = tmp_plugin({
      ["plugin/p.vim"] = "command! Foo call s:f()",
    })
    local spec = { name = "demo", auto = true }
    T.eq(I.apply(spec, dir), true)
    T.eq(spec.cmd, { "Foo" })
    T.eq(spec.lazy, true)
  end)

  T.it("does nothing when auto is false", function()
    local I = fresh()
    local dir = tmp_plugin({ ["plugin/p.vim"] = "command! Foo call s:f()" })
    local spec = { name = "demo", auto = false }
    T.eq(I.apply(spec, dir), false)
    T.eq(spec.cmd, nil)
  end)

  T.it("does not override an explicit trigger", function()
    local I = fresh()
    local dir = tmp_plugin({ ["plugin/p.vim"] = "command! Foo call s:f()" })
    local spec = { name = "demo", auto = true, event = "BufReadPre" }
    T.eq(I.apply(spec, dir), false)
    T.eq(spec.cmd, nil)
  end)

  T.it("returns false when nothing inferable", function()
    local I = fresh()
    local dir = tmp_plugin({ ["lua/foo.lua"] = "return {}" })
    local spec = { name = "demo", auto = true }
    T.eq(I.apply(spec, dir), false)
    T.eq(spec.lazy, nil)
  end)
end)
