local T = require("tests.helpers")

local function fixture()
  local dir = vim.uv.fs_realpath(vim.fn.tempname()) or vim.fn.tempname()
  vim.fn.mkdir(dir, "p")
  dir = vim.uv.fs_realpath(dir)
  for _, f in ipairs({ "a.txt", "b.txt", ".hidden" }) do vim.fn.writefile({ f }, dir .. "/" .. f) end
  vim.fn.mkdir(dir .. "/sub", "p")
  vim.uv.fs_symlink(dir .. "/sub", dir .. "/c-dirlink")
  vim.fn.writefile({ "target" }, dir .. "/sub/target")
  vim.uv.fs_symlink(dir .. "/sub/target", dir .. "/d-filelink")
  return dir
end

local function cycle_from(dir, name, direction)
  vim.cmd("edit " .. vim.fn.fnameescape(dir .. "/" .. name))
  require("lib.keymaps").cycle_sibling(direction)
  local got = vim.fs.basename(vim.api.nvim_buf_get_name(0))
  vim.cmd("silent! %bwipeout!")
  return got
end

T.describe("lib.keymaps.cycle_sibling", function()
  T.it("skips directory symlinks but keeps file symlinks", function()
    local dir = fixture()
    T.eq(cycle_from(dir, "b.txt", 1), "d-filelink")
    T.eq(cycle_from(dir, "a.txt", -1), "d-filelink")
    vim.fn.delete(dir, "rf")
  end)

  T.it("lands on the first sibling in either direction from a filtered file", function()
    local dir = fixture()
    T.eq(cycle_from(dir, ".hidden", 1), "a.txt")
    T.eq(cycle_from(dir, ".hidden", -1), "a.txt")
    vim.fn.delete(dir, "rf")
  end)
end)
