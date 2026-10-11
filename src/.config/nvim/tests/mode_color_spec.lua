local T = require("tests.helpers")

local function reset_mc()
  package.loaded["lib.mode_color"] = nil
  return require("lib.mode_color")
end

local function is_hex(s)
  return type(s) == "string" and s:match("^#%x%x%x%x%x%x$") ~= nil
end

T.describe("lib.mode_color", function()
  T.it("target returns a valid #rrggbb hex for each mode", function()
    local mc = reset_mc()
    for _, mode in ipairs({ "n", "i", "v", "V", "c", "s", "R", "t" }) do
      T.truthy(is_hex(mc.target(mode)), "mode " .. mode)
    end
  end)

  T.it("unknown mode falls back to normal color", function()
    local mc = reset_mc()
    T.eq(mc.target("?"), mc.target("n"))
  end)

  T.it("palette.mix produces the midpoint", function()
    T.eq(require("lib.palette").mix("#000000", "#ffffff", 0.5), "#808080")
  end)

  T.it("palette.shade lightens and clamps", function()
    local P = require("lib.palette")
    T.eq(P.shade("#101010", 12), "#1c1c1c")
    T.eq(P.shade("#fafafa", 12), "#ffffff")
  end)

  T.it("target resolves full mode(1) strings through their prefixes", function()
    local mc = reset_mc()
    T.eq(mc.target("Rvc"), mc.target("R"))
    T.eq(mc.target("niI"), mc.target("n"))
    T.eq(mc.target("ntT"), mc.target("n"))
    T.truthy(mc.target("Rvc") ~= mc.target("n"), "replace differs from normal")
  end)

  T.it("different modes produce different colors", function()
    local mc = reset_mc()
    T.truthy(mc.target("n") ~= mc.target("i"), "normal vs insert should differ")
    T.truthy(mc.target("i") ~= mc.target("v"), "insert vs visual should differ")
  end)
end)

T.describe("lib.mode_color with the statusline", function()
  T.it("a ColorScheme event keeps the current mode's colour on the mode block", function()
    local real_mode = vim.fn.mode
    local animate = vim.g.status_animate
    vim.g.status_animate = false
    package.loaded["lib.mode_color"] = nil
    package.loaded["lib.statusline"] = nil
    local mc = require("lib.mode_color")
    local sl = require("lib.statusline")
    local ok, err = pcall(function()
      sl.setup()
      vim.fn.mode = function() return "i" end
      mc.animate("i")
      vim.api.nvim_exec_autocmds("ColorScheme", {})
      local want = tonumber(mc.target("i"):sub(2), 16)
      T.eq(vim.api.nvim_get_hl(0, { name = "StslMode" }).bg, want)
      T.truthy(mc.target("i") ~= mc.target("n"), "insert differs from normal")
    end)
    vim.fn.mode = real_mode
    vim.g.status_animate = animate
    for _, g in ipairs({ "Lib.mode_color", "Lib.statusline.hl", "Lib.statusline.cache" }) do
      pcall(vim.api.nvim_del_augroup_by_name, g)
    end
    if not ok then error(err, 0) end
  end)
end)
