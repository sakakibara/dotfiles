-- Auto-scan every .lua file under lua/config/plugins/ (recursively, one
-- level deep) and flatten their returned spec lists. Plugin files simply
-- `return { ... specs }` — no registration call needed.

local root = vim.fn.stdpath("config") .. "/lua/config/plugins"
local specs = {}

local function append(name)
  local ok, list = pcall(require, "config.plugins." .. name)
  if ok and type(list) == "table" then
    for _, s in ipairs(list) do specs[#specs + 1] = s end
  end
end

for _, f in ipairs(vim.fn.glob(root .. "/*.lua",   false, true)) do
  append(vim.fn.fnamemodify(f, ":t:r"))
end
for _, f in ipairs(vim.fn.glob(root .. "/*/*.lua", false, true)) do
  local dir  = vim.fn.fnamemodify(f, ":h:t")
  local base = vim.fn.fnamemodify(f, ":t:r")
  append(dir .. "." .. base)
end

return specs
