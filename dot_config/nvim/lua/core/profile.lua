-- lua/core/profile.lua
local uv = vim.uv
local M = {}

local t0
local spans = {}  -- { { name, ms, kind } }

function M.start()
  t0 = uv.hrtime()
end

function M.mark(name, kind)
  if not t0 then return end
  local ns = uv.hrtime() - t0
  table.insert(spans, { name = name, ms = ns / 1e6, kind = kind or "event" })
end

function M.span(name, kind, fn)
  local s = uv.hrtime()
  fn()
  table.insert(spans, { name = name, ms = (uv.hrtime() - s) / 1e6, kind = kind or "span" })
end

function M.report()
  table.sort(spans, function(a, b) return a.ms > b.ms end)
  local lines = { string.format("Total startup: %.2f ms", (uv.hrtime() - (t0 or uv.hrtime())) / 1e6), "" }
  for _, s in ipairs(spans) do
    lines[#lines + 1] = string.format("  %-40s %7.2f ms  (%s)", s.name, s.ms, s.kind)
  end
  return table.concat(lines, "\n")
end

function M.dump()
  local path = vim.fn.stdpath("state") .. "/pack-profile.json"
  vim.fn.mkdir(vim.fn.fnamemodify(path, ":h"), "p")
  local f = io.open(path, "w")
  if f then
    f:write(vim.json.encode({ total_ms = (uv.hrtime() - (t0 or uv.hrtime())) / 1e6, spans = spans }))
    f:close()
  end
end

vim.api.nvim_create_user_command("PackProfile", function()
  print(M.report())
end, { desc = "Print pack profile" })

return M
