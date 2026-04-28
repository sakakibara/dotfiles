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

function M._structured_report()
  local sorted = vim.deepcopy(spans)
  table.sort(sorted, function(a, b) return a.ms > b.ms end)
  local total_ms = (uv.hrtime() - (t0 or uv.hrtime())) / 1e6
  local lines = {
    ("core.pack profile — total startup %.2f ms"):format(total_ms),
    "",
  }
  local highlights = { { 0, 0, #lines[1], "Title" } }
  for _, s in ipairs(sorted) do
    local name_padded = ("%-30s"):format(s.name)
    local ms_str = ("%9.2f ms"):format(s.ms)
    local kind_str = ("(%s)"):format(s.kind)
    local line = ("  %s  %s  %s"):format(name_padded, ms_str, kind_str)
    local row = #lines
    local col = 2
    table.insert(highlights, { row, col, col + #name_padded, "Identifier" }); col = col + #name_padded + 2
    table.insert(highlights, { row, col, col + #ms_str, "Number" }); col = col + #ms_str + 2
    table.insert(highlights, { row, col, col + #kind_str, "Comment" })
    lines[#lines + 1] = line
  end
  return { lines = lines, highlights = highlights }
end

vim.api.nvim_create_user_command("PackProfile", function()
  local UI = require("core.pack.ui")
  local data = M._structured_report()
  UI.status(data.lines, {
    title = "core.pack: profile",
    highlights = data.highlights,
    filetype = "pack-profile",
  })
end, { desc = "Show pack startup profile in a scratch buffer" })

return M
