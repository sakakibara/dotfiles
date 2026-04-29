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

local KIND_LABELS = { packadd = "load", config = "setup" }
local BAR_WIDTH = 24
local BAR_FILLED = "█"   -- U+2588 (3 bytes UTF-8)
local BAR_EMPTY  = "░"   -- U+2591 (3 bytes UTF-8)

function M._structured_report()
  local total_ms = (uv.hrtime() - (t0 or uv.hrtime())) / 1e6

  -- Aggregate spans by plugin name (strip "kind:" prefix from name).
  local agg = {}
  local order = {}
  for _, s in ipairs(spans) do
    local name = s.name:gsub("^[^:]*:", "")
    local entry = agg[name]
    if not entry then
      entry = { total_ms = 0, phases = {}, name = name }
      agg[name] = entry
      order[#order + 1] = name
    end
    entry.total_ms = entry.total_ms + s.ms
    local label = KIND_LABELS[s.kind] or s.kind
    entry.phases[label] = (entry.phases[label] or 0) + s.ms
  end

  -- Sort plugins by total_ms desc.
  local sorted = {}
  for _, name in ipairs(order) do sorted[#sorted + 1] = agg[name] end
  table.sort(sorted, function(a, b) return a.total_ms > b.total_ms end)

  -- Bars are normalized to the heaviest plugin so the slowest is full and the
  -- rest scale relative to it. Wall-clock total_ms (header) dwarfs any single
  -- plugin's spans, so normalizing against it would round every bar to zero.
  local max_plugin_ms = sorted[1] and sorted[1].total_ms or 0

  -- Compute name column width: longest actual, floor 16, cap 40.
  local name_max = 16
  for _, e in ipairs(sorted) do name_max = math.max(name_max, #e.name) end
  if name_max > 40 then name_max = 40 end

  local lines = {
    ("core.pack profile — %d plugins, %.2f ms total"):format(#sorted, total_ms),
    "",
  }
  local highlights = { { 0, 0, #lines[1], "Title" } }

  for _, e in ipairs(sorted) do
    -- Truncate name if too long.
    local name = e.name
    if #name > name_max then name = name:sub(1, name_max - 1) .. "…" end
    local name_padded = ("%-" .. name_max .. "s"):format(name)

    -- Bar: relative to heaviest plugin (so slowest = full bar). Percentages
    -- below stay relative to wall-clock total — that's the meaningful number.
    local bar_share = (max_plugin_ms > 0) and (e.total_ms / max_plugin_ms) or 0
    local filled = math.floor(bar_share * BAR_WIDTH + 0.5)
    if filled > BAR_WIDTH then filled = BAR_WIDTH end
    local bar = BAR_FILLED:rep(filled) .. BAR_EMPTY:rep(BAR_WIDTH - filled)

    local ms_str = ("%9.2f ms"):format(e.total_ms)
    local share = (total_ms > 0) and (e.total_ms / total_ms) or 0
    local pct_str = ("%5.1f%%"):format(share * 100)

    -- Phase breakdown: "setup 120.34 + load 45.12" (sorted desc by ms).
    local phase_parts = {}
    for label, ms in pairs(e.phases) do
      phase_parts[#phase_parts + 1] = { label = label, ms = ms }
    end
    table.sort(phase_parts, function(a, b) return a.ms > b.ms end)
    local phase_strs = {}
    for _, p in ipairs(phase_parts) do
      phase_strs[#phase_strs + 1] = ("%s %.2f"):format(p.label, p.ms)
    end
    local phases = table.concat(phase_strs, " + ")

    local line = ("  %s  %s  %s  %s  %s"):format(name_padded, bar, ms_str, pct_str, phases)
    local row = #lines

    -- Byte-aware col offsets. Each bar character is 3 bytes UTF-8.
    local col = 2
    table.insert(highlights, { row, col, col + #name_padded, "Identifier" }); col = col + #name_padded + 2
    -- Bar: filled portion = green, empty portion = dim.
    local bar_filled_bytes = filled * 3
    table.insert(highlights, { row, col, col + bar_filled_bytes, "DiagnosticOk" })
    table.insert(highlights, { row, col + bar_filled_bytes, col + #bar, "Comment" })
    col = col + #bar + 2
    table.insert(highlights, { row, col, col + #ms_str, "Number" }); col = col + #ms_str + 2
    table.insert(highlights, { row, col, col + #pct_str, "Type" }); col = col + #pct_str + 2
    table.insert(highlights, { row, col, col + #phases, "Comment" })

    lines[#lines + 1] = line
  end

  return { lines = lines, highlights = highlights }
end

-- Lookup total profile time for a single plugin (sum of all phases).
function M.lookup(name)
  local total = 0
  local found = false
  for _, s in ipairs(spans) do
    if s.name:gsub("^[^:]*:", "") == name then
      total = total + s.ms
      found = true
    end
  end
  return found and total or nil
end

vim.api.nvim_create_user_command("PackProfile", function()
  local UI = require("core.pack.ui")
  local data = M._structured_report()
  UI.status(data.lines, {
    title = "core.pack: profile",
    highlights = data.highlights,
    filetype = "PackProfile",
  })
end, { desc = "Show pack startup profile in a scratch buffer" })

return M
