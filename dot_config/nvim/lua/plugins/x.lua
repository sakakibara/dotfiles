local prios = {
  ["plugins.extras.coding.nvim-cmp"] = 2,
  ["plugins.extras.coding.blink"] = 5,
  ["plugins.extras.lang.typescript"] = 5,
  ["plugins.extras.formatter.prettier"] = 10,
}

local extras = Util.dedup(Util.config.json.data.extras)

Util.plugin.save_core()

table.sort(extras, function(a, b)
  local pa = prios[a] or 50
  local pb = prios[b] or 50
  if pa == pb then
    return a < b
  end
  return pa < pb
end)

return vim.tbl_map(function(extra)
  return { import = extra }
end, extras)
