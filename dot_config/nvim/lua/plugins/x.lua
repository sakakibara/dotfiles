local prios = {
  ["plugins.extras.lang.typescript"] = 5,
}

local extras = Util.plugin.dedup(Util.config.json.data.extras)

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
