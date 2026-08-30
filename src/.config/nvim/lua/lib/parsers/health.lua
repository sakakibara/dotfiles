local M = {}

local MODELINE = "^;+%s*inherits%s*:?%s*([a-z_,()]+)%s*$"

local function sorted_keys(set)
  local out = vim.tbl_keys(set)
  table.sort(out)
  return out
end

local function inherited_langs(dir)
  local required, optional = {}, {}
  for _, path in ipairs(vim.fn.glob(dir .. "/*.scm", false, true)) do
    for _, line in ipairs(vim.fn.readfile(path)) do
      if not vim.startswith(line, ";") then break end
      local list = line:match(MODELINE)
      if list then
        for item in vim.gsplit(list, ",") do
          if item:match("%(.*%)") then
            optional[item:sub(2, #item - 1)] = true
          else
            required[item] = true
          end
        end
      end
    end
  end
  return required, optional
end

local function provided_by(seed, requires)
  local have, queue = {}, vim.list_slice(seed)
  while #queue > 0 do
    local lang = table.remove(queue)
    if not have[lang] then
      have[lang] = true
      for _, req in ipairs(requires(lang)) do queue[#queue + 1] = req end
    end
  end
  return have
end

local function missing_for(seed, deps)
  local top_level = {}
  for _, lang in ipairs(seed) do top_level[lang] = true end
  local have = provided_by(seed, deps.requires)
  local pending, done, missing = sorted_keys(have), {}, {}
  while #pending > 0 do
    local lang = table.remove(pending, 1)
    if not done[lang] then
      done[lang] = true
      local required, optional = inherited_langs(deps.query_dir(lang))
      local wanted = top_level[lang] and vim.tbl_extend("force", {}, required, optional) or required
      for _, dep in ipairs(sorted_keys(wanted)) do
        if not have[dep] and dep ~= lang then
          missing[#missing + 1] = { lang = dep, via = lang }
          for _, pulled in ipairs(sorted_keys(provided_by({ dep }, deps.requires))) do
            if not have[pulled] then
              have[pulled] = true
              pending[#pending + 1] = pulled
            end
          end
          table.sort(pending)
        end
      end
    end
  end
  table.sort(missing, function(a, b)
    if a.lang ~= b.lang then return a.lang < b.lang end
    return a.via < b.via
  end)
  return missing
end

function M.inherit_gaps(deps)
  local gaps = {}
  for _, ft in ipairs(deps.fts()) do
    local missing = missing_for(deps.parsers_for(ft), deps)
    if #missing > 0 then
      gaps[#gaps + 1] = { ft = ft, missing = missing }
    end
  end
  return gaps
end

local function default_deps()
  local ts_parsers = require("nvim-treesitter.parsers")
  local install = require("nvim-treesitter.install")
  return {
    fts = Lib.parsers.fts,
    parsers_for = Lib.parsers.list_for_ft,
    requires = function(lang)
      local entry = ts_parsers[lang]
      return entry and entry.requires or {}
    end,
    query_dir = function(lang) return install.get_package_path("runtime", "queries", lang) end,
  }
end

local function gather(deps)
  local items = {}
  local function add(kind, text) items[#items + 1] = { kind = kind, text = text } end

  if not deps then
    local ok, resolved = pcall(default_deps)
    if not ok then
      add("warn", "nvim-treesitter is not loaded - parser query coverage not checked")
      return items
    end
    deps = resolved
  end

  local gaps = M.inherit_gaps(deps)
  if #gaps == 0 then
    add("ok", "every registered parser provides the languages its queries inherit")
    return items
  end

  for _, gap in ipairs(gaps) do
    local parts = {}
    for _, miss in ipairs(gap.missing) do
      parts[#parts + 1] = ("%s (inherited by %s)"):format(miss.lang, miss.via)
    end
    add("warn", ("%s: no registered parser provides %s - those queries never load"):format(
      gap.ft, table.concat(parts, ", ")))
  end

  return items
end

function M.report(deps) return gather(deps) end

function M.check()
  vim.health.start("lib.parsers")
  for _, item in ipairs(gather()) do
    if item.kind == "ok"        then vim.health.ok(item.text)
    elseif item.kind == "warn"  then vim.health.warn(item.text)
    elseif item.kind == "error" then vim.health.error(item.text)
    else                             vim.health.info(item.text)
    end
  end
end

return M
