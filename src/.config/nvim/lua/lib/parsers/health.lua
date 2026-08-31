local M = {}

local MODELINE = "^;+%s*inherits%s*:?%s*([a-z_,()]+)%s*$"

local function sorted_keys(set)
  local keys = vim.tbl_keys(set)
  table.sort(keys)
  return keys
end

local function to_set(list)
  local set = {}
  for _, item in ipairs(list) do set[item] = true end
  return set
end

local function closure(langs, requires)
  local reached, queue = {}, vim.list_slice(langs)
  while #queue > 0 do
    local lang = table.remove(queue)
    if not reached[lang] then
      reached[lang] = true
      vim.list_extend(queue, requires(lang))
    end
  end
  return reached
end

local function inherits_of(lang, query_dir, with_optional)
  local wanted = {}
  for _, path in ipairs(vim.fn.glob(query_dir(lang) .. "/*.scm", false, true)) do
    for _, line in ipairs(vim.fn.readfile(path)) do
      if not vim.startswith(line, ";") then break end
      for item in vim.gsplit(line:match(MODELINE) or "", ",", { trimempty = true }) do
        local optional = item:match("^%((.+)%)$")
        if not optional then
          wanted[item] = true
        elseif with_optional then
          wanted[optional] = true
        end
      end
    end
  end
  wanted[lang] = nil
  return sorted_keys(wanted)
end

local function missing_for(seed, deps)
  local provided, top_level = closure(seed, deps.requires), to_set(seed)
  local missing, frontier = {}, sorted_keys(provided)

  while #frontier > 0 do
    local discovered = {}
    for _, lang in ipairs(frontier) do
      for _, dep in ipairs(inherits_of(lang, deps.query_dir, top_level[lang])) do
        if not provided[dep] then
          missing[#missing + 1] = { lang = dep, via = lang }
          for pulled in pairs(closure({ dep }, deps.requires)) do
            if not provided[pulled] then
              provided[pulled] = true
              discovered[pulled] = true
            end
          end
        end
      end
    end
    frontier = sorted_keys(discovered)
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

function M.buffer_gaps(deps)
  local gaps, seen = {}, {}
  for _, ft in ipairs(deps.buffer_fts()) do
    if ft ~= "" and not seen[ft] then
      seen[ft] = true
      local lang = deps.lang_for_ft(ft)
      if lang and not vim.tbl_contains(deps.parsers_for(ft), lang) and deps.parser_available(lang) then
        gaps[#gaps + 1] = { ft = ft, lang = lang }
      end
    end
  end
  table.sort(gaps, function(a, b) return a.ft < b.ft end)
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
    buffer_fts = function()
      local fts = {}
      for _, buf in ipairs(vim.api.nvim_list_bufs()) do
        if vim.api.nvim_buf_is_loaded(buf) then fts[#fts + 1] = vim.bo[buf].filetype end
      end
      return fts
    end,
    lang_for_ft = vim.treesitter.language.get_lang,
    parser_available = function(lang) return ts_parsers[lang] ~= nil end,
  }
end

local function describe_inherit(gap)
  local parts = {}
  for _, miss in ipairs(gap.missing) do
    parts[#parts + 1] = ("%s (inherited by %s)"):format(miss.lang, miss.via)
  end
  return ("%s: no registered parser provides %s - those queries never load")
    :format(gap.ft, table.concat(parts, ", "))
end

local function describe_buffer(gap)
  return ("%s: the %s parser serves this filetype but is not registered - highlighting is off in these buffers")
    :format(gap.ft, gap.lang)
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

  local checks = {
    { gaps = M.inherit_gaps(deps), describe = describe_inherit,
      ok = "every registered parser provides the languages its queries inherit" },
    { gaps = M.buffer_gaps(deps), describe = describe_buffer,
      ok = "every open buffer has a parser registered for its filetype" },
  }
  for _, check in ipairs(checks) do
    if #check.gaps == 0 then
      add("ok", check.ok)
    else
      for _, gap in ipairs(check.gaps) do add("warn", check.describe(gap)) end
    end
  end

  return items
end

function M.report(deps) return gather(deps) end

function M.check()
  vim.health.start("lib.parsers")
  for _, item in ipairs(gather()) do
    local emit = vim.health[item.kind] or vim.health.info
    emit(item.text)
  end
end

return M
