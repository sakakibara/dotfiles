local uv = vim.uv
local M = {}

M.sep = package.config:sub(1, 1)
M.home = vim.fn.fnamemodify("~", ":p"):sub(1, -2)

function M.is_remote(p)
  return type(p) == "string" and (p:match("^/mnt/") or p:match("^//") or p:match("^\\\\"))
end

function M.buf_get_name(buf)
  buf = buf or 0
  local name = vim.api.nvim_buf_get_name(buf)
  if name:match("^oil://") then
    local ok, oil = pcall(require, "oil")
    if ok then return oil.get_current_dir(buf) or name end
  end
  return name
end

function M.replace_home(p)
  if type(p) ~= "string" or #p == 0 then return p end
  if p:sub(1, #M.home) == M.home then
    return "~" .. p:sub(#M.home + 1)
  end
  return p
end

function M.split(p)
  local parts = {}
  for part in string.gmatch(p or "", "[^" .. M.sep .. "]+") do
    parts[#parts + 1] = part
  end
  return parts
end

function M.short(p, max_seg)
  max_seg = max_seg or 3
  local parts = M.split(M.replace_home(p))
  if #parts <= max_seg then return table.concat(parts, M.sep) end
  local kept = { parts[1], "…" }
  for i = #parts - max_seg + 2, #parts do kept[#kept + 1] = parts[i] end
  return table.concat(kept, M.sep)
end

function M.exists(p)
  return p and uv.fs_stat(p) ~= nil
end

return M
