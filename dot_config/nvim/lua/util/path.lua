local Path = require("plenary.path")

---@class util.path
local M = {}

M.remote_patterns = { "/mnt/" }

function M.is_remote_file(path)
  local current_path = path or ""
  for _, pattern in ipairs(M.remote_patterns) do
    if current_path:sub(1, #pattern) == pattern then
      return true
    end
  end
end

M.home = vim.uv.os_homedir()

M.sep = package.config:sub(1, 1)

function M.split(path)
  return vim.split(path, M.sep, { trimempty = true })
end

function M.is_dir(path)
  local current_path = path or ""
  local stat = vim.uv.fs_stat(current_path)
  return stat and stat.type == "directory"
end

function M.get_current_file_path()
  return M.buf_get_name(vim.api.nvim_get_current_buf()) or ""
end

function M.buf_get_name(buf)
  local ft = vim.api.nvim_get_option_value("filetype", { buf = buf })
  if ft == "oil" then
    return require("oil").get_current_dir(buf) or ""
  end
  return vim.api.nvim_buf_get_name(buf)
end

function M.get_path_segments(path)
  local fnmod = vim.fn.fnamemodify
  local cpath = path and path or M.get_current_file_path() or ""
  local cwd = vim.uv.cwd() or ""
  local pwd, reldirpath, basename

  if cpath == "" then
    pwd = fnmod(cwd, ":~")
    if cwd ~= M.sep then
      pwd = pwd .. M.sep
    end
    reldirpath = nil
    basename = "[No Name]"
  else
    if M.is_dir(cpath) then
      basename = ""
    else
      basename = fnmod(cpath, ":t")
    end
    if cwd == M.sep then
      pwd = cwd
      reldirpath = fnmod(cpath, ":h"):sub(2)
    elseif cpath:find(cwd, 1, true) then
      if cwd:find(M.home, 1, true) then
        pwd = fnmod(cwd, ":~") .. M.sep
        reldirpath = fnmod(cpath, ":~:.:h")
      else
        pwd = cwd .. M.sep
        reldirpath = fnmod(cpath, ":.:h")
      end
    else
      pwd = ""
      reldirpath = fnmod(cpath, ":~:h")
    end
    if reldirpath and reldirpath ~= "" then
      if reldirpath == "." then
        reldirpath = ""
      elseif reldirpath ~= M.sep then
        reldirpath = reldirpath .. M.sep
      end
    end
  end
  return pwd, reldirpath, basename
end

function M.get_parent_path()
  return vim.fn.fnamemodify(M.get_current_file_path(), ":h")
end

function M.to_relative_path(path, base)
  local abs_base = Path:new(base):absolute()
  local abs_path = Path:new(path):absolute()
  if string.sub(abs_path, 1, #abs_base) ~= abs_base then
    return path
  end
  return string.sub(abs_path, #abs_base + 2)
end

function M.shorten_path(path, opts)
  opts = opts or {}
  local short_len = opts.short_len or 1
  local tail_count = opts.tail_count or 2
  local head_max = opts.head_max or 0
  local relative = opts.relative == nil or opts.relative
  local return_table = opts.return_table or false
  if relative then
    path = M.to_relative_path(path, vim.uv.cwd())
  end
  local components = vim.split(path, Path.path.sep)
  if #components == 1 then
    if return_table then
      return { nil, path }
    end
    return path
  end
  local tail = { unpack(components, #components - tail_count + 1) }
  local head = { unpack(components, 1, #components - tail_count) }
  if head_max > 0 and #head > head_max then
    head = { unpack(head, #head - head_max + 1) }
  end
  ---@diagnostic disable-next-line: param-type-mismatch
  local head_short = #head > 0 and Path.new(unpack(head)):shorten(short_len, {}) or nil
  if head_short == "/" then
    head_short = ""
  end
  local result = {
    head_short,
    table.concat(tail, Path.path.sep),
  }
  if return_table then
    return result
  end
  return table.concat(result, Path.path.sep)
end

return M
