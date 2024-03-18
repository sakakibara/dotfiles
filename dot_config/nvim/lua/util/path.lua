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

M.home = vim.loop.os_homedir()

local function get_sep()
  if jit then
    local os = string.lower(jit.os)
    if os ~= "windows" then
      return "/"
    else
      return "\\"
    end
  else
    return package.config:sub(1, 1)
  end
end

M.sep = get_sep()

function M.split(path)
  return vim.split(path, M.sep, { trimempty = true })
end

function M.is_dir(path)
  local current_path = path or ""
  local stat = vim.loop.fs_stat(current_path)
  return stat and stat.type == "directory"
end

function M.get_current_file_path()
  return M.buf_get_name(vim.api.nvim_get_current_buf())
end

function M.buf_get_name(buf)
  local ft = vim.api.nvim_buf_get_option(buf, "filetype")
  if ft == "oil" then
    return require("oil").get_current_dir()
  end
  return vim.api.nvim_buf_get_name(0)
end

function M.get_path_segments(path)
  local fnmod = vim.fn.fnamemodify
  local cpath = path and path or M.get_current_file_path() or ""
  local cwd = vim.loop.cwd() or ""
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

return M
