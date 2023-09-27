local M = {}

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
  local path
  if vim.bo.filetype == "oil" then
    path = require("oil").get_current_dir()
  else
    path = vim.api.nvim_buf_get_name(0)
  end
  return path
end

function M.get_path_segments(path)
  local fnmod = vim.fn.fnamemodify
  local cpath = path and path or M.get_current_file_path()
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

M.root_patterns = { ".git", "lua", ".svn", ".vs" }

function M.get_root_path()
  local path = M.get_current_file_path()
  path = path ~= "" and vim.loop.fs_realpath(path) or nil
  local roots = {}
  if path then
    for _, client in pairs(vim.lsp.get_active_clients({ bufnr = 0 })) do
      local workspace = client.config.workspace_folders
      local paths = workspace and vim.tbl_map(function(ws)
        return vim.uri_to_fname(ws.uri)
      end, workspace) or client.config.root_dir and { client.config.root_dir } or {}
      for _, p in ipairs(paths) do
        local r = vim.loop.fs_realpath(p)
        if r and path:find(r, 1, true) then
          roots[#roots + 1] = r
        end
      end
    end
  end
  table.sort(roots, function(a, b)
    return #a > #b
  end)
  local root = roots[1]
  if not root then
    path = path and vim.fs.dirname(path) or vim.loop.cwd()
    root = vim.fs.find(M.root_patterns, { path = path, upward = true })[1]
    root = root and vim.fs.dirname(root) or vim.loop.cwd()
  end
  return root
end

function M.get_parent_path()
  return vim.fn.fnamemodify(M.get_current_file_path(), ":h")
end

return M
