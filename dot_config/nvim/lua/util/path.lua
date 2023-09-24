local M = {}

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

function M.get_basename(path)
  local current_path = path and path or M.get_current_file_path()
  local basename = vim.fn.fnamemodify(current_path, ":t")
  if basename == "" and not M.is_dir(current_path) then
    basename = "[No Name]"
  end
  return basename
end

function M.get_pwd(path)
  local current_path = path and path or M.get_current_file_path()
  local pwd = vim.loop.cwd()
  if current_path == "" or current_path:find(pwd, 1, true) then
    pwd = vim.fn.fnamemodify(pwd, ":~") .. M.sep
  else
    pwd = ""
  end
  return pwd
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

function M.get_relative_dir_path(path)
  local current_path = path and path or M.get_current_file_path()
  local dir_path
  if current_path ~= "" then
    dir_path = vim.fn.fnamemodify(current_path, ":~:.:h")
  end
  if not dir_path or dir_path == "." then
    dir_path = ""
  elseif dir_path ~= M.sep then
    dir_path = dir_path .. M.sep
  end
  return dir_path
end

return M
