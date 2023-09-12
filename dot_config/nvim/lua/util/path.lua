local M = {}

M.root_patterns = { ".git", "lua", ".svn", ".vs" }

function M.root()
  local path
  if vim.bo.filetype == "oil" then
    path = require("oil").get_current_dir()
  else
    path = vim.api.nvim_buf_get_name(0)
    path = path ~= "" and vim.loop.fs_realpath(path) or nil
  end
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

function M.basedir()
  local path
  if vim.bo.filetype == "oil" then
    path = vim.fn.fnamemodify(require("oil").get_current_dir(), ":h:p")
  else
    path = vim.api.nvim_buf_get_name(0)
    path = path ~= "" and vim.fn.fnamemodify(path, ":h:p") or vim.loop.cwd()
  end
  return path
end

return M
