local M = {}

local util_path = require("util.path")

local function get_files(dir)
  local handle = vim.loop.fs_scandir(dir)
  if handle == nil then return end
  local files_stream = function() return vim.loop.fs_scandir_next(handle) end

  local files = {}
  for basename, fs_type in files_stream do
    if fs_type == 'file' then table.insert(files, basename) end
  end

  table.sort(files, function(x, y) return x:lower() < y:lower() end)

  return files
end

local function current_file_index(files)
  local basename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ':t')
  local basename_index
  if basename ~= '' then
    for i, f in ipairs(files) do
      if basename == f then
        basename_index = i
        break
      end
    end
  end
  return basename_index
end

function M.next_file()
  local dir = util_path.basedir()
  local files = get_files(dir)
  if files == nil then return end
  local index = current_file_index(files)
  if index and index + 1 <= #files then
    local path_sep = package.config:sub(1, 1)
    local target_path = dir .. path_sep .. files[index + 1]
    vim.cmd('edit ' .. target_path)
  end
end

function M.prev_file()
  local dir = util_path.basedir()
  local files = get_files(dir)
  if files == nil then return end
  local index = current_file_index(files)
  if index and index > 1 then
    local path_sep = package.config:sub(1, 1)
    local target_path = dir .. path_sep .. files[index - 1]
    vim.cmd('edit ' .. target_path)
  end
end

function M.first_file()
  local dir = util_path.basedir()
  local files = get_files(dir)
  if files == nil then return end
  local path_sep = package.config:sub(1, 1)
  local target_path = dir .. path_sep .. files[1]
  vim.cmd('edit ' .. target_path)
end

function M.last_file()
  local dir = util_path.basedir()
  local files = get_files(dir)
  if files == nil then return end
  local path_sep = package.config:sub(1, 1)
  local target_path = dir .. path_sep .. files[#files]
  vim.cmd('edit ' .. target_path)
end

return M
