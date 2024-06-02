---@class util.file
local M = {}

local function fs_normalize_path(path)
  local res = path:gsub("\\", "/"):gsub("/+", "/"):gsub("(.)/$", "%1")
  return res
end

local function fs_is_present_path(path)
  return vim.uv.fs_stat(path) ~= nil
end

local function fs_child_path(dir, name)
  return fs_normalize_path(string.format("%s/%s", dir, name))
end

local function fs_get_type(path)
  if not fs_is_present_path(path) then
    return nil
  end
  return vim.fn.isdirectory(path) == 1 and "directory" or "file"
end

local function get_files(path)
  local fs = vim.uv.fs_scandir(path)
  local res = {}
  if not fs then
    return res
  end

  local name, fs_type = vim.uv.fs_scandir_next(fs)
  while name do
    if not (fs_type == "file" or fs_type == "directory") then
      fs_type = fs_get_type(fs_child_path(path, name))
    end
    if fs_type == "file" then
      table.insert(res, name)
    end
    name, fs_type = vim.uv.fs_scandir_next(fs)
  end

  table.sort(res, function(x, y)
    return x:lower() < y:lower()
  end)

  return res
end

local function current_file_index(files)
  local basename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ":t")
  local basename_index
  if basename ~= "" then
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
  local dir = Util.path.get_parent_path()
  local files = get_files(dir)
  if files == nil then
    return
  end
  local index = current_file_index(files)
  if index and index + 1 <= #files then
    local path_sep = package.config:sub(1, 1)
    local target_path = dir .. path_sep .. files[index + 1]
    vim.cmd("edit " .. vim.fn.fnameescape(target_path))
  end
end

function M.prev_file()
  local dir = Util.path.get_parent_path()
  local files = get_files(dir)
  if files == nil then
    return
  end
  local index = current_file_index(files)
  if index and index > 1 then
    local path_sep = package.config:sub(1, 1)
    local target_path = dir .. path_sep .. files[index - 1]
    vim.cmd("edit " .. vim.fn.fnameescape(target_path))
  end
end

function M.first_file()
  local dir = Util.path.get_parent_path()
  local files = get_files(dir)
  if files == nil then
    return
  end
  local path_sep = package.config:sub(1, 1)
  local target_path = dir .. path_sep .. files[1]
  vim.cmd("edit " .. vim.fn.fnameescape(target_path))
end

function M.last_file()
  local dir = Util.path.get_parent_path()
  local files = get_files(dir)
  if files == nil then
    return
  end
  local path_sep = package.config:sub(1, 1)
  local target_path = dir .. path_sep .. files[#files]
  vim.cmd("edit " .. vim.fn.fnameescape(target_path))
end

return M
