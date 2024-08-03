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
  local components = {}
  for component in string.gmatch(path, "[^" .. M.sep .. "]+") do
    table.insert(components, component)
  end
  return components
end

function M.is_dir(path)
  local current_path = path or ""
  local stat = vim.uv.fs_stat(current_path)
  return stat and stat.type == "directory"
end

local function is_absolute(path)
  return path:sub(1, 1) == M.sep or (Util.is_win and path:match("^[a-zA-Z]:"))
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

function M.get_parent_path()
  return vim.fn.fnamemodify(M.get_current_file_path(), ":h")
end

function M.replace_home_dir(path)
  local home_dir = vim.uv.os_homedir()
  if home_dir ~= "" and path:sub(1, #home_dir) == home_dir then
    return "~" .. path:sub(#home_dir + 1)
  else
    return path
  end
end

function M.make_relative(path)
  local cwd = vim.uv.cwd()
  if path:sub(1, #cwd) == cwd then
    return path:sub(#cwd + 1)
  end
  return path
end

function M.format_path(path, opts)
  -- Default options
  opts = opts or {}
  local short_len = opts.short_len or 0
  local tail_count = opts.tail_count or 1
  local max_segments = opts.max_segments or 0
  local relative = opts.relative or false
  local replace_home = opts.replace_home or false
  local join_separator = opts.join_separator or M.sep
  local return_segments = opts.return_segments or false
  local last_separator = opts.last_separator or false
  local ellipsis = opts.ellipsis or false

  -- Convert absolute path to relative if needed
  if relative and is_absolute(path) then
    path = M.make_relative(path)
  end

  -- Replace home directory with '~' if needed
  if replace_home then
    path = M.replace_home_dir(path)
  end

  -- Get the current working directory and replace home if needed
  local cwd = vim.uv.cwd()
  if replace_home then
    cwd = M.replace_home_dir(cwd)
  end
  local cwd_components = M.split(cwd)
  local path_components = M.split(path)

  -- Check if the path contains the CWD
  local contains_cwd = true
  for i = 1, #cwd_components do
    if cwd_components[i] ~= path_components[i] then
      contains_cwd = false
      break
    end
  end

  -- Split into CWD, head, and tail
  local head_start = contains_cwd and #cwd_components + 1 or 1
  local head_end = math.max(#path_components - tail_count, head_start)
  local cwd_part = contains_cwd and table.concat(cwd_components, M.sep) or nil
  local head_components = { unpack(path_components, head_start, head_end) }
  local tail_components = { unpack(path_components, head_end + 1) }

  -- Function to shorten each component with special handling for '.'
  local function shorten_component(c, len, append_ellipsis)
    if len > 0 then
      if c == "." then
        return "."
      end
      local short = string.sub(c, 1, len)
      if append_ellipsis and #c > len then
        return short .. Util.config.icons.status.Ellipsis
      end
      return short
    else
      return c
    end
  end

  -- Shorten head components
  local head_short = {}
  for _, component in ipairs(head_components) do
    table.insert(head_short, shorten_component(component, short_len, ellipsis))
  end

  -- Shorten CWD components
  local cwd_short = {}
  if cwd_part then
    for _, component in ipairs(M.split(cwd_part)) do
      table.insert(cwd_short, shorten_component(component, short_len, ellipsis))
    end
  end

  -- Combine head and cwd components
  local combined_components = {}
  if cwd_short then
    for _, component in ipairs(cwd_short) do
      table.insert(combined_components, component)
    end
  end
  for _, component in ipairs(head_short) do
    table.insert(combined_components, component)
  end

  -- Apply max_segments
  if max_segments > 0 and #combined_components > max_segments then
    local excess_count = #combined_components - max_segments
    -- Remove excess components from the beginning
    for _ = 1, excess_count do
      table.remove(combined_components, 1)
    end
  end

  -- Split back into head and cwd parts
  local final_head_components = {}
  local final_cwd_components = {}

  if cwd_short and #combined_components > #cwd_short then
    final_cwd_components = { unpack(combined_components, 1, #cwd_short) }
    final_head_components = { unpack(combined_components, #cwd_short + 1) }
  else
    final_head_components = combined_components
  end

  -- Create the final output
  local head_str = table.concat(final_head_components, join_separator)
  local tail_str = table.concat(tail_components, join_separator)
  local cwd_str = cwd_part and table.concat(final_cwd_components, join_separator) or nil

  -- Append separator if last_separator is true and return_segments is false
  if return_segments and last_separator then
    if cwd_str and cwd_str ~= "" then
      cwd_str = cwd_str .. join_separator
    end
    if head_str and head_str ~= "" then
      head_str = head_str .. join_separator
    end
  end

  -- Construct the result
  if return_segments then
    return cwd_str, head_str, tail_str
  else
    -- Concatenate with join_separator
    local result = {}
    if cwd_str and cwd_str ~= "" then
      table.insert(result, cwd_str)
    end
    if head_str and head_str ~= "" then
      table.insert(result, head_str)
    end
    if tail_str and tail_str ~= "" then
      table.insert(result, tail_str)
    end

    -- Join the result components
    local result_str = table.concat(result, join_separator)

    return result_str
  end
end

return M
