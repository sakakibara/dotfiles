---@class util.path
local M = {}

M.home = vim.uv.os_homedir()

M.sep = package.config:sub(1, 1)

M.remote_patterns = { "/mnt/" }

function M.is_remote_file(path)
  if not path then
    return false
  end
  for _, pattern in ipairs(M.remote_patterns) do
    if path:sub(1, #pattern) == pattern then
      return true
    end
  end
  return false
end

function M.is_dir(path)
  local stat = vim.uv.fs_stat(path)
  return stat and stat.type == "directory"
end

function M.is_absolute(path)
  return path:sub(1, 1) == M.sep or (Util.is_win and path:match("^[a-zA-Z]:"))
end

function M.split(path)
  local segments = {}
  for segment in string.gmatch(path, "[^" .. M.sep .. "]+") do
    table.insert(segments, segment)
  end
  return segments
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
  if M.home ~= "" and path:sub(1, #M.home) == M.home then
    return "~" .. path:sub(#M.home + 1)
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
  local no_name = opts.no_name or false

  -- Handle nil or empty path
  if not path or path == "" then
    if no_name then
      return "", "", "[No Name]"
    else
      return "", "", ""
    end
  end

  -- Remove trailing slash
  if path:len() > 1 and path:sub(-1) == M.sep then
    path = path:sub(1, -2)
  end

  -- Check if the path is a directory
  local is_directory = M.is_dir(path)

  -- Convert absolute path to relative if needed
  if relative and M.is_absolute(path) then
    path = M.make_relative(path)
  end

  -- Get the current working directory
  local cwd = vim.uv.cwd()

  -- Replace home directory with '~' if needed
  if replace_home then
    path = M.replace_home_dir(path)
    cwd = M.replace_home_dir(cwd)
  end

  local cwd_segments = M.split(cwd)
  local path_segments = M.split(path)

  -- Check if the path contains the CWD
  local contains_cwd = path:sub(1, #cwd) == cwd

  -- Adjust the start and end indices for head and tail
  local head_start = contains_cwd and #cwd_segments + 1 or 1
  local tail_start = math.max(#path_segments - tail_count + 1, head_start)
  local head_end = tail_start - 1

  -- Extract segments
  local cwd_part = contains_cwd and table.concat(cwd_segments, M.sep) or ""
  local head_segments = { unpack(path_segments, head_start, head_end) }
  local tail_segments = { unpack(path_segments, tail_start) }

  -- Shorten each segment with special handling for '.'
  local function shorten_segment(segment, len, append_ellipsis)
    if len > 0 then
      if len == 1 and segment:sub(1, 1) == "." then
        len = len + 1
      end
      local short = segment:sub(1, len)
      if append_ellipsis and #segment > len then
        return short .. Util.config.icons.status.Ellipsis
      end
      return short
    else
      return segment
    end
  end

  -- Shorten segments
  local function shorten_segments(segments)
    local shortened = {}
    for _, segment in ipairs(segments) do
      table.insert(shortened, shorten_segment(segment, short_len, ellipsis))
    end
    return shortened
  end

  -- Shorten head and cwd segments
  local head_short = shorten_segments(head_segments)
  local cwd_short = shorten_segments(M.split(cwd_part))

  -- Combine head and cwd segments
  local combined_segments = {}
  for _, segment in ipairs(cwd_short) do
    table.insert(combined_segments, segment)
  end
  for _, segment in ipairs(head_short) do
    table.insert(combined_segments, segment)
  end

  -- Apply max_segments
  if max_segments > 0 and #combined_segments > max_segments then
    local excess_count = #combined_segments - max_segments
    -- Remove excess segments from the beginning
    for _ = 1, excess_count do
      table.remove(combined_segments, 1)
    end
  end

  -- Split back into head and cwd parts
  local final_cwd_segments, final_head_segments
  if #combined_segments > #cwd_short then
    final_cwd_segments = { unpack(combined_segments, 1, #cwd_short) }
    final_head_segments = { unpack(combined_segments, #cwd_short + 1) }
  else
    final_cwd_segments = combined_segments
    final_head_segments = {}
  end

  -- Create the final output
  local function concat_segments(segments)
    return table.concat(segments, join_separator)
  end

  local cwd_str = cwd_part ~= "" and concat_segments(final_cwd_segments) or ""
  local head_str = concat_segments(final_head_segments)
  local tail_str = concat_segments(tail_segments)

  -- Append separator to tail if the path is a directory
  if is_directory and tail_str ~= "" and not tail_str:match(join_separator .. "$") then
    tail_str = tail_str .. join_separator
  end

  -- Append separator if return_segments and last_separator are true
  if return_segments and last_separator then
    if cwd_str ~= "" then
      cwd_str = cwd_str .. join_separator
    end
    if head_str ~= "" then
      head_str = head_str .. join_separator
    end
  end

  -- Prepend separator to head if it's the root path
  if head_start == 1 and join_separator == M.sep and path:sub(1, 1) == M.sep then
    head_str = join_separator .. head_str
  end

  -- Construct the result
  if return_segments then
    return cwd_str, head_str, tail_str
  else
    local result = {}
    if cwd_str ~= "" then
      table.insert(result, cwd_str)
    end
    if head_str ~= "" then
      table.insert(result, head_str)
    end
    if tail_str ~= "" then
      table.insert(result, tail_str)
    end
    return table.concat(result, join_separator)
  end
end

return M
