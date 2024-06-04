---@class util.telescope
local M = {}

function M.run(picker, opts)
  local params = { picker = picker, opts = opts }
  return function()
    picker = params.picker
    opts = params.opts or {}
    if type(opts.cwd) == "function" then
      opts = vim.tbl_deep_extend("force", opts or {}, { cwd = opts.cwd() })
    end

    if picker == "files" then
      if
        vim.uv.fs_stat((opts.cwd or vim.uv.cwd()) .. "/.git")
        and not vim.uv.fs_stat((opts.cwd or vim.uv.cwd()) .. "/.ignore")
        and not vim.uv.fs_stat((opts.cwd or vim.uv.cwd()) .. "/.rgignore")
      then
        opts.show_untracked = true
        picker = "git_files"
      else
        picker = "find_files"
      end
    end

    local builtin = require("telescope.builtin")[picker]
    if builtin then
      builtin(opts)
    else
      require("telescope").extensions[picker][picker](opts)
    end
  end
end

function M.get_kind_filter(buf)
  buf = (buf == nil or buf == 0) and vim.api.nvim_get_current_buf() or buf
  local ft = vim.bo[buf].filetype
  if Util.config.kind_filter == false then
    return
  end
  if Util.config.kind_filter[ft] == false then
    return
  end
  if type(Util.config.kind_filter[ft]) == "table" then
    return Util.config.kind_filter[ft]
  end
  ---@diagnostic disable-next-line: return-type-mismatch
  return type(Util.config.kind_filter) == "table"
      and type(Util.config.kind_filter.default) == "table"
      and Util.config.kind_filter.default
    or nil
end

return M
