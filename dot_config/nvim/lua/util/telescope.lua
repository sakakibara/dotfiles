local M = {}

function M.func(picker, opts)
  local params = { picker = picker, opts = opts }
  return function()
    picker = params.picker
    opts = params.opts
    if type(opts.cwd) == "function" then
      opts = vim.tbl_deep_extend("force", opts or {}, { cwd = opts.cwd() })
    end

    if picker == "files" then
      if vim.loop.fs_stat((opts.cwd or vim.loop.cwd()) .. "/.git") then
        opts.show_untracked = true
        picker = "git_files"
      else
        picker = "find_files"
      end
    end

    if opts.cwd and opts.cwd ~= vim.loop.cwd() then
      opts.attach_mappings = function(_, map)
        map("i", "<a-c>", function()
          local action_state = require("telescope.actions.state")
          local line = action_state.get_current_line()
          M.run(
            params.picker,
            vim.tbl_deep_extend("force", {},
              params.opts or {},
              { cwd = false, default_text = line })
          )()
        end)
        return true
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

return M
