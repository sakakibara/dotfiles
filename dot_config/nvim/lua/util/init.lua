local M = {}

M.root_patterns = { ".git", "lua" }

function M.get_root()
  ---@type string?
  local path = vim.api.nvim_buf_get_name(0)
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

function M.telescope(picker, opts)
  local params = { picker = picker, opts = opts }
  return function()
    picker = params.picker
    opts = params.opts
    opts = vim.tbl_deep_extend("force", { cwd = M.get_root() }, opts or {})

    if picker == "files" then
      if vim.loop.fs_stat((opts.cwd or vim.loop.cwd()) .. "/.git") then
        opts.show_untracked = true
        picker = "git_files"
      else
        picker = "find_files"
      end
    elseif picker == "file_browser" then
      local path
      if vim.bo.filetype == "oil" then
        path = require("oil").get_current_dir()
      else
        local bufname = vim.api.nvim_buf_get_name(0)
        if bufname == "" then
          path = vim.loop.cwd()
        else
          path = vim.fn.fnamemodify(bufname, ":p:h")
        end
      end
      opts = vim.tbl_deep_extend("force", { path = path }, opts or {})
    end

    if opts.cwd and opts.cwd ~= vim.loop.cwd() then
      opts.attach_mappings = function(_, map)
        map("i", "<a-c>", function()
          local action_state = require("telescope.actions.state")
          local line = action_state.get_current_line()
          M.telescope(
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

function M.on_load(name, fn)
  local Config = require("lazy.core.config")
  if Config.plugins[name] and Config.plugins[name]._.loaded then
    vim.schedule(function()
      fn(name)
    end)
  else
    vim.api.nvim_create_autocmd("User", {
      pattern = "LazyLoad",
      callback = function(event)
        if event.data == name then
          fn(name)
          return true
        end
      end,
    })
  end
end

return M
