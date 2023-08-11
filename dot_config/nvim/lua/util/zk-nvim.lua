local M = {}

function M.grep_notes(opts)
  local collection = {}
  local list_opts = { select = { "title", "path", "absPath" } }
  require("zk.api").list(vim.env.ZK_NOTEBOOK_DIR, list_opts, function(_, notes)
    for _, note in ipairs(notes) do
      collection[note.absPath] = note.title or note.path
    end
  end)
  local options = vim.tbl_deep_extend("force", {
    prompt_title = "Notes",
    search_dirs = { vim.env.ZK_NOTEBOOK_DIR },
    disable_coordinates = true,
    path_display = function(_, path)
      return collection[path]
    end,
  }, opts or {})
  require("telescope.builtin").live_grep(options)
end

return M
