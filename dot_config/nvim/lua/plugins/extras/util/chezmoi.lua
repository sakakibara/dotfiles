local pick_chezmoi = function()
  if Util.pick.picker.name == "telescope" then
    require("telescope").extensions.chezmoi.find_files()
  elseif Util.pick.picker.name == "fzf" then
    local fzf_lua = require("fzf-lua")
    local results = require("chezmoi.commands").list({})
    local chezmoi = require("chezmoi.commands")

    local opts = {
      fzf_opts = {},
      fzf_colors = true,
      actions = {
        ["default"] = function(selected)
          chezmoi.edit({
            targets = { "~/" .. selected[1] },
            args = { "--watch" },
          })
        end,
      },
    }
    fzf_lua.fzf_exec(results, opts)
  end
end

return {
  {
    "alker0/chezmoi.vim",
    init = function()
      vim.g["chezmoi#use_tmp_buffer"] = 1
      vim.g["chezmoi#source_dir_path"] = os.getenv("HOME") .. "/.local/share/chezmoi"
    end,
  },

  {
    "xvzc/chezmoi.nvim",
    keys = {
      {
        "<leader>sz",
        pick_chezmoi,
        desc = "Chezmoi",
      },
    },
    opts = {
      edit = {
        watch = false,
        force = false,
      },
      notification = {
        on_open = true,
        on_apply = true,
        on_watch = false,
      },
      telescope = {
        select = { "<CR>" },
      },
    },
    init = function()
      vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
        pattern = { os.getenv("HOME") .. "/.local/share/chezmoi/*" },
        callback = function()
          vim.schedule(require("chezmoi.commands.__edit").watch)
        end,
      })
    end,
  },
}
