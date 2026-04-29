-- tests/run.lua
-- Usage: nvim --headless -l tests/run.lua
vim.opt.rtp:prepend(vim.fn.getcwd())

-- In-memory clipboard so tests touching `+`/`*` registers don't depend on
-- xclip/xsel/wl-copy being installed (headless CI Linux has no provider).
do
  local plus, star = { "" }, { "" }
  vim.g.clipboard = {
    name = "test-stub",
    copy = {
      ["+"] = function(lines) plus = lines end,
      ["*"] = function(lines) star = lines end,
    },
    paste = {
      ["+"] = function() return plus end,
      ["*"] = function() return star end,
    },
    cache_enabled = 0,
  }
end

require("lib").init()

dofile(vim.fn.getcwd() .. "/tests/event_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/pack_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lsp_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/mode_color_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/statusline_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/format_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lang_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/theme_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/chezmoi_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/pack/lock_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/pack/jobs_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/pack/git_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/pack/version_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/pack/install_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/pack/history_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/pack/ui_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/pack/health_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/pack/log_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/pack/spec_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/pack/refs_spec.lua")

dofile(vim.fn.getcwd() .. "/tests/lib/colors/init_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/color_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/parse_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/tailwind_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/detect_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/render_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/harmony_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/picker_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/format_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/contrast_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/augends_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/gradient_spec.lua")

require("tests.helpers").report_and_exit()
