-- tests/run.lua
-- Usage: nvim --headless -l tests/run.lua
vim.opt.rtp:prepend(vim.fn.getcwd())
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

dofile(vim.fn.getcwd() .. "/tests/lib/colors/init_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/color_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/parse_spec.lua")
dofile(vim.fn.getcwd() .. "/tests/lib/colors/tailwind_spec.lua")

require("tests.helpers").report_and_exit()
