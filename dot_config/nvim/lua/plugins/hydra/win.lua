local cmd = require("hydra.keymap-util").cmd
local pcmd = require("hydra.keymap-util").pcmd
local splits = require("smart-splits")

return require("hydra")({
  name = "Windows",
  hint = [[
 ^^^^^^^^^^^^     Move      ^^    Size   ^^   ^^     Split
 ^^^^^^^^^^^^-------------  ^^-----------^^   ^^---------------
 ^ ^ _k_ ^ ^  ^ ^ _K_ ^ ^   ^   _<C-k>_   ^   _s_: horizontally
 _h_ ^ ^ _l_  _H_ ^ ^ _L_   _<C-h>_ _<C-l>_   _v_: vertically
 ^ ^ _j_ ^ ^  ^ ^ _J_ ^ ^   ^   _<C-j>_   ^   _c_: close
 focus^^^^^^  window^^^^^^  ^_=_: equalize^   _z_: maximize
 ^ ^ ^ ^ ^ ^  ^ ^ ^ ^ ^ ^   ^^ ^          ^   _o_: remain only
]],
  config = {
    invoke_on_body = true,
    hint = {
      border = "rounded",
      offset = -1,
    },
  },
  mode = "n",
  body = "<Leader>w",
  heads = {
    { "h", "<C-w>h" },
    { "j", "<C-w>j" },
    { "k", pcmd("wincmd k", "E11", "close") },
    { "l", "<C-w>l" },

    { "H", cmd("WinShift left") },
    { "J", cmd("WinShift down") },
    { "K", cmd("WinShift up") },
    { "L", cmd("WinShift right") },

    {
      "<C-h>",
      function()
        splits.resize_left(1)
      end,
    },
    {
      "<C-j>",
      function()
        splits.resize_down(1)
      end,
    },
    {
      "<C-k>",
      function()
        splits.resize_up(1)
      end,
    },
    {
      "<C-l>",
      function()
        splits.resize_right(1)
      end,
    },
    { "=", "<C-w>=", { desc = "equalize" } },

    { "s", pcmd("split", "E36") },
    { "<C-s>", pcmd("split", "E36"), { desc = false } },
    { "v", pcmd("vsplit", "E36") },
    { "<C-v>", pcmd("vsplit", "E36"), { desc = false } },

    { "w", "<C-w>w", { exit = true, desc = false } },
    { "<C-w>", "<C-w>w", { exit = true, desc = false } },

    { "z", cmd("WindowsMaximize"), { exit = true, desc = "maximize" } },
    { "<C-z>", cmd("WindowsMaximize"), { exit = true, desc = false } },

    { "o", "<C-w>o", { exit = true, desc = "remain only" } },
    { "<C-o>", "<C-w>o", { exit = true, desc = false } },

    { "c", pcmd("close", "E444"), { desc = "close window" } },
    { "<C-c>", pcmd("close", "E444"), { desc = false } },
    { "<C-q>", pcmd("close", "E444"), { desc = false } },

    { "q", nil, { exit = true, desc = false } },
    { "<Esc>", nil, { exit = true, desc = false } },
  },
})
