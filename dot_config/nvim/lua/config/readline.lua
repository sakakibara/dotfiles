local map = vim.keymap
local readline = require 'readline'

map.set('!', '<C-k>', readline.kill_line)
map.set('!', '<C-u>', readline.backward_kill_line)
map.set('!', '<M-d>', readline.kill_word)
map.set('!', '<M-BS>', readline.backward_kill_word)
map.set('!', '<C-w>', readline.unix_word_rubout)
map.set('!', '<C-d>', '<Delete>')  -- delete-char
map.set('!', '<C-h>', '<BS>')      -- backward-delete-char
map.set('!', '<C-a>', readline.beginning_of_line)
map.set('!', '<C-e>', readline.end_of_line)
map.set('!', '<M-f>', readline.forward_word)
map.set('!', '<M-b>', readline.backward_word)
map.set('!', '<C-f>', '<Right>') -- forward-char
map.set('!', '<C-b>', '<Left>')  -- backward-char
map.set('!', '<C-n>', '<Down>')  -- next-line
map.set('!', '<C-p>', '<Up>')    -- previous-line
