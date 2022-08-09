local keymap = vim.keymap
local map = require 'dial.map'
local augend = require 'dial.augend'

require("dial.config").augends:register_group{
  default = {
    augend.integer.alias.decimal,
    augend.integer.alias.hex,
    augend.date.alias["%Y/%m/%d"],
    augend.date.alias["%Y-%m-%d"],
    augend.date.alias["%Y年%-m月%-d日"],
    augend.date.alias["%H:%M:%S"],
    augend.date.alias["%H:%M"],
    augend.constant.alias.bool,
    augend.semver.alias.semver,
  },
  visual = {
    augend.integer.alias.decimal,
    augend.integer.alias.hex,
    augend.date.alias["%Y/%m/%d"],
    augend.date.alias["%Y-%m-%d"],
    augend.date.alias["%Y年%-m月%-d日"],
    augend.date.alias["%H:%M:%S"],
    augend.date.alias["%H:%M"],
    augend.constant.alias.bool,
    augend.constant.alias.alpha,
    augend.constant.alias.Alpha,
    augend.semver.alias.semver,
  },
}

keymap.set('n', '<C-a>', map.inc_normal('default'))
keymap.set('n', '<C-x>', map.dec_normal('default'))
keymap.set('v', '<C-a>', map.inc_visual('visual'))
keymap.set('v', '<C-x>', map.dec_visual('visual'))
keymap.set('v', 'g<C-a>', map.inc_gvisual('visual'))
keymap.set('v', 'g<C-x>', map.dec_gvisual('visual'))
