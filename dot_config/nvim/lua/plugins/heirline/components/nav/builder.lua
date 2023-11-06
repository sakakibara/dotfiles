local SegmentSeparator = require("plugins.heirline.components.segment_separator")
local icons = require("config.icons")

local M = {}

function M.symbol_segments(opts)
  opts = opts or {}
  return function(self)
    local children = {}
    local data = self.data or {}
    local is_empty = vim.tbl_isempty(data)
    if opts.prefix and not is_empty then
      children[#children + 1] = SegmentSeparator
    end
    local start_index = 0
    if opts.max_depth and opts.max_depth > 0 then
      start_index = #data - opts.max_depth
      if start_index > 0 then
        children[#children + 1] = {
          provider = icons.status.Ellipsis,
          hl = { fg = "gray" },
        }
        children[#children + 1] = SegmentSeparator
      end
    end
    for i, d in ipairs(data) do
      if i > start_index then
        local pos = self.enc(d.lnum, d.col, self.winnr)
        local child = {
          {
            provider = d.icon,
            hl = { fg = self.type_hl[d.kind] },
          },
          {
            provider = function()
              local symbol = string.gsub(d.name, "%%", "%%%%"):gsub("%s*->%s*", "")
              if opts.max_char and opts.max_char > 0 then
                symbol = symbol:sub(-opts.max_char) .. icons.status.Ellipsis
              end
              return symbol
            end,
          },
          on_click = {
            callback = function(_, minwid)
              local line, col, winnr = self.dec(minwid)
              vim.api.nvim_win_set_cursor(vim.fn.win_getid(winnr), { line, col })
            end,
            minwid = pos,
            name = "heirline_aerial",
          },
        }
        if #data > 1 and i < #data then
          child[#child + 1] = SegmentSeparator
        end
        children[#children + 1] = child
      end
      if opts.suffix and not is_empty then
        children[#children + 1] = SegmentSeparator
      end
    end
    self[1] = self:new(children, 1)
  end
end

return M
