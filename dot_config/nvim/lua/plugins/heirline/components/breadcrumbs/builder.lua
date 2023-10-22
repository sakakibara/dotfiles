local SegmentSeparator = require("plugins.heirline.components.segment_separator")
local icons = require("config.icons")
local upath = require("util.path")

local M = {}

function M.dir_segments(opts)
  opts = opts or {}
  return function(self)
    local children = {}
    local reldirpath = self.reldirpath or nil
    local shorten = opts.max_char and opts.max_char > 0
    local is_root = reldirpath and reldirpath:sub(1, 1) == upath.sep
    if reldirpath then
      if shorten then
        reldirpath = vim.fn.pathshorten(reldirpath, opts.max_char)
      end
      local protocol_start_index = reldirpath:find("://")
      if protocol_start_index ~= nil then
        local protocol = reldirpath:sub(1, protocol_start_index + 2)
        children[#children + 1] = {
          provider = protocol,
          hl = { fg = "gray" },
        }
        reldirpath = reldirpath:sub(protocol_start_index + 3)
      end
      local data = upath.split(reldirpath)
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
      elseif is_root then
        table.insert(data, 1, upath.sep)
      end
      for i, d in ipairs(data) do
        if i > start_index then
          local child = {
            {
              provider = shorten and d .. icons.status.Ellipsis or d,
              hl = { fg = "gray" },
            },
          }
          if #data > 1 and i < #data then
            child[#child + 1] = SegmentSeparator
          end
          children[#children + 1] = child
        end
      end
      if opts.suffix and (not is_empty or is_root) then
        children[#children + 1] = SegmentSeparator
      end
    end
    self[1] = self:new(children, 1)
  end
end

return M
