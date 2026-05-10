local M = {}

function M.init()
  if _G.Lib then return _G.Lib end
  _G.Lib = setmetatable({}, {
    __index = function(t, k)
      local ok, mod = pcall(require, "lib." .. k)
      if ok then
        rawset(t, k, mod)
        return mod
      end
      return nil
    end,
  })
  return _G.Lib
end

return M
