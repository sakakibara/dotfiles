local M = {}

function M.path()
  return vim.fn.stdpath("config") .. "/supply-chain.json"
end

function M.release_min_age_days(path)
  local f = io.open(path or M.path(), "r")
  if not f then return nil end
  local body = f:read("*a")
  f:close()
  local ok, data = pcall(vim.json.decode, body)
  if not ok or type(data) ~= "table" then return nil end
  local minutes = tonumber(data.release_min_age_minutes)
  if not minutes or minutes < 0 or minutes ~= math.floor(minutes) then return nil end
  return minutes / 1440
end

return M
