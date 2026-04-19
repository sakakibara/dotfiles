-- lua/core/event.lua
local M = {}

local EXPAND = {
  LazyFile = { "BufReadPost", "BufNewFile", "BufWritePre" },
}

local handlers = {}     -- { [event] = { fn, fn, ... } }
local fired_once = {}   -- { [event] = true }

function M.expand(events)
  events = type(events) == "table" and events or { events }
  local out = {}
  for _, e in ipairs(events) do
    if EXPAND[e] then
      for _, sub in ipairs(EXPAND[e]) do out[#out + 1] = sub end
    else
      out[#out + 1] = e
    end
  end
  return out
end

function M.on(event, fn)
  -- If the event already fired, invoke immediately so late registrations don't silently drop.
  if fired_once[event] then
    local ok, err = xpcall(fn, debug.traceback)
    if not ok then vim.notify("core.event late handler error: " .. err, vim.log.levels.ERROR) end
    return 0
  end
  handlers[event] = handlers[event] or {}
  table.insert(handlers[event], fn)
  return #handlers[event]
end

function M.trigger(event)
  if fired_once[event] then return end
  fired_once[event] = true
  for _, fn in ipairs(handlers[event] or {}) do
    local ok, err = xpcall(fn, debug.traceback)
    if not ok then vim.notify("core.event handler error: " .. err, vim.log.levels.ERROR) end
  end
end

function M.schedule_user_events()
  vim.api.nvim_create_autocmd("UIEnter", {
    once = true,
    callback = function()
      vim.schedule(function() M.trigger("VeryLazy") end)
    end,
  })
end

return M
