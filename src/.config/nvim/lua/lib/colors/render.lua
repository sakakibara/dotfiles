-- Manages extmarks for color highlights. Diff-patches against previous
-- state so scrolling/editing don't bulk-clear-and-redraw.
local C = require("lib.colors.color")
local M = {}

M.ns = vim.api.nvim_create_namespace("lib.colors")

-- per-buffer state: { marks = { [mark_id] = key }, by_key = { [key] = mark_id } }
M._state    = {}
-- highlight group cache: { [hex] = true } once nvim_set_hl has been called
M._hl_cache = {}

-- Reset all internal state. Used in tests; not part of public API.
function M.reset()
  for buf in pairs(M._state) do
    if vim.api.nvim_buf_is_valid(buf) then
      vim.api.nvim_buf_clear_namespace(buf, M.ns, 0, -1)
    end
  end
  M._state    = {}
  M._hl_cache = {}
end

local function ensure_hl(color)
  local hex = C.to_hex({ r = color.r, g = color.g, b = color.b, a = 1 })
  local short = hex:sub(2)  -- strip #
  if M._hl_cache[short] then return short end
  vim.api.nvim_set_hl(0, "LibColorsFg_" .. short, { fg = hex })
  vim.api.nvim_set_hl(0, "LibColorsBg_" .. short, {
    bg = hex,
    fg = C.contrast_text({ r = color.r, g = color.g, b = color.b, a = 1 }),
  })
  M._hl_cache[short] = true
  return short
end

local function key_for(detected)
  local hex = C.to_hex({ r = detected.color.r, g = detected.color.g, b = detected.color.b, a = 1 })
  return string.format("%d:%d:%d:%s", detected.lnum, detected.col_s, detected.col_e, hex:sub(2))
end

-- apply(buf, detected_list) — diff against prior state, patch extmarks.
-- Each detected = { lnum, col_s, col_e, color }.
function M.apply(buf, detected)
  if not vim.api.nvim_buf_is_valid(buf) then return end

  local state = M._state[buf] or { marks = {}, by_key = {} }
  M._state[buf] = state

  local new_by_key = {}
  for _, d in ipairs(detected) do
    new_by_key[key_for(d)] = d
  end

  -- Delete marks whose keys are no longer present
  for key, mark_id in pairs(state.by_key) do
    if not new_by_key[key] then
      pcall(vim.api.nvim_buf_del_extmark, buf, M.ns, mark_id)
      state.by_key[key]    = nil
      state.marks[mark_id] = nil
    end
  end

  -- Add marks for new keys
  for key, d in pairs(new_by_key) do
    if not state.by_key[key] then
      local short = ensure_hl(d.color)
      local mark_id = vim.api.nvim_buf_set_extmark(buf, M.ns, d.lnum, d.col_e, {
        virt_text     = { { "●", "LibColorsFg_" .. short } },
        virt_text_pos = "inline",
        right_gravity = false,
      })
      state.by_key[key]    = mark_id
      state.marks[mark_id] = key
    end
  end
end

-- Clear all marks from a buffer.
function M.clear(buf)
  if vim.api.nvim_buf_is_valid(buf) then
    vim.api.nvim_buf_clear_namespace(buf, M.ns, 0, -1)
  end
  M._state[buf] = nil
end

return M
