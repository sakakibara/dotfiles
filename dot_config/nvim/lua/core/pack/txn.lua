-- Update transaction marker. apply_pending writes the in-flight pending
-- list to disk before any checkout runs and clears it after the apply
-- finishes. If nvim crashes mid-update, the marker survives across
-- restart and Pack.setup picks it up to resume.
--
-- Stored as JSON at $XDG_STATE_HOME/nvim/core-pack/update.txn.json. Only
-- the fields needed to reconstruct apply_pending input are persisted —
-- the spec table itself is rebuilt from M._specs at resume time.

local M = {}

M._path_override = nil  -- tests

local function path()
  if M._path_override then return M._path_override end
  return vim.fn.stdpath("state") .. "/core-pack/update.txn.json"
end

function M.begin(pending)
  local entries = {}
  for _, p in ipairs(pending) do
    entries[#entries + 1] = {
      name         = p.spec.name,
      dir          = p.dir,
      from         = p.from,
      to           = p.to,
      target_rev   = p.target_rev,
      ref          = p.ref,
      checkout_ref = p.checkout_ref,
    }
  end
  local raw = vim.json.encode({ ts = os.time(), pending = entries })
  local p = path()
  vim.fn.mkdir(vim.fn.fnamemodify(p, ":h"), "p")
  local fd = assert(io.open(p, "w"))
  fd:write(raw)
  fd:close()
end

function M.clear()
  vim.fn.delete(path())
end

function M.read()
  local fd = io.open(path(), "r")
  if not fd then return nil end
  local raw = fd:read("*a"); fd:close()
  local ok, data = pcall(vim.json.decode, raw)
  if not ok or type(data) ~= "table" then return nil end
  return data
end

return M
