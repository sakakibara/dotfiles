local uv = vim.uv
local M = {}

M.spec = { "lsp", { ".git", "lua" }, "cwd" }
M.cache = {}

local detectors = {}

function detectors.cwd() return { uv.cwd() } end

function detectors.buf(buf)
  local p = vim.api.nvim_buf_get_name(buf)
  if p == "" then return {} end
  local d = vim.fn.fnamemodify(p, ":p:h")
  return { uv.fs_realpath(d) or d }
end

function detectors.lsp(buf)
  local bufpath = vim.api.nvim_buf_get_name(buf)
  if bufpath == "" then return {} end
  local ignore = vim.g.root_lsp_ignore or {}
  local roots = {}
  for _, client in pairs(vim.lsp.get_clients({ bufnr = buf })) do
    if not vim.tbl_contains(ignore, client.name) then
      for _, ws in ipairs(client.workspace_folders or {}) do
        roots[#roots + 1] = vim.uri_to_fname(ws.uri)
      end
      if client.root_dir then roots[#roots + 1] = client.root_dir end
    end
  end
  return roots
end

function detectors.pattern(buf, patterns)
  patterns = type(patterns) == "string" and { patterns } or patterns
  local path = vim.api.nvim_buf_get_name(buf)
  path = path == "" and uv.cwd() or vim.fn.fnamemodify(path, ":p:h")
  local found = vim.fs.find(patterns, { path = path, upward = true })[1]
  return found and { vim.fs.dirname(found) } or {}
end

local function resolve(s, buf)
  if s == "lsp" then return detectors.lsp(buf) end
  if s == "cwd" then return detectors.cwd() end
  if s == "buf" then return detectors.buf(buf) end
  if type(s) == "table" then return detectors.pattern(buf, s) end
  return {}
end

-- Run a spec against a buffer. Default spec is M.spec; passing a custom
-- spec lets the named accessors below reuse the same engine. Cache is
-- only consulted for the default spec — custom specs are cheap enough
-- that caching them isn't worth the invalidation surface.
function M.detect(buf, spec)
  buf = buf or vim.api.nvim_get_current_buf()
  if spec == nil and M.cache[buf] then return M.cache[buf] end
  spec = spec or M.spec
  for _, s in ipairs(spec) do
    local found = resolve(s, buf)
    if #found > 0 then
      if spec == M.spec then M.cache[buf] = found[1] end
      return found[1]
    end
  end
  return uv.cwd()
end

-- Closest .git ancestor, falling back to cwd. Used by lazygit and
-- similar where you always want a working directory.
function M.git(buf) return M.detect(buf, { { ".git" }, "cwd" }) end

-- Directory of the buffer's file, falling back to cwd for unnamed
-- buffers (scratch, pickers, terminals).
function M.buf(buf) return M.detect(buf, { "buf", "cwd" }) end

function M.setup()
  vim.api.nvim_create_autocmd({ "LspAttach", "BufEnter", "DirChanged" }, {
    group = vim.api.nvim_create_augroup("Lib.root", { clear = true }),
    callback = function(args)
      -- DirChanged invalidates everything since cwd-fallback entries
      -- are now stale; per-buffer events drop the affected entry.
      if args.event == "DirChanged" then
        M.cache = {}
      else
        M.cache[args.buf] = nil
      end
    end,
  })
end

setmetatable(M, { __call = function(m, buf) return m.detect(buf) end })

return M
