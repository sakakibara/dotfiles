-- lua/lib/root.lua
local uv = vim.uv
local M = {}

M.spec = { "lsp", { ".git", "lua" }, "cwd" }
M.cache = {}

local detectors = {}

function detectors.cwd()
  return { uv.cwd() }
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

local function resolve(spec, buf)
  if spec == "lsp" then return detectors.lsp(buf) end
  if spec == "cwd" then return detectors.cwd() end
  if type(spec) == "table" then return detectors.pattern(buf, spec) end
  return {}
end

function M.detect(buf)
  buf = buf or vim.api.nvim_get_current_buf()
  for _, s in ipairs(M.spec) do
    local found = resolve(s, buf)
    if #found > 0 then return found[1] end
  end
  return uv.cwd()
end

function M.setup()
  vim.api.nvim_create_autocmd({ "LspAttach", "BufEnter" }, {
    group = vim.api.nvim_create_augroup("Lib.root", { clear = true }),
    callback = function(args) M.cache[args.buf] = nil end,
  })
end

setmetatable(M, { __call = function(m, buf) return m.detect(buf) end })

return M
