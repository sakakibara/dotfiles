local ulsp = require("util.lsp")
local uplugin = require("util.plugin")

local M = {}

M._keys = nil

function M.get()
  if M._keys then
    return M._keys
  end
  M._keys = {
    { "<Leader>cl", "<Cmd>LspInfo<CR>", desc = "Lsp info" },
    {
      "gd",
      function()
        require("telescope.builtin").lsp_definitions({ reuse_win = true })
      end,
      desc = "Goto definition",
      has = "definition",
    },
    { "gr", "<Cmd>Telescope lsp_references<CR>", desc = "References" },
    { "gD", vim.lsp.buf.declaration, desc = "Goto declaration" },
    {
      "gI",
      function()
        require("telescope.builtin").lsp_implementations({ reuse_win = true })
      end,
      desc = "Goto implementation",
    },
    {
      "gy",
      function()
        require("telescope.builtin").lsp_type_definitions({ reuse_win = true })
      end,
      desc = "Goto t[y]pe Definition",
    },
    { "K", vim.lsp.buf.hover, desc = "Hover" },
    { "gK", vim.lsp.buf.signature_help, desc = "Signature help", has = "signatureHelp" },
    { "<C-k>", vim.lsp.buf.signature_help, mode = "i", desc = "Signature help", has = "signatureHelp" },
    { "<Leader>ca", vim.lsp.buf.code_action, desc = "Code action", mode = { "n", "v" }, has = "codeAction" },
    {
      "<Leader>cA",
      function()
        vim.lsp.buf.code_action({
          context = {
            only = {
              "source",
            },
            diagnostics = {},
          },
        })
      end,
      desc = "Source action",
      has = "codeAction",
    },
    { "<Leader>cc", vim.lsp.codelens.run, desc = "Run codelens", mode = { "n", "v" }, has = "codeLens" },
    { "<Leader>cC", vim.lsp.codelens.refresh, desc = "Refresh & display codelens", mode = { "n" }, has = "codeLens" },
    {
      "]]",
      function()
        ulsp.words.jump(vim.v.count1)
      end,
      has = "documentHighlight",
      desc = "Next reference",
      cond = function()
        return ulsp.words.enabled
      end,
    },
    {
      "[[",
      function()
        ulsp.words.jump(-vim.v.count1)
      end,
      has = "documentHighlight",
      desc = "Prev reference",
      cond = function()
        return ulsp.words.enabled
      end,
    },
  }
  if uplugin.has("inc-rename.nvim") then
    M._keys[#M._keys + 1] = {
      "<Leader>cr",
      function()
        local inc_rename = require("inc_rename")
        return ":" .. inc_rename.config.cmd_name .. " " .. vim.fn.expand("<cword>")
      end,
      expr = true,
      desc = "Rename",
      has = "rename",
    }
  else
    M._keys[#M._keys + 1] = { "<Leader>cr", vim.lsp.buf.rename, desc = "Rename", has = "rename" }
  end
  return M._keys
end

function M.has(buffer, method)
  if type(method) == "table" then
    for _, m in ipairs(method) do
      if M.has(buffer, m) then
        return true
      end
    end
    return false
  end
  method = method:find("/") and method or "textDocument/" .. method
  local clients = ulsp.get_clients({ bufnr = buffer })
  for _, client in ipairs(clients) do
    if client.supports_method(method) then
      return true
    end
  end
  return false
end

function M.resolve(buffer)
  local Keys = require("lazy.core.handler.keys")
  if not Keys.resolve then
    return {}
  end
  local spec = M.get()
  local opts = uplugin.opts("nvim-lspconfig")
  local clients = ulsp.get_clients({ bufnr = buffer })
  for _, client in ipairs(clients) do
    local maps = opts.servers[client.name] and opts.servers[client.name].keys or {}
    vim.list_extend(spec, maps)
  end
  return Keys.resolve(spec)
end

function M.on_attach(_, buffer)
  local Keys = require("lazy.core.handler.keys")
  local keymaps = M.resolve(buffer)

  for _, keys in pairs(keymaps) do
    local has = not keys.has or M.has(buffer, keys.has)
    local cond = not (keys.cond == false or ((type(keys.cond) == "function") and not keys.cond()))

    if has and cond then
      local opts = Keys.opts(keys)
      opts.cond = nil
      opts.has = nil
      opts.silent = opts.silent ~= false
      opts.buffer = buffer
      vim.keymap.set(keys.mode or "n", keys.lhs, keys.rhs, opts)
    end
  end
end

return M
