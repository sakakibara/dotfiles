local LazyUtil = require("lazy.core.util")

local M = {}

function M.get_clients(opts)
  local ret = {}
  ret = vim.lsp.get_clients(opts)
  return opts and opts.filter and vim.tbl_filter(opts.filter, ret) or ret
end

function M.on_attach(on_attach)
  vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
      local buffer = args.buf
      local client = vim.lsp.get_client_by_id(args.data.client_id)
      if client then
        return on_attach(client, buffer)
      end
    end,
  })
end

function M.setup_dynamic_capability()
  local register_capability = vim.lsp.handlers["client/registerCapability"]
  ---@diagnostic disable-next-line: duplicate-set-field
  vim.lsp.handlers["client/registerCapability"] = function(err, res, ctx)
    ---@diagnostic disable-next-line: no-unknown
    local ret = register_capability(err, res, ctx)
    local client = vim.lsp.get_client_by_id(ctx.client_id)
    local buffer = vim.api.nvim_get_current_buf()
    if client then
      vim.api.nvim_exec_autocmds("User", {
        pattern = "LspDynamicCapability",
        data = { client_id = client.id, buffer = buffer },
      })
    end
    return ret
  end
end

---@param fn fun(client:vim.lsp.Client, buffer):boolean?
---@param opts? {group?: integer}
function M.on_dynamic_capability(fn, opts)
  return vim.api.nvim_create_autocmd("User", {
    pattern = "LspDynamicCapability",
    group = opts and opts.group or nil,
    callback = function(args)
      local client = vim.lsp.get_client_by_id(args.data.client_id)
      local buffer = args.data.buffer ---@type number
      if client then
        return fn(client, buffer)
      end
    end,
  })
end

M._on_supports_method_id = 0

function M.on_supports_method(method, fn)
  M.on_attach(function(client, buffer)
    if client.supports_method(method, { bufnr = buffer }) then
      fn(client, buffer)
    else
      M._on_supports_method_id = M._on_supports_method_id + 1
      local id = M._on_supports_method_id
      local group = vim.api.nvim_create_augroup("on_supports_method_" .. id, { clear = true })
      M.on_dynamic_capability(function(c, b)
        if c == client and b == buffer and client.supports_method(method, { bufnr = buffer }) then
          fn(client, buffer)
          pcall(vim.api.nvim_del_augroup_by_id, group)
        end
      end, { group = group })
      vim.api.nvim_create_autocmd({ "LspDetach", "BufDelete" }, {
        group = group,
        buffer = buffer,
        callback = function()
          pcall(vim.api.nvim_del_augroup_by_id, group)
        end,
      })
    end
  end)
end
function M.on_rename(from, to)
  local clients = M.get_clients()
  for _, client in ipairs(clients) do
    if client.supports_method("workspace/willRenameFiles") then
      local resp = client.request_sync("workspace/willRenameFiles", {
        files = {
          {
            oldUri = vim.uri_from_fname(from),
            newUri = vim.uri_from_fname(to),
          },
        },
      }, 1000, 0)
      if resp and resp.result ~= nil then
        vim.lsp.util.apply_workspace_edit(resp.result, client.offset_encoding)
      end
    end
  end
end

function M.get_config(server)
  local configs = require("lspconfig.configs")
  return rawget(configs, server)
end

function M.disable(server, cond)
  local util = require("lspconfig.util")
  local def = M.get_config(server)
  ---@diagnostic disable-next-line: undefined-field
  def.document_config.on_new_config = util.add_hook_before(def.document_config.on_new_config, function(config, root_dir)
    if cond(root_dir, config) then
      config.enabled = false
    end
  end)
end

function M.formatter(opts)
  opts = opts or {}
  local filter = opts.filter or {}
  filter = type(filter) == "string" and { name = filter } or filter
  local ret = {
    name = "LSP",
    primary = true,
    priority = 1,
    format = function(buf)
      M.format(LazyUtil.merge(filter, { bufnr = buf }))
    end,
    sources = function(buf)
      local clients = M.get_clients(LazyUtil.merge(filter, { bufnr = buf }))
      local ret = vim.tbl_filter(function(client)
        return client.supports_method("textDocument/formatting")
          or client.supports_method("textDocument/rangeFormatting")
      end, clients)
      return vim.tbl_map(function(client)
        return client.name
      end, ret)
    end,
  }
  return LazyUtil.merge(ret, opts)
end

function M.format(opts)
  opts = vim.tbl_deep_extend(
    "force",
    {},
    opts or {},
    require("util.plugin").opts("nvim-lspconfig").format or {},
    require("util.plugin").opts("conform.nvim").format or {}
  )
  local ok, conform = pcall(require, "conform")
  if ok then
    opts.formatters = {}
    conform.format(opts)
  else
    vim.lsp.buf.format(opts)
  end
end

M.words = {}
M.words.ns = vim.api.nvim_create_namespace("vim_lsp_references")

---@param opts? {enabled?: boolean}
function M.words.setup(opts)
  opts = opts or {}
  if not opts.enabled then
    return
  end
  local handler = vim.lsp.handlers["textDocument/documentHighlight"]
  ---@diagnostic disable-next-line: duplicate-set-field
  vim.lsp.handlers["textDocument/documentHighlight"] = function(err, result, ctx, config)
    if not vim.api.nvim_buf_is_loaded(ctx.bufnr) then
      return
    end
    return handler(err, result, ctx, config)
  end

  M.on_supports_method("textDocument/documentHighlight", function(_, buf)
    local group = vim.api.nvim_create_augroup("lsp_word_" .. buf, { clear = true })
    vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI", "CursorMoved", "CursorMovedI" }, {
      group = group,
      buffer = buf,
      callback = function(ev)
        if not M.words.at() then
          if ev.event:find("CursorMoved") then
            vim.lsp.buf.clear_references()
          else
            vim.lsp.buf.document_highlight()
          end
        end
      end,
    })
    vim.api.nvim_create_autocmd("LspDetach", {
      callback = function()
        vim.api.nvim_create_augroup("lsp_word_" .. buf, { clear = true })
      end,
    })
  end)
end

function M.words.get()
  local cursor = vim.api.nvim_win_get_cursor(0)
  return vim.tbl_map(function(extmark)
    local ret = {
      from = { extmark[2] + 1, extmark[3] },
      to = { extmark[4].end_row + 1, extmark[4].end_col },
    }
    if cursor[1] >= ret.from[1] and cursor[1] <= ret.to[1] and cursor[2] >= ret.from[2] and cursor[2] <= ret.to[2] then
      ret.current = true
    end
    return ret
  end, vim.api.nvim_buf_get_extmarks(0, M.words.ns, 0, -1, { details = true }))
end

function M.words.at(words)
  for idx, word in ipairs(words or M.words.get()) do
    if word.current then
      return word, idx
    end
  end
end

function M.words.jump(count)
  local words = M.words.get()
  local _, idx = M.words.at(words)
  if not idx then
    return
  end
  local target = words[idx + count]
  if target then
    vim.api.nvim_win_set_cursor(0, target.from)
  end
end

return M
