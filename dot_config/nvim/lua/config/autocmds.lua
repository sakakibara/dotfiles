vim.api.nvim_create_autocmd("BufWritePre", {
  group = vim.api.nvim_create_augroup("auto_create_dir", { clear = true }),
  callback = function(event)
    local file = vim.loop.fs_realpath(event.match) or event.match
    local force = vim.v.cmdbang
    local dir = vim.fn.fnamemodify(file, ":p:h")
    local message = "'" .. dir .. "' does not exist. Create? [y/N] "

    -- This handles URLs using netrw. See ':help netrw-transparent' for details.
    if dir:find("%l+://") == 1 then
      return
    end

    if vim.fn.isdirectory(dir) == 0 then
      if force == 1
        or vim.regex("^y\\%[es]$"):match_str(vim.fn.input(message)) then
        vim.fn.mkdir(dir, "p")
      end
    end
  end,
})

local vmr_augrp = vim.api.nvim_create_augroup("visual_mode_relnum", {})
vim.api.nvim_create_autocmd("ModeChanged", {
  group = vmr_augrp,
  pattern = "*:[V\x16]*",
  callback = function() vim.wo.relativenumber = vim.wo.number end,
  desc = "Show relative line numbers",
})
vim.api.nvim_create_autocmd("ModeChanged", {
  group = vmr_augrp,
  pattern = '[V\x16]*:*',
  callback = function() vim.wo.relativenumber = string.find(vim.fn.mode(), '^[V\22]') ~= nil end,
  desc = "Hide relative line numbers",
})
