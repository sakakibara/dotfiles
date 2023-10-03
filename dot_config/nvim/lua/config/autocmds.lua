local function augroup(name)
  return vim.api.nvim_create_augroup("userconf_" .. name, { clear = true })
end

vim.api.nvim_create_autocmd("BufWritePre", {
  group = augroup("auto_create_dir"),
  callback = function(event)
    if event.match:match("^%w%w+://") then
      return
    end
    local file = vim.loop.fs_realpath(event.match) or event.match
    local force = vim.v.cmdbang
    local dir = vim.fn.fnamemodify(file, ":p:h")
    local message = "'" .. dir .. "' does not exist. Create? [y/N] "
    if vim.fn.isdirectory(dir) == 0 then
      if force == 1
        or vim.regex("^y\\%[es]$"):match_str(vim.fn.input(message)) then
        vim.fn.mkdir(dir, "p")
      end
    end
  end,
})

vim.api.nvim_create_autocmd("ModeChanged", {
  group = augroup("visual_mode_relnum"),
  pattern = "*:[V\x16]*",
  callback = function() vim.wo.relativenumber = vim.wo.number end,
  desc = "Show relative line numbers",
})
vim.api.nvim_create_autocmd("ModeChanged", {
  group = augroup("visual_mode_relnum"),
  pattern = '[V\x16]*:*',
  callback = function() vim.wo.relativenumber = string.find(vim.fn.mode(), '^[V\22]') ~= nil end,
  desc = "Hide relative line numbers",
})

if os.getenv("WSL_DISTRO_NAME") then
  vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
    group = augroup("disable_fixeol"),
    pattern = "/mnt/*",
    callback = function() vim.opt_local.fixendofline = false end,
    desc = "Do not fix end of line",
  })
end
