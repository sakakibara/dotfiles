-- Standalone smoke-check script. Invoked from core.pack.install via
--   nvim --headless --clean -l <this file> <main_module> <rtp_dir>...
-- Tries to require the plugin's main module after prepending the
-- supplied dirs to runtimepath. Exit 0 on success, 1 with the error on
-- stderr otherwise. Runs in a fresh subprocess so any side effects of
-- require (autocmds, options, globals) die with the process and never
-- leak into the calling session.
--
-- This file isn't `require`d from anywhere in the running config; it
-- exists only as the -l target of the smoke subprocess.

local main = arg[1]
if not main or main == "" then
  io.stderr:write("smoke: missing main module argument")
  os.exit(2)
end

for i = 2, #arg do
  vim.opt.runtimepath:prepend(arg[i])
end

local ok, err = pcall(require, main)
if not ok then
  io.stderr:write(tostring(err))
  os.exit(1)
end
