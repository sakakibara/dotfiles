-- Syntax-check the Lua files passed as arguments. Run with:
--   luajit etc/ci/lua-check.lua <file> [<file> ...]
-- Uses LuaJIT's parser since nvim runs LuaJIT; this is the most accurate
-- check for our nvim config (catches syntax that Lua 5.4 would accept but
-- LuaJIT would reject, and vice versa).
-- Exits 0 if all files parse, non-zero otherwise.

local fails = 0
for _, file in ipairs(arg) do
  local f, err = loadfile(file)
  if not f then
    io.stderr:write(string.format("FAIL: %s (%s)\n", file, tostring(err)))
    fails = fails + 1
  end
end
os.exit(fails == 0 and 0 or 1)
