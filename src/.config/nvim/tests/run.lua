-- Usage: nvim --headless -l tests/run.lua
vim.opt.rtp:prepend(vim.fn.getcwd())

-- Isolate git operations from the user's global config. Tests fixture
-- `git init` + commit; if the user's gitconfig has commit.gpgsign=true
-- (e.g. SSH commit signing via 1Password), the fixture commits would
-- try to sign and fail wherever the agent isn't reachable (CI, locked
-- sessions). Per-repo user.email / user.name are still set explicitly
-- by each fixture, so blanking the global config is safe.
vim.env.GIT_CONFIG_GLOBAL = "/dev/null"
vim.env.GIT_CONFIG_SYSTEM = "/dev/null"

-- In-memory clipboard so tests touching `+`/`*` registers don't depend on
-- xclip/xsel/wl-copy being installed (headless CI Linux has no provider).
do
  local plus, star = { "" }, { "" }
  vim.g.clipboard = {
    name = "test-stub",
    copy = {
      ["+"] = function(lines) plus = lines end,
      ["*"] = function(lines) star = lines end,
    },
    paste = {
      ["+"] = function() return plus end,
      ["*"] = function() return star end,
    },
    cache_enabled = 0,
  }
end

require("lib").init()

for _, spec in ipairs(vim.fn.sort(vim.fn.glob(vim.fn.getcwd() .. "/tests/**/*_spec.lua", true, true))) do
  dofile(spec)
end

require("tests.helpers").report_and_exit()
