local T = require("tests.helpers")

local function with_state(content, fn)
  local path = vim.fn.tempname()
  if content then vim.fn.writefile({ content }, path) end

  -- Force a fresh require so any caching in lib/theme.lua resets per test.
  package.loaded["lib.theme"] = nil
  local theme = require("lib.theme")
  local original_path = theme.path
  theme.path = function() return path end

  local ok, err = pcall(fn, theme)
  theme.path = original_path
  pcall(vim.fn.delete, path)
  if not ok then error(err) end
end

T.describe("lib.theme.read", function()
  T.it("parses family/variant from a valid state file", function()
    with_state("catppuccin/frappe", function(theme)
      T.eq(theme.read(), { family = "catppuccin", variant = "frappe" })
    end)
  end)

  T.it("returns defaults when state file is missing", function()
    with_state(nil, function(theme)
      T.eq(theme.read(), { family = "catppuccin", variant = "mocha" })
    end)
  end)

  T.it("parses no-variant state (family without slash)", function()
    with_state("dracula", function(theme)
      T.eq(theme.read(), { family = "dracula", variant = "" })
    end)
  end)

  T.it("returns defaults when state file is empty", function()
    with_state("", function(theme)
      T.eq(theme.read(), { family = "catppuccin", variant = "mocha" })
    end)
  end)

  T.it("preserves family/variant exactly (no case folding)", function()
    with_state("Tokyonight/Storm", function(theme)
      T.eq(theme.read(), { family = "Tokyonight", variant = "Storm" })
    end)
  end)

  T.it("path() honors XDG_STATE_HOME", function()
    local saved = vim.env.XDG_STATE_HOME
    vim.env.XDG_STATE_HOME = "/tmp/xdgs-test"
    package.loaded["lib.theme"] = nil
    local theme = require("lib.theme")
    T.eq(theme.path(), "/tmp/xdgs-test/dotfiles/theme")
    vim.env.XDG_STATE_HOME = saved
  end)
end)
