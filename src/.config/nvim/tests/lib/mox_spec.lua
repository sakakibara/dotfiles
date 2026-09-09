local T = require("tests.helpers")

local mox = require("lib.mox")
local blink = require("lib.mox.blink")

local o = { src_root = "/repo/src", home = "/home/me" }

T.describe("lib.mox.live_of", function()
  T.it("maps a base source to its live path", function()
    T.eq(mox.live_of("/repo/src/.zshrc", o), "/home/me/.zshrc")
    T.eq(mox.live_of("/repo/src/.config/fish/config.fish", o), "/home/me/.config/fish/config.fish")
  end)

  T.it("maps an overlay to its base's live path", function()
    T.eq(mox.live_of("/repo/src/.config/app/config.toml.d/os=darwin.toml", o), "/home/me/.config/app/config.toml")
  end)

  T.it("maps a region fragment to its base's live path", function()
    T.eq(mox.live_of("/repo/src/.zshrc.d/os/darwin", o), "/home/me/.zshrc")
  end)

  T.it("rejects a path outside src", function()
    T.eq(mox.live_of("/repo/data/ids.toml", o), nil)
    T.eq(mox.live_of("/elsewhere/.zshrc", o), nil)
  end)

  T.it("does not treat a bare .d component as an overlay dir", function()
    T.eq(mox.live_of("/repo/src/.d", o), "/home/me/.d")
  end)
end)

T.describe("lib.mox.source_of", function()
  T.it("maps a live path into src", function()
    T.eq(mox.source_of("/home/me/.zshrc", o), "/repo/src/.zshrc")
  end)

  T.it("rejects a path outside home", function()
    T.eq(mox.source_of("/etc/passwd", o), nil)
  end)
end)

T.describe("lib.mox.diff_text", function()
  local mox = require("lib.mox")
  T.it("is the diff when there is one", function()
    T.eq(mox.diff_text({ code = 0, stdout = "-a\n+b\n", stderr = "" }), "-a\n+b\n")
  end)
  T.it("says so when there is none", function()
    T.eq(mox.diff_text({ code = 0, stdout = "", stderr = "" }), "(no difference)")
  end)
  T.it("is nothing when the run failed, so the failure is reported instead", function()
    T.eq(mox.diff_text({ code = 2, stdout = "", stderr = "mox diff: not managed" }), nil)
  end)
end)

T.describe("lib.mox.blink.directive_candidates", function()
  T.it("offers directives right after the marker", function()
    local got = blink.directive_candidates("# mox: ")
    T.truthy(vim.tbl_contains(got, "when"))
    T.truthy(vim.tbl_contains(got, "for"))
  end)

  T.it("offers directives for a partial keyword", function()
    T.truthy(blink.directive_candidates("-- mox: wh"))
    T.truthy(blink.directive_candidates("# mox: keep-e"))
  end)

  T.it("stays quiet on an ordinary line", function()
    T.eq(blink.directive_candidates("export EDITOR=nvim"), nil)
    T.eq(blink.directive_candidates("# mox: when os=darwin and "), nil)
  end)
end)

T.describe("lib.mox.blink.axis_candidates", function()
  T.it("offers axes and facts inside a when expression", function()
    local got = blink.axis_candidates("# mox: when ", { "signing_key" })
    T.truthy(vim.tbl_contains(got, "os"))
    T.truthy(vim.tbl_contains(got, "signing_key"))
  end)

  T.it("offers axes after a boolean operator", function()
    T.truthy(blink.axis_candidates("# mox: when os=darwin and ", {}))
    T.truthy(blink.axis_candidates("# mox: when not ", {}))
  end)

  T.it("stays quiet outside a when", function()
    T.eq(blink.axis_candidates("# mox: include ", {}), nil)
  end)
end)

T.describe("lib.mox.blink.capture_candidates", function()
  T.it("offers namespaces after an open angle", function()
    local got = blink.capture_candidates("email = <", {})
    T.truthy(vim.tbl_contains(got, "machine."))
    T.truthy(vim.tbl_contains(got, "secret:"))
  end)

  T.it("offers machine fields and facts after machine dot", function()
    local got, prefix = blink.capture_candidates("email = <machine.", { "email", "brew_prefix" })
    T.eq(prefix, "machine.")
    for _, f in ipairs({
      "os", "arch", "hostname", "username", "home",
      "xdg_config_home", "xdg_cache_home", "xdg_data_home", "xdg_state_home",
    }) do
      T.truthy(vim.tbl_contains(got, f))
    end
    T.eq(vim.tbl_contains(got, "machine"), false)
    T.truthy(vim.tbl_contains(got, "brew_prefix"))
    T.truthy(vim.tbl_contains(got, "email"))
  end)

  T.it("stays quiet with no open capture", function()
    T.eq(blink.capture_candidates("plain text", {}), nil)
  end)
end)

T.describe("lib.mox.blink.read_facts", function()
  T.it("reads top-level keys and caches by mtime", function()
    local path = vim.fn.tempname()
    local f = assert(io.open(path, "w"))
    f:write('email = "a@b.c"\nprofile = "work"\n')
    f:close()
    T.eq(blink.read_facts(path), { "email", "profile" })
    T.eq(blink.read_facts(path), { "email", "profile" })
    os.remove(path)
    T.eq(blink.read_facts(path), {})
  end)
end)

T.describe("lib.mox.blink.read_derived_facts", function()
  T.it("reads the name of every derived fact row", function()
    local path = vim.fn.tempname()
    local f = assert(io.open(path, "w"))
    f:write('# comment\n[[facts]]\nname = "brew_prefix"\nenv = "HOMEBREW_PREFIX"\n\n[[facts]]\nname = "cargo_home"\n')
    f:close()
    T.eq(blink.read_derived_facts(path), { "brew_prefix", "cargo_home" })
    os.remove(path)
    T.eq(blink.read_derived_facts(path), {})
  end)
end)

T.describe("lib.mox.blink.fact_names", function()
  T.it("returns the same sorted list however many times it is asked", function()
    local dir = vim.fn.tempname()
    vim.fn.mkdir(dir .. "/mox", "p")
    vim.fn.mkdir(dir .. "/repo/data", "p")
    vim.fn.writefile({ 'profile = "work"', 'email = "a@b.c"' }, dir .. "/mox/facts.toml")
    vim.fn.writefile({ "[[facts]]", 'name = "zzz_derived"', 'candidates = ["/tmp"]', "", "[[facts]]", 'name = "aaa_derived"', 'candidates = ["/tmp"]' }, dir .. "/repo/data/facts.toml")
    local prev_cfg, prev_repo = vim.env.XDG_CONFIG_HOME, vim.env.MOX_REPO
    vim.env.XDG_CONFIG_HOME = dir
    vim.env.MOX_REPO = dir .. "/repo"
    local first = blink.fact_names()
    local second = blink.fact_names()
    vim.env.XDG_CONFIG_HOME, vim.env.MOX_REPO = prev_cfg, prev_repo
    T.eq(first, { "aaa_derived", "email", "profile", "zzz_derived" })
    T.eq(second, first)
  end)

  T.it("reads the machine file it is pointed at, not the last one it read", function()
    local one, two = vim.fn.tempname(), vim.fn.tempname()
    for _, d in ipairs({ one, two }) do
      vim.fn.mkdir(d .. "/mox", "p")
      vim.fn.mkdir(d .. "/repo", "p")
    end
    vim.fn.writefile({ 'email = "a@b.c"' }, one .. "/mox/facts.toml")
    vim.fn.writefile({ 'zzz = "1"' }, two .. "/mox/facts.toml")
    local prev_cfg, prev_repo = vim.env.XDG_CONFIG_HOME, vim.env.MOX_REPO
    vim.env.XDG_CONFIG_HOME, vim.env.MOX_REPO = one, one .. "/repo"
    local first = blink.fact_names()
    vim.env.XDG_CONFIG_HOME, vim.env.MOX_REPO = two, two .. "/repo"
    local second = blink.fact_names()
    vim.env.XDG_CONFIG_HOME, vim.env.MOX_REPO = prev_cfg, prev_repo
    T.eq(first, { "email" })
    T.eq(second, { "zzz" })
  end)

  T.it("merges the repo's derived facts with the machine's own", function()
    local dir = vim.fn.tempname()
    vim.fn.mkdir(dir .. "/mox", "p")
    vim.fn.mkdir(dir .. "/repo/data", "p")
    vim.fn.writefile({ 'email = "a@b.c"' }, dir .. "/mox/facts.toml")
    vim.fn.writefile({ "[[facts]]", 'name = "only_derived"', 'candidates = ["/tmp"]' }, dir .. "/repo/data/facts.toml")
    local prev_cfg, prev_repo = vim.env.XDG_CONFIG_HOME, vim.env.MOX_REPO
    vim.env.XDG_CONFIG_HOME = dir
    vim.env.MOX_REPO = dir .. "/repo"
    local names = blink.fact_names()
    vim.env.XDG_CONFIG_HOME, vim.env.MOX_REPO = prev_cfg, prev_repo
    T.truthy(vim.tbl_contains(names, "email"))
    T.truthy(vim.tbl_contains(names, "only_derived"))
  end)
end)

T.describe("lib.mox.blink directive and axis vocabulary", function()
  T.it("offers the directives mox 0.10 accepts", function()
    local got = blink.directive_candidates("# mox: ")
    for _, name in ipairs({ "when", "for", "keep-empty", "completions", "own", "disown", "check", "default" }) do
      T.truthy(vim.tbl_contains(got, name))
    end
  end)

  T.it("does not offer the deleted path axis", function()
    local got = blink.axis_candidates("# mox: when ", {})
    T.truthy(vim.tbl_contains(got, "hostname"))
    T.eq(vim.tbl_contains(got, "path"), false)
  end)
end)

T.describe("lib.mox.is_generator", function()
  local Mx = require("lib.mox")
  local function with_source(text, fn)
    local path = vim.fn.tempname()
    local fh = assert(io.open(path, "w"))
    fh:write(text)
    fh:close()
    fn(path)
    os.remove(path)
  end

  T.it("recognises a for-into generator and a completions generator", function()
    with_source("# mox: for id in \"data/x.toml\" into \"id-<id.slug>.inc\"\nx\n", function(p) T.eq(Mx.is_generator(p), true) end)
    with_source("# mox: completions zsh\n", function(p) T.eq(Mx.is_generator(p), true) end)
  end)

  T.it("leaves an ordinary gated source alone", function()
    with_source("# mox: when os=darwin\nx\n# mox: end\n", function(p) T.eq(Mx.is_generator(p), false) end)
  end)
end)

T.describe("lib.mox.touched_nothing", function()
  local Mx = require("lib.mox")
  T.it("is true only when nothing was written, removed or kept", function()
    T.eq(Mx.touched_nothing({ code = 0, stdout = "Applied: 0 written, 0 removed, 0 unchanged, 3 skipped, 0 drifted, 0 failed\n" }), true)
    T.eq(Mx.touched_nothing({ code = 0, stdout = "Applied: 1 written, 0 removed, 0 unchanged, 0 skipped, 0 drifted, 0 failed\n" }), false)
    T.eq(Mx.touched_nothing({ code = 0, stdout = "  removed ~/x (composes to nothing)\nApplied: 0 written, 1 removed, 0 unchanged, 0 skipped, 0 drifted, 0 failed\n" }), false)
    T.eq(Mx.touched_nothing({ code = 0, stdout = "", stderr = "Applied: 0 written, 0 removed, 2 unchanged, 0 skipped, 0 drifted, 0 failed\n" }), false)
  end)
end)

T.describe("lib.mox.blink fact caches", function()
  T.it("keys on the path as well as the mtime", function()
    local B = require("lib.mox.blink")
    local a_path, b_path = vim.fn.tempname(), vim.fn.tempname()
    for path, name in pairs({ [a_path] = "alpha", [b_path] = "beta" }) do
      local fh = assert(io.open(path, "w"))
      fh:write(name .. ' = "1"\n')
      fh:close()
    end
    local same = os.time() - 10
    vim.uv.fs_utime(a_path, same, same)
    vim.uv.fs_utime(b_path, same, same)
    T.eq(B.read_facts(a_path), { "alpha" })
    T.eq(B.read_facts(b_path), { "beta" })
    os.remove(a_path)
    os.remove(b_path)
  end)
end)

T.describe("lib.mox.repo", function()
  local Mx = require("lib.mox")
  T.it("follows XDG_DATA_HOME when MOX_REPO is unset", function()
    local saved_repo, saved_data = vim.env.MOX_REPO, vim.env.XDG_DATA_HOME
    vim.env.MOX_REPO = nil
    vim.env.XDG_DATA_HOME = "/srv/data"
    T.eq(Mx.repo(), "/srv/data/mox/dotfiles")
    vim.env.XDG_DATA_HOME = saved_data
    vim.env.MOX_REPO = saved_repo
  end)
end)
