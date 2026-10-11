local T = require("tests.helpers")

local function reset_sl()
  package.loaded["lib.statusline"] = nil
  return require("lib.statusline")
end

T.describe("lib.statusline segment builders", function()
  T.it("mode_segment returns mode glyph + name", function()
    local sl = reset_sl()
    local s = sl._segments.mode("n")
    T.truthy(s:match("NORMAL"))
    s = sl._segments.mode("i")
    T.truthy(s:match("INSERT"))
  end)

  T.it("scrollbar returns a compact 2-char glyph", function()
    local sl = reset_sl()
    local bar = sl._segments.scrollbar(50, 100)
    T.eq(vim.fn.strchars(bar), 2)
  end)

  T.it("mode_segment labels full mode(1) strings", function()
    local sl = reset_sl()
    T.truthy(sl._segments.mode("niI"):match("NORMAL"))
    T.truthy(sl._segments.mode("nov"):match("O%-PEND"))
    T.truthy(sl._segments.mode("Rvc"):match("V%-RPLCE"))
    T.truthy(sl._segments.mode("ntT"):match("NORMAL"))
    T.truthy(sl._segments.mode("cvr"):match("EX"))
  end)
end)

T.describe("lib.statusline path zones", function()
  T.it("abbreviates home only at a path boundary", function()
    local sl = reset_sl()
    local home = Lib.path.home
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(buf, home .. "ko/dir/file.txt")
    local prev = vim.api.nvim_get_current_buf()
    vim.api.nvim_set_current_buf(buf)
    local s = sl.render()
    vim.api.nvim_set_current_buf(prev)
    vim.api.nvim_buf_delete(buf, { force = true })
    T.eq(s:find("~ko", 1, true), nil)
    T.truthy(s:find(home .. "ko/dir/", 1, true), "full parent path shown")
  end)

  T.it("splits the path against each window's own working directory", function()
    local sl = reset_sl()
    local root = vim.fn.resolve(vim.fn.tempname())
    vim.fn.mkdir(root .. "/sub", "p")
    vim.cmd("tabnew")
    vim.cmd("edit " .. vim.fn.fnameescape(root .. "/sub/f.txt"))
    local w1 = vim.api.nvim_get_current_win()
    vim.cmd("lcd " .. vim.fn.fnameescape(root))
    vim.cmd("split")
    local w2 = vim.api.nvim_get_current_win()
    vim.cmd("lcd " .. vim.fn.fnameescape(root .. "/sub"))
    vim.api.nvim_set_current_win(w1)
    local s1 = sl.render()
    vim.api.nvim_set_current_win(w2)
    local s2 = sl.render()
    vim.cmd("tabclose!")
    vim.fn.delete(root, "rf")
    T.truthy(s1:find("#StslPathRel#", 1, true), "w1 shows sub/ relative to its cwd")
    T.eq(s2:find("#StslPathRel#", 1, true), nil, "w2 has the file directly under its cwd")
  end)
end)

T.describe("lib.statusline segment cache", function()
  local function with_setup(fn)
    local sl = reset_sl()
    local ok, err = pcall(function()
      sl.setup()
      fn(sl)
    end)
    for _, g in ipairs({ "Lib.mode_color", "Lib.statusline.hl", "Lib.statusline.cache" }) do
      pcall(vim.api.nvim_del_augroup_by_name, g)
    end
    if not ok then error(err, 0) end
  end

  local function scratch_file(name)
    local buf = vim.api.nvim_create_buf(true, false)
    vim.api.nvim_buf_set_name(buf, vim.uv.cwd() .. "/" .. name)
    return buf
  end

  T.it("builds the path once per buffer until an invalidating event", function()
    with_setup(function(sl)
      local buf = scratch_file("cached-path.txt")
      local prev = vim.api.nvim_get_current_buf()
      vim.api.nvim_set_current_buf(buf)
      local real = Lib.path.buf_get_name
      local calls = 0
      Lib.path.buf_get_name = function(...) calls = calls + 1; return real(...) end
      local ok, err = pcall(function()
        sl.render(); sl.render(); sl.render()
        T.eq(calls, 1)
        T.eq(sl.render():find(Lib.icons.status.Lock, 1, true), nil)
        vim.bo[buf].readonly = true
        T.truthy(sl.render():find(Lib.icons.status.Lock, 1, true), "readonly mark after OptionSet")
        T.eq(calls, 2)
      end)
      Lib.path.buf_get_name = real
      vim.api.nvim_set_current_buf(prev)
      vim.api.nvim_buf_delete(buf, { force = true })
      if not ok then error(err, 0) end
    end)
  end)

  T.it("rebuilds the LSP segment for the new width after a resize", function()
    with_setup(function(sl)
      local buf = scratch_file("lsp-width.txt")
      vim.cmd("tabnew")
      vim.api.nvim_set_current_buf(buf)
      local real = vim.lsp.get_clients
      vim.lsp.get_clients = function() return { { name = "alpha" }, { name = "beta" } } end
      local ok, err = pcall(function()
        T.truthy(sl.render():find("alpha,beta", 1, true), "wide window lists names")
        vim.cmd("vsplit")
        vim.api.nvim_win_set_width(0, 40)
        vim.api.nvim_exec_autocmds("VimResized", {})
        T.truthy(sl.render():find("lsp:2", 1, true), "narrow window counts clients")
      end)
      vim.lsp.get_clients = real
      vim.cmd("tabclose!")
      vim.api.nvim_buf_delete(buf, { force = true })
      if not ok then error(err, 0) end
    end)
  end)

  T.it("GitSignsUpdate invalidates the buffer it names, not the current one", function()
    with_setup(function()
      local other = scratch_file("git-other.txt")
      local cur = vim.api.nvim_get_current_buf()
      vim.b[other]._sl_cache = { git = "stale" }
      vim.b[cur]._sl_cache = { git = "current" }
      vim.api.nvim_exec_autocmds("User", { pattern = "GitSignsUpdate", data = { buffer = other } })
      T.eq(vim.b[other]._sl_cache.git, nil)
      T.eq(vim.b[cur]._sl_cache.git, "current")
      vim.b[cur]._sl_cache = nil
      vim.api.nvim_buf_delete(other, { force = true })
    end)
  end)
end)
