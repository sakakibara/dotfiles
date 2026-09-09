local T = require("tests.helpers")

local function fresh()
  package.loaded["lib.parsers"] = nil
  local M = require("lib.parsers")
  M._reset()
  return M
end

T.describe("lib.parsers", function()
  T.it("registers a single parser against one filetype", function()
    local M = fresh()
    M.add("lua", { ft = "lua" })
    T.eq(M.list_for_ft("lua"), { "lua" })
  end)

  T.it("registers many parsers in one call (variadic)", function()
    local M = fresh()
    M.add("rust", "ron", { ft = "rust" })
    T.eq(M.list_for_ft("rust"), { "ron", "rust" })
  end)

  T.it("registers one parser against multiple filetypes via ft = { ... }", function()
    local M = fresh()
    M.add("bash", { ft = { "bash", "sh", "zsh" } })
    T.eq(M.list_for_ft("bash"), { "bash" })
    T.eq(M.list_for_ft("sh"),   { "bash" })
    T.eq(M.list_for_ft("zsh"),  { "bash" })
  end)

  T.it("dedupes identical (parser, ft) pairs across calls", function()
    local M = fresh()
    M.add("lua", { ft = "lua" })
    M.add("lua", { ft = "lua" })
    T.eq(M.list_for_ft("lua"), { "lua" })
  end)

  T.it("list_for_ft returns sorted results", function()
    local M = fresh()
    M.add("zoo", "alpha", "mid", { ft = "x" })
    T.eq(M.list_for_ft("x"), { "alpha", "mid", "zoo" })
  end)

  T.it("list_for_ft returns empty table for unknown filetype", function()
    local M = fresh()
    T.eq(M.list_for_ft("nonexistent"), {})
  end)

  T.it("fts() returns every registered ft, sorted", function()
    local M = fresh()
    M.add("rust", { ft = "rust" })
    M.add("c",    { ft = "c" })
    M.add("lua",  { ft = "lua" })
    T.eq(M.fts(), { "c", "lua", "rust" })
  end)

  T.it("fts() returns empty before any registration", function()
    local M = fresh()
    T.eq(M.fts(), {})
  end)

  T.it("errors when ft is omitted", function()
    local M = fresh()
    local ok, err = pcall(M.add, "x")
    T.eq(ok, false)
    T.truthy(tostring(err):find("ft is required"))
  end)

  T.it("_reset clears the registry", function()
    local M = fresh()
    M.add("lua", { ft = "lua" })
    M._reset()
    T.eq(M.list_for_ft("lua"), {})
    T.eq(M.fts(), {})
  end)
end)

T.describe("lib.parsers.lang_for_ft", function()
  T.it("returns the filetype's language when its parser is registered", function()
    local M = fresh()
    M.add("lua", "luadoc", { ft = "lua" })
    T.eq(M.lang_for_ft("lua"), "lua")
  end)

  T.it("returns nil when only sibling parsers are registered", function()
    local M = fresh()
    M.add("c_sharp", "razor", { ft = "vb" })
    T.eq(M.lang_for_ft("vb"), nil)
  end)

  T.it("follows a registered language alias", function()
    local M = fresh()
    vim.treesitter.language.register("sql", { "plsql_spec_alias" })
    M.add("sql", { ft = "plsql_spec_alias" })
    T.eq(M.lang_for_ft("plsql_spec_alias"), "sql")
  end)

  T.it("returns nil for an unregistered filetype", function()
    local M = fresh()
    T.eq(M.lang_for_ft("nonexistent"), nil)
  end)
end)

T.describe("lib.parsers.src_block_for_buf", function()
  local function with_organ(stub, fn)
    local prev = package.loaded["organ"]
    package.loaded["organ"] = stub
    local ok, err = pcall(fn)
    package.loaded["organ"] = prev
    if not ok then error(err) end
  end

  T.it("delegates to organ.src_block_parsers and forwards the bufnr", function()
    local M = fresh()
    with_organ({
      src_block_parsers = function(b) return { "zig", "nix", _bufnr = b } end,
    }, function()
      local out = M.src_block_for_buf(42)
      T.eq(out, { "zig", "nix", _bufnr = 42 })
    end)
  end)

  T.it("returns empty when organ lacks the helper (older pin)", function()
    local M = fresh()
    with_organ({}, function()
      T.eq(M.src_block_for_buf(0), {})
    end)
  end)

  T.it("returns empty when organ.src_block_parsers is not a function", function()
    local M = fresh()
    with_organ({ src_block_parsers = "nope" }, function()
      T.eq(M.src_block_for_buf(0), {})
    end)
  end)

  T.it("returns empty when the helper errors", function()
    local M = fresh()
    with_organ({ src_block_parsers = function() error("boom") end }, function()
      T.eq(M.src_block_for_buf(0), {})
    end)
  end)

  T.it("returns empty when the helper returns a non-table", function()
    local M = fresh()
    with_organ({ src_block_parsers = function() return nil end }, function()
      T.eq(M.src_block_for_buf(0), {})
    end)
  end)
end)

T.describe("lib.parsers.start_for_buf", function()
  local function with_start(impl, fn)
    local real = vim.treesitter.start
    vim.treesitter.start = impl
    local ok, err = pcall(fn)
    vim.treesitter.start = real
    if not ok then error(err, 0) end
  end

  local function current_buf_with_ft(ft)
    local b = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_set_current_buf(b)
    vim.bo[b].filetype = ft
    vim.bo[b].indentexpr = ""
    vim.wo[0][0].foldmethod = "manual"
    return b
  end

  T.it("does not start where the filetype has no registered parser", function()
    local M = fresh()
    local b = current_buf_with_ft("zzz")
    local started = {}
    with_start(function(_, lang) started[#started + 1] = lang end, function()
      T.eq(M.start_for_buf(b), false)
    end)
    T.eq(started, {})
    T.eq(vim.bo[b].indentexpr, "")
    T.eq(vim.wo[0][0].foldmethod, "manual")
  end)

  T.it("starts the registered parser and switches folds and indent to it", function()
    local M = fresh()
    M.add("zzz", { ft = "zzz" })
    local b = current_buf_with_ft("zzz")
    local started = {}
    with_start(function(buf, lang) started[#started + 1] = { buf, lang } end, function()
      T.eq(M.start_for_buf(b), true)
    end)
    T.eq(started, { { b, "zzz" } })
    T.eq(vim.bo[b].indentexpr, "v:lua.require'nvim-treesitter'.indentexpr()")
    T.eq(vim.wo[0][0].foldmethod, "expr")
    T.eq(vim.wo[0][0].foldexpr, "v:lua.vim.treesitter.foldexpr()")
  end)

  T.it("leaves the ftplugin's folds and indent in force when the parser fails to start", function()
    local M = fresh()
    M.add("zzz", { ft = "zzz" })
    local b = current_buf_with_ft("zzz")
    with_start(function() error("no parser") end, function()
      T.eq(M.start_for_buf(b), false)
    end)
    T.eq(vim.bo[b].indentexpr, "")
    T.eq(vim.wo[0][0].foldmethod, "manual")
  end)

  T.it("never starts on a bigfile buffer, parser or not", function()
    local M = fresh()
    M.add("bigfile", { ft = "bigfile" })
    local b = current_buf_with_ft("bigfile")
    local started = {}
    with_start(function(_, lang) started[#started + 1] = lang end, function()
      T.eq(M.start_for_buf(b), false)
    end)
    T.eq(started, {})
  end)
end)
