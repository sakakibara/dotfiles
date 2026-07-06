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
