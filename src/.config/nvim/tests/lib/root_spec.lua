local T = require("tests.helpers")

local function fresh()
  package.loaded["lib.root"] = nil
  return require("lib.root")
end

local function norm(p) return vim.fs.normalize(vim.uv.fs_realpath(p) or p) end

T.describe("lib.root.buf", function()
  T.it("returns the buffer file's directory (realpath-resolved)", function()
    local R = fresh()
    local f = vim.fn.tempname()
    vim.fn.writefile({ "x" }, f)
    local buf = vim.fn.bufadd(f); vim.fn.bufload(buf)
    T.eq(norm(R.buf(buf)), norm(vim.fn.fnamemodify(f, ":p:h")))
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end)

  T.it("falls back to cwd for unnamed buffers", function()
    local R = fresh()
    local buf = vim.api.nvim_create_buf(false, true)
    T.eq(R.buf(buf), vim.uv.cwd())
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end)
end)

T.describe("lib.root.git", function()
  T.it("walks upward to the nearest .git directory", function()
    local R = fresh()
    local repo = vim.fn.tempname()
    vim.fn.mkdir(repo .. "/sub/deep", "p")
    vim.fn.system({ "git", "-C", repo, "init", "-q" })
    vim.fn.writefile({ "x" }, repo .. "/sub/deep/file.txt")
    local buf = vim.fn.bufadd(repo .. "/sub/deep/file.txt"); vim.fn.bufload(buf)
    T.eq(norm(R.git(buf)), norm(repo))
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end)

  T.it("falls back to cwd when no .git ancestor exists", function()
    local R = fresh()
    local d = vim.fn.tempname()
    vim.fn.mkdir(d, "p")
    vim.fn.writefile({ "x" }, d .. "/file.txt")
    local buf = vim.fn.bufadd(d .. "/file.txt"); vim.fn.bufload(buf)
    T.eq(R.git(buf), vim.uv.cwd())
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end)
end)

T.describe("lib.root.detect caching", function()
  T.it("caches results per buffer for the default spec", function()
    local R = fresh()
    local buf = vim.api.nvim_create_buf(false, true)
    local first  = R.detect(buf)
    R.cache[buf] = "/sentinel/from/cache"
    T.eq(R.detect(buf), "/sentinel/from/cache")
    R.cache[buf] = nil
    T.eq(R.detect(buf), first)
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end)

  T.it("does not consult the cache when a custom spec is supplied", function()
    local R = fresh()
    local buf = vim.api.nvim_create_buf(false, true)
    R.cache[buf] = "/sentinel/from/cache"
    -- Custom spec ignores the cache and runs its detectors fresh.
    T.eq(R.detect(buf, { "cwd" }), vim.uv.cwd())
    pcall(vim.api.nvim_buf_delete, buf, { force = true })
  end)
end)
