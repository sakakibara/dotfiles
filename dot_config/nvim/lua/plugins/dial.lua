local M = {}

function M.dial(increment, g)
  local mode = vim.fn.mode(true)
  local is_visual = mode == "v" or mode == "V" or mode == "\22"
  local func = (increment and "inc" or "dec") .. (g and "_g" or "_") .. (is_visual and "visual" or "normal")
  local group = vim.g.dials_by_ft[vim.bo.filetype] or "default"
  return require("dial.map")[func](group)
end

return {
  "monaqa/dial.nvim",
  keys = {
    {
      "<C-a>",
      function()
        return M.dial(true)
      end,
      expr = true,
      desc = "Increment",
      mode = { "n", "v" },
    },
    {
      "<C-x>",
      function()
        return M.dial(false)
      end,
      expr = true,
      desc = "Decrement",
      mode = { "n", "v" },
    },
    {
      "g<C-a>",
      function()
        return M.dial(true, true)
      end,
      expr = true,
      desc = "Increment",
      mode = { "n", "v" },
    },
    {
      "g<C-x>",
      function()
        return M.dial(false, true)
      end,
      expr = true,
      desc = "Decrement",
      mode = { "n", "v" },
    },
  },
  opts = function()
    local augend = require("dial.augend")

    local logical_alias = augend.constant.new({
      elements = { "&&", "||" },
      word = false,
      cyclic = true,
    })

    local ordinal_numbers = augend.constant.new({
      elements = {
        "first",
        "second",
        "third",
        "fourth",
        "fifth",
        "sixth",
        "seventh",
        "eighth",
        "ninth",
        "tenth",
      },
      word = false,
      cyclic = true,
    })

    local weekdays = augend.constant.new({
      elements = {
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday",
      },
      word = true,
      cyclic = true,
    })

    local months = augend.constant.new({
      elements = {
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July",
        "August",
        "September",
        "October",
        "November",
        "December",
      },
      word = true,
      cyclic = true,
    })

    local capitalized_boolean = augend.constant.new({
      elements = {
        "True",
        "False",
      },
      word = true,
      cyclic = true,
    })

    return {
      dials_by_ft = {
        css = "css",
        javascript = "typescript",
        javascriptreact = "typescript",
        json = "json",
        lua = "lua",
        markdown = "markdown",
        python = "python",
        sass = "css",
        scss = "css",
        typescript = "typescript",
        typescriptreact = "typescript",
        vue = "vue",
      },
      groups = {
        default = {
          augend.integer.alias.decimal,
          augend.integer.alias.hex,
          augend.date.alias["%Y/%m/%d"],
          augend.date.alias["%Y-%m-%d"],
          augend.date.alias["%Y年%-m月%-d日"],
          augend.date.alias["%H:%M:%S"],
          augend.date.alias["%H:%M"],
          ordinal_numbers,
          weekdays,
          months,
          capitalized_boolean,
          logical_alias,
          augend.constant.alias.bool,
        },
        vue = {
          augend.constant.new({ elements = { "let", "const" } }),
          augend.hexcolor.new({ case = "lower" }),
          augend.hexcolor.new({ case = "upper" }),
        },
        typescript = {
          augend.constant.new({ elements = { "let", "const" } }),
        },
        css = {
          augend.hexcolor.new({
            case = "lower",
          }),
          augend.hexcolor.new({
            case = "upper",
          }),
        },
        markdown = {
          augend.misc.alias.markdown_header,
        },
        json = {
          augend.integer.alias.decimal,
        },
        lua = {
          augend.constant.new({
            elements = { "and", "or" },
            word = true,
            cyclic = true,
          }),
        },
        python = {
          augend.constant.new({
            elements = { "and", "or" },
          }),
        },
      },
    }
  end,
  config = function(_, opts)
    for name, group in pairs(opts.groups) do
      if name ~= "default" then
        vim.list_extend(group, opts.groups.default)
      end
    end
    require("dial.config").augends:register_group(opts.groups)
    vim.g.dials_by_ft = opts.dials_by_ft
  end,
}
