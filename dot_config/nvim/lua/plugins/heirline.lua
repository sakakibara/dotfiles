return {
  {
    "rebelot/heirline.nvim",
    event = "UIEnter",
    config = function()
      local conditions = require("heirline.conditions")
      local utils = require("heirline.utils")
      local ulazy = require("util.lazy")
      local upath = require("util.path")
      local icons = require("config.icons")

      local function blend(color1, color2, alpha)
        color1 = type(color1) == "number" and string.format("#%06x", color1) or color1
        color2 = type(color2) == "number" and string.format("#%06x", color2) or color2
        local r1, g1, b1 = color1:match("#(%x%x)(%x%x)(%x%x)")
        local r2, g2, b2 = color2:match("#(%x%x)(%x%x)(%x%x)")
        local r = tonumber(r1, 16) * alpha + tonumber(r2, 16) * (1 - alpha)
        local g = tonumber(g1, 16) * alpha + tonumber(g2, 16) * (1 - alpha)
        local b = tonumber(b1, 16) * alpha + tonumber(b2, 16) * (1 - alpha)
        return "#"
          .. string.format("%02x", math.min(255, math.max(r, 0)))
          .. string.format("%02x", math.min(255, math.max(g, 0)))
          .. string.format("%02x", math.min(255, math.max(b, 0)))
      end

      local function dim(color, n)
        return blend(color, "#000000", n)
      end

      local function setup_colors()
        return {
          bright_bg = utils.get_highlight("Folded").bg,
          bright_fg = utils.get_highlight("Folded").fg,
          red = utils.get_highlight("DiagnosticError").fg,
          dark_red = utils.get_highlight("DiffDelete").bg,
          green = utils.get_highlight("String").fg,
          blue = utils.get_highlight("Function").fg,
          gray = utils.get_highlight("NonText").fg,
          orange = utils.get_highlight("Constant").fg,
          purple = utils.get_highlight("Statement").fg,
          cyan = utils.get_highlight("Special").fg,
          diag_warn = utils.get_highlight("DiagnosticWarn").fg,
          diag_error = utils.get_highlight("DiagnosticError").fg,
          diag_hint = utils.get_highlight("DiagnosticHint").fg,
          diag_info = utils.get_highlight("DiagnosticInfo").fg,
          git_del = utils.get_highlight("diffDeleted").fg,
          git_add = utils.get_highlight("diffAdded").fg,
          git_change = utils.get_highlight("diffChanged").fg,
        }
      end

      local Align = { provider = "%=" }
      local Space = setmetatable({ provider = ' ' }, {
        __call = function(_, n)
          return { provider = string.rep(' ', n) }
        end
      })
      local BreadcrumbSep = {
        provider = "  ",
        hl = { fg = "gray" },
      }
      local LeftCap = {
        provider = '▌',
      }

      local ViMode = {
        init = function(self)
          self.mode = vim.fn.mode(1)
        end,
        static = {
          mode_names = {
            n = "N",
            no = "N?",
            nov = "N?",
            noV = "N?",
            ["no\22"] = "N?",
            niI = "Ni",
            niR = "Nr",
            niV = "Nv",
            nt = "Nt",
            v = "V",
            vs = "Vs",
            V = "V_",
            Vs = "Vs",
            ["\22"] = "^V",
            ["\22s"] = "^V",
            s = "S",
            S = "S_",
            ["\19"] = "^S",
            i = "I",
            ic = "Ic",
            ix = "Ix",
            R = "R",
            Rc = "Rc",
            Rx = "Rx",
            Rv = "Rv",
            Rvc = "Rv",
            Rvx = "Rv",
            c = "C",
            cv = "Ex",
            r = "...",
            rm = "M",
            ["r?"] = "?",
            ["!"] = "!",
            t = "T",
          },
        },
        provider = function(self)
          return icons.status.Mode .. "%-2(" .. self.mode_names[self.mode] .. "%)"
        end,
        hl = function(self)
          local color = self:mode_color()
          return { fg = color, bold = true }
        end,
        update = {
          "ModeChanged",
          pattern = "*:*",
          callback = vim.schedule_wrap(function()
            vim.cmd.redrawstatus()
          end),
        },
      }

      local FileIcon = {
        init = function(self)
          local file_path = self.file_path or upath.get_current_file_path()
          if upath.is_dir(file_path) then
            self.icon, self.icon_color = icons.status.DirectoryAlt, "blue"
          else
            local extension = vim.fn.fnamemodify(file_path, ":e")
            self.icon, self.icon_color =
            require("nvim-web-devicons").get_icon_color(
              file_path, extension, { default = true })
            self.icon = self.icon and self.icon .. " "
          end
        end,
        provider = function(self)
          return self.icon
        end,
        hl = function(self)
          return { fg = self.icon_color }
        end,
        update = { "BufWinEnter", "BufWritePost" }
      }

      local WorkDir = {
        init = function(self)
          self.icon = (vim.fn.haslocaldir(0) == 1 and icons.status.DirectoryAlt or icons.status.Directory)
        end,
        hl = { fg = "gray", bold = true },
        on_click = {
          callback = function()
            vim.cmd("Neotree toggle")
          end,
          name = "heirline_workdir",
        },
        flexible = 1,
        {
          provider = function(self)
            return self.icon .. self.pwd
          end,
        },
        {
          provider = function(self)
            return self.icon .. vim.fn.pathshorten(self.pwd)
          end,
        },
        { provider = "" },
        update = {
          "BufWinEnter",
          "DirChanged",
          "WinResized",
        },
      }

      local DirPath = {
        hl = { fg = "blue", bold = true },
        flexible = 1,
        {
          provider = function(self)
            return self.reldirpath
          end,
        },
        {
          provider = function(self)
            return vim.fn.pathshorten(self.reldirpath)
          end,
        },
        { provider = "" },
        update = {
          "BufWinEnter",
          "DirChanged",
          "WinResized",
        },
      }

      local function build_path_breadcrumbs(opts)
        opts = opts or {}
        return function(self)
          local children = {}
          local reldirpath = self.reldirpath or nil
          local shorten = opts.max_char and opts.max_char > 0
          local is_root = reldirpath and reldirpath:sub(1, 1) == upath.sep
          if shorten then
            reldirpath = vim.fn.pathshorten(reldirpath, opts.max_char)
          end
          if reldirpath then
            local protocol_start_index = reldirpath:find("://")
            if protocol_start_index ~= nil then
              local protocol = reldirpath:sub(1, protocol_start_index + 2)
              children[#children+1] = {
                provider = protocol,
                hl = { fg = "gray" },
              }
              reldirpath = reldirpath:sub(protocol_start_index + 3)
            end
            local data = upath.split(reldirpath)
            local is_empty = vim.tbl_isempty(data)
            if opts.prefix and not is_empty then
              children[#children+1] = BreadcrumbSep
            end
            local start_index = 0
            if opts.max_depth and opts.max_depth > 0 then
              start_index = #data - opts.max_depth
              if start_index > 0 then
                children[#children+1] =  {
                  provider = icons.status.Ellipsis,
                  hl = { fg = "gray" },
                }
                children[#children+1] = BreadcrumbSep
              end
            elseif is_root then
              table.insert(data, 1, upath.sep)
            end
            for i, d in ipairs(data) do
              if i > start_index then
                local child = {
                  {
                    provider = shorten and d .. icons.status.Ellipsis or d,
                    hl = { fg = "gray" },
                  }
                }
                if #data > 1 and i < #data then child[#child+1] = BreadcrumbSep end
                children[#children+1] = child
              end
            end
            if opts.suffix and (not is_empty or is_root) then
              children[#children+1] = BreadcrumbSep
            end
          end
          self[1] = self:new(children, 1)
        end
      end

      local function build_symbol_breadcrumbs(opts)
        opts = opts or {}
        return function(self)
          local children = {}
          local data = self.data or {}
          local is_empty = vim.tbl_isempty(data)
          if opts.prefix and not is_empty then
            children[#children+1] =  BreadcrumbSep
          end
          local start_index = 0
          if opts.max_depth and opts.max_depth > 0 then
            start_index = #data - opts.max_depth
            if start_index > 0 then
              children[#children+1] = {
                provider = icons.status.Ellipsis,
                hl = { fg = "gray" },
              }
              children[#children+1] =  BreadcrumbSep
            end
          end
          for i, d in ipairs(data) do
            if i > start_index then
              local pos = self.enc(d.lnum, d.col, self.winnr)
              local child = {
                {
                  provider = d.icon,
                  hl = { fg = self.type_hl[d.kind] },
                },
                {
                  provider = function()
                    local symbol = string.gsub(d.name, "%%", "%%%%"):gsub("%s*->%s*", "")
                    if opts.max_char and opts.max_char > 0 then
                      symbol = symbol:sub(-opts.max_char) .. icons.status.Ellipsis
                    end
                    return symbol
                  end
                },
                on_click = {
                  callback = function(_, minwid)
                    local line, col, winnr = self.dec(minwid)
                    vim.api.nvim_win_set_cursor(vim.fn.win_getid(winnr), { line, col })
                  end,
                  minwid = pos,
                  name = "heirline_aerial",
                },
              }
              if #data > 1 and i < #data then child[#child+1] = BreadcrumbSep end
              children[#children+1] = child
            end
            if opts.suffix and not is_empty then
              children[#children+1] = BreadcrumbSep
            end
          end
          self[1] = self:new(children, 1)
        end
      end

      local DirBreadcrumb = {
        flexible = 1,
        {
          init = function(self)
            build_path_breadcrumbs({ suffix = true })(self)
          end
        },
        {
          init = function(self)
            build_path_breadcrumbs({
              suffix = true,
              max_depth = 3,
            })(self)
          end
        },
        {
          init = function(self)
            build_path_breadcrumbs({
              suffix = true,
              max_char = 1,
            })(self)
          end
        },
        {
          init = function(self)
            build_path_breadcrumbs({
              suffix = true,
              max_char = 1,
              max_depth = 3,
            })(self)
          end
        },
        { provider = "" },
        update = {
          "BufWinEnter",
          "DirChanged",
          "WinResized",
        },
      }

      local BaseName = {
        provider = function(self)
          return self.basename
        end,
        hl = function()
          if vim.bo.modified then
            return { fg = "bright_fg", bold = true, italic = true }
          end
          return "bright_fg"
        end,
        update = {
          "BufWinEnter",
          "TextChanged",
          "InsertLeave",
          "BufModifiedSet",
        },
      }

      local FileModified = {
        condition = function()
          return vim.bo.modified
        end,
        provider = " [+]",
        hl = { fg = "green" },
        update = {
          "TextChanged",
          "InsertLeave",
          "BufModifiedSet",
        },
      }

      local FileReadOnly = {
        {
          condition = function()
            return not vim.bo.modifiable or vim.bo.readonly
          end,
          provider = " " .. icons.status.Lock,
          hl = { fg = "orange" },
          update = "BufReadPost",
        },
      }

      local Breadcrumb = {
        init = function(self)
          self.fpath = upath.get_current_file_path()
          _, self.reldirpath, self.basename = upath.get_path_segments(self.fpath)
        end,
        DirBreadcrumb,
        FileIcon,
        BaseName,
        FileModified,
        FileReadOnly,
      }

      local FilePath = {
        condition = function()
          return conditions.buffer_matches({
            buftype = { "" },
            filetype = { "oil" },
          })
        end,
        init = function(self)
          self.fpath = upath.get_current_file_path()
          self.pwd, self.reldirpath, self.basename =
          upath.get_path_segments(self.fpath)
        end,
        WorkDir,
        DirPath,
        BaseName,
      }

      local FileType = {
        provider = function()
          return string.upper(vim.bo.filetype)
        end,
        hl = "Type",
        update = "FileType",
      }

      local FileEncoding = {
        provider = function()
          local enc = (vim.bo.fenc ~= "" and vim.bo.fenc) or vim.o.enc -- :h 'enc'
          return enc ~= "utf-8" and enc:upper()
        end,
      }

      local FileFormat = {
        provider = function()
          local fmt = vim.bo.fileformat
          return fmt ~= "unix" and fmt:upper()
        end,
      }

      local FileSize = {
        provider = function()
          local suffix = { "b", "k", "M", "G", "T", "P", "E" }
          local fsize = vim.fn.getfsize(vim.api.nvim_buf_get_name(0))
          fsize = (fsize < 0 and 0) or fsize
          if fsize <= 0 then
            return "0" .. suffix[1]
          end
          local i = math.floor((math.log(fsize) / math.log(1024)))
          return string.format("%.2g%s", fsize / math.pow(1024, i), suffix[i])
        end,
      }

      local Ruler = {
        -- %l = current line number
        -- %L = number of lines in the buffer
        -- %c = column number
        -- %P = percentage through file of displayed window
        provider = "%7(%3l/%-3L%):%2c %P",
      }

      local ScrollBar = {
        static = {
          sbar = { "🭶", "🭷", "🭸", "🭹", "🭺", "🭻" },
        },
        provider = function(self)
          local curr_line = vim.api.nvim_win_get_cursor(0)[1]
          local lines = vim.api.nvim_buf_line_count(0)
          local i = math.floor(curr_line / lines * (#self.sbar - 1)) + 1
          return string.rep(self.sbar[i], 2)
        end,
        hl = { fg = "blue", bg = "bright_bg" },
      }

      local Aerial = {
        condifion = function()
          return package.loaded["aerial"]
        end,
        static = {
          type_hl = {
            File = dim(utils.get_highlight("Directory").fg, 0.75),
            Module = dim(utils.get_highlight("@include").fg, 0.75),
            Namespace = dim(utils.get_highlight("@namespace").fg, 0.75),
            Package = dim(utils.get_highlight("@include").fg, 0.75),
            Class = dim(utils.get_highlight("@type").fg, 0.75),
            Method = dim(utils.get_highlight("@method").fg, 0.75),
            Property = dim(utils.get_highlight("@property").fg, 0.75),
            Field = dim(utils.get_highlight("@field").fg, 0.75),
            Constructor = dim(utils.get_highlight("@constructor").fg, 0.75),
            Enum = dim(utils.get_highlight("@type").fg, 0.75),
            Interface = dim(utils.get_highlight("@type").fg, 0.75),
            Function = dim(utils.get_highlight("@function").fg, 0.75),
            Variable = dim(utils.get_highlight("@variable").fg, 0.75),
            Constant = dim(utils.get_highlight("@constant").fg, 0.75),
            String = dim(utils.get_highlight("@string").fg, 0.75),
            Number = dim(utils.get_highlight("@number").fg, 0.75),
            Boolean = dim(utils.get_highlight("@boolean").fg, 0.75),
            Array = dim(utils.get_highlight("@field").fg, 0.75),
            Object = dim(utils.get_highlight("@type").fg, 0.75),
            Key = dim(utils.get_highlight("@keyword").fg, 0.75),
            Null = dim(utils.get_highlight("@comment").fg, 0.75),
            EnumMember = dim(utils.get_highlight("@constant").fg, 0.75),
            Struct = dim(utils.get_highlight("@type").fg, 0.75),
            Event = dim(utils.get_highlight("@type").fg, 0.75),
            Operator = dim(utils.get_highlight("@operator").fg, 0.75),
            TypeParameter = dim(utils.get_highlight("@type").fg, 0.75),
          },
          enc = function(line, col, winnr)
            return bit.bor(bit.lshift(line, 16), bit.lshift(col, 6), winnr)
          end,
          dec = function(c)
            local line = bit.rshift(c, 16)
            local col = bit.band(bit.rshift(c, 6), 1023)
            local winnr = bit.band(c, 63)
            return line, col, winnr
          end,
        },
        init = function(self)
          self.data = require("aerial").get_location(true)
        end,
        update = {
          "CursorMoved",
          "WinResized",
        },
        hl = { fg = "gray" },
        flexible = 1,
        {
          init = function(self)
            build_symbol_breadcrumbs({ prefix = true })(self)
          end
        },
        {
          init = function(self)
            build_symbol_breadcrumbs({
              prefix = true,
              max_depth = 3,
            })(self)
          end
        },
        { provider = "" },
      }

      local Diagnostics = {
        condition = conditions.has_diagnostics,
        update = { "DiagnosticChanged", "BufEnter" },
        on_click = {
          callback = function()
            require("trouble").toggle({ mode = "document_diagnostics" })
          end,
          name = "heirline_diagnostics",
        },
        static = {},
        init = function(self)
          self.errors = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
          self.warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
          self.hints = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
          self.info = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
        end,
        {
          provider = function(self)
            return self.errors > 0 and (icons.diagnostics.Error .. " " .. self.errors .. " ")
          end,
          hl = "DiagnosticError",
        },
        {
          provider = function(self)
            return self.warnings > 0 and (icons.diagnostics.Warn .. " " .. self.warnings .. " ")
          end,
          hl = "DiagnosticWarn",
        },
        {
          provider = function(self)
            return self.info > 0 and (icons.diagnostics.Info .. " " .. self.info .. " ")
          end,
          hl = "DiagnosticInfo",
        },
        {
          provider = function(self)
            return self.hints > 0 and (icons.diagnostics.Hint .. " " .. self.hints)
          end,
          hl = "DiagnosticHint",
        },
      }

      local Git = {
        condition = conditions.is_git_repo,
        init = function(self)
          ---@diagnostic disable-next-line: undefined-field
          self.status_dict = vim.b.gitsigns_status_dict
          self.has_changes = self.status_dict.added ~= 0 or self.status_dict.removed ~= 0 or self.status_dict.changed ~= 0
        end,
        on_click = {
          callback = function()
            vim.defer_fn(function()
              vim.cmd("Lazygit %:p:h")
            end, 100)
          end,
          name = "heirline_git",
          update = false,
        },
        hl = { fg = "orange" },
        {
          provider = function(self)
            return self.status_dict.head and
              (" " .. self.status_dict.head .. " ")
          end,
          hl = { bold = true },
        },
        {
          condition = function(self)
            return self.has_changes
          end,
          provider = "(",
        },
        {
          provider = function(self)
            local count = self.status_dict.added or 0
            return count > 0 and ("+" .. count)
          end,
          hl = "diffAdded",
        },
        {
          provider = function(self)
            local count = self.status_dict.removed or 0
            return count > 0 and ("-" .. count)
          end,
          hl = "diffDeleted",
        },
        {
          provider = function(self)
            local count = self.status_dict.changed or 0
            return count > 0 and ("~" .. count)
          end,
          hl = "diffChanged",
        },
        {
          condition = function(self)
            return self.has_changes
          end,
          provider = ")",
        },
      }

      local Lsp = {
        condition = conditions.lsp_attached,
        update = { "LspAttach", "LspDetach", "WinEnter" },
        provider = icons.status.Lsp .. "LSP",
        hl = { fg = "green", bold = true },
        on_click = {
          name = "heirline_LSP",
          callback = function()
            vim.schedule(function()
              vim.cmd("LspInfo")
            end)
          end,
        },
      }

      local Dap = {
        condition = function()
          local session

          if ulazy.has("dap") then
            session = require("dap").session()
          end

          return session ~= nil
        end,
        provider = function()
          return icons.status.Debug .. require("dap").status() .. " "
        end,
        hl = "Debug",
        {
          provider = " ",
          on_click = {
            callback = function()
              require("dap").step_into()
            end,
            name = "heirline_dap_step_into",
          },
        },
        { provider = " " },
        {
          provider = " ",
          on_click = {
            callback = function()
              require("dap").step_out()
            end,
            name = "heirline_dap_step_out",
          },
        },
        { provider = " " },
        {
          provider = " ",
          on_click = {
            callback = function()
              require("dap").step_over()
            end,
            name = "heirline_dap_step_over",
          },
        },
        { provider = " " },
        {
          provider = " ",
          hl = { fg = "green" },
          on_click = {
            callback = function()
              require("dap").run_last()
            end,
            name = "heirline_dap_run_last",
          },
        },
        { provider = " " },
        {
          provider = " ",
          hl = { fg = "red" },
          on_click = {
            callback = function()
              require("dap").terminate()
              require("dapui").close({})
            end,
            name = "heirline_dap_close",
          },
        },
        { provider = " " },
      }

      local HelpFilename = {
        condition = function()
          return vim.bo.filetype == "help"
        end,
        provider = function()
          local filename = vim.api.nvim_buf_get_name(0)
          return vim.fn.fnamemodify(filename, ":t")
        end,
        hl = "Directory",
      }

      local TerminalName = {
        {
          provider = function()
            local tname, _ = vim.api.nvim_buf_get_name(0):gsub(".*:", "")
            return " " .. tname
          end,
          hl = { fg = "blue", bold = true },
        },
        { provider = " - " },
        {
          provider = function()
            ---@diagnostic disable-next-line: undefined-field
            return vim.b.term_title
          end,
        },
      }

      local Spell = {
        condition = function()
          return vim.wo.spell
        end,
        provider = function()
          return "󰓆 " .. vim.o.spelllang .. " "
        end,
        hl = { bold = true, fg = "green" },
      }

      local SearchCount = {
        condition = function()
          ---@diagnostic disable-next-line: undefined-field
          return vim.v.hlsearch ~= 0 and vim.o.cmdheight == 0
        end,
        init = function(self)
          local ok, search = pcall(vim.fn.searchcount)
          ---@diagnostic disable-next-line: undefined-field
          if ok and search.total then
            self.search = search
          end
        end,
        provider = function(self)
          local search = self.search
          return string.format(" %d/%d", search.current, math.min(search.total, search.maxcount))
        end,
        hl = { fg = "purple", bold = true },
      }

      local MacroRec = {
        condition = function()
          return vim.fn.reg_recording() ~= "" and vim.o.cmdheight == 0
        end,
        provider = " ",
        hl = { fg = "orange", bold = true },
        utils.surround({ "[", "]" }, nil, {
          provider = function()
            return vim.fn.reg_recording()
          end,
          hl = { fg = "green", bold = true },
        }),
        update = {
          "RecordingEnter",
          "RecordingLeave",
        },
        { provider = " " },
      }

      local ShowCmd = {
        condition = function()
          return vim.o.cmdheight == 0
        end,
        provider = "%3.5(%S%)",
        hl = function(self)
          return { bold = true, fg = self:mode_color() }
        end,
      }

      ViMode = { MacroRec, ViMode }

      local DefaultStatusline = {
        LeftCap,
        Space,
        ViMode,
        Space,
        Git,
        FilePath,
        FileModified,
        FileReadOnly,
        { provider = "%<" },
        Align,
        -- ShowCmd,
        Space,
        Spell,
        Diagnostics,
        Space,
        Dap,
        Space,
        Lsp,
        Space,
        FileIcon,
        FileType,
        Space,
        { flexible = 10, { FileEncoding, Space, FileFormat, Space, }, { provider = "" } },
        Ruler,
        SearchCount,
        Space,
        ScrollBar,
      }

      local InactiveStatusline = {
        condition = conditions.is_not_active,
        { hl = { fg = "gray", force = true }, FilePath },
        { provider = "%<" },
        Align,
      }

      local SpecialStatusline = {
        condition = function()
          return conditions.buffer_matches({
            buftype = { "nofile", "prompt", "help", "quickfix" },
            filetype = { "^git.*" },
          })
        end,
        FileType,
        { provider = "%q" },
        Space,
        HelpFilename,
        Align,
      }

      local TerminalStatusline = {
        condition = function()
          return conditions.buffer_matches({ buftype = { "terminal" } })
        end,
        hl = { bg = "dark_red" },
        { condition = conditions.is_active, ViMode, Space },
        FileType,
        Space,
        TerminalName,
        Align,
      }

      local StatusLines = {
        hl = function()
          if conditions.is_active() then
            return "StatusLine"
          else
            return "StatusLineNC"
          end
        end,
        static = {
          mode_colors = {
            n = "red",
            i = "green",
            v = "cyan",
            V = "cyan",
            ["\22"] = "cyan", -- this is an actual ^V, type <C-v><C-v> in insert mode
            c = "orange",
            s = "purple",
            S = "purple",
            ["\19"] = "purple", -- this is an actual ^S, type <C-v><C-s> in insert mode
            R = "orange",
            r = "orange",
            ["!"] = "red",
            t = "green",
          },
          mode_color = function(self)
            local mode = conditions.is_active() and vim.fn.mode() or "n"
            return self.mode_colors[mode]
          end,
        },
        fallthrough = false,
        SpecialStatusline,
        TerminalStatusline,
        -- InactiveStatusline,
        DefaultStatusline,
      }

      local WinBar = {
        fallthrough = false,
        {
          condition = function()
            return conditions.buffer_matches({ buftype = { "terminal" } })
          end,
          {
            FileType,
            Space,
            TerminalName,
          },
        },
        {
          fallthrough = false,
          {
            condition = conditions.is_not_active,
            {
              hl = { fg = "gray", force = true },
              Breadcrumb,
            },
          },
          {
            Breadcrumb,
            Aerial,
          },
        },
        Align,
      }

      vim.o.laststatus = 3
      vim.o.showcmdloc = "statusline"

      require("heirline").setup({
        statusline = StatusLines,
        winbar = WinBar,
        opts = {
          disable_winbar_cb = function(args)
            return conditions.buffer_matches({
              buftype = { "nofile", "prompt", "help", "quickfix" },
              filetype = { "^git.*", "Trouble", "neo-tree" },
            }, args.buf)
          end,
          colors = setup_colors,
        }
      })

      vim.api.nvim_create_augroup("Heirline", { clear = true })
      vim.api.nvim_create_autocmd("ColorScheme", {
        callback = function()
          require("heirline.utils").on_colorscheme(setup_colors)
        end,
        group = "Heirline",
      })
    end,
  },
}
