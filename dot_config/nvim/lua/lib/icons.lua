-- lua/lib/icons.lua
-- Centralized icon table — single source of truth for diagnostic, git, LSP
-- kind, status, DAP, and powerline glyphs. Ported verbatim from the old
-- config's Util.config.icons table (proven to render in the user's nerd
-- font). When adding a new icon, add it here and reference via Lib.icons.
--
-- Glyphs are written as raw UTF-8 byte escapes (\xHH) rather than literal
-- characters so they survive edits through tools that silently strip
-- Private Use Area codepoints.

return {
  dap = {
    Breakpoint          = "\xef\x86\x92 ",                               -- U+F192
    BreakpointCondition = "\xef\x81\x99 ",                               -- U+F059
    BreakpointRejected  = { "\xef\x81\xaa ", "DiagnosticError" },        -- U+F06A
    LogPoint            = ".>",
    Stopped             = { "\xf3\xb0\x81\x95 ", "DiagnosticWarn", "DapStoppedLine" }, -- U+F0055
  },

  diagnostics = {
    Error = "\xee\xaa\x87 ", -- U+EA87
    Hint  = "\xef\x83\xab ", -- U+F0EB
    Info  = "\xee\xa9\xb4 ", -- U+EA74
    Warn  = "\xee\xa9\xac ", -- U+EA6C
  },

  git = {
    added    = "\xef\x83\xbe ", -- U+F0FE
    modified = "\xef\x85\x8b ", -- U+F14B
    removed  = "\xef\x85\x86 ", -- U+F146
  },

  kinds = {
    Array         = "\xee\xaa\x8a ", -- U+EA8A
    Boolean       = "\xee\xaa\x8f ", -- U+EA8F
    Class         = "\xee\xad\x9b ", -- U+EB5B
    Color         = "\xee\xad\x9c ", -- U+EB5C
    Constant      = "\xee\xad\x9d ", -- U+EB5D
    Constructor   = "\xee\xaa\x8c ", -- U+EA8C
    Copilot       = "\xee\x9c\x88 ", -- U+E708
    Enum          = "\xee\xaa\x95 ", -- U+EA95
    EnumMember    = "\xee\xad\x9e ", -- U+EB5E
    Event         = "\xee\xaa\x86 ", -- U+EA86
    Field         = "\xee\xad\x9f ", -- U+EB5F
    File          = "\xee\xa9\xbb ", -- U+EA7B
    Folder        = "\xee\x97\xbf ", -- U+E5FF
    Function      = "\xee\xaa\x8c ", -- U+EA8C
    Interface     = "\xee\xad\xa1 ", -- U+EB61
    Key           = "\xee\xaa\x93 ", -- U+EA93
    Keyword       = "\xee\xad\xa2 ", -- U+EB62
    Method        = "\xee\xaa\x8c ", -- U+EA8C
    Module        = "\xee\xac\xa9 ", -- U+EB29
    Namespace     = "\xee\xaa\x8b ", -- U+EA8B
    Null          = "\xee\x8a\x99 ", -- U+E299
    Number        = "\xee\xaa\x90 ", -- U+EA90
    Object        = "\xee\xaa\x8b ", -- U+EA8B
    Operator      = "\xee\xad\xa4 ", -- U+EB64
    Package       = "\xee\xac\xa9 ", -- U+EB29
    Property      = "\xee\xad\xa5 ", -- U+EB65
    Reference     = "\xee\xac\xb6 ", -- U+EB36
    Snippet       = "\xee\xad\xa6 ", -- U+EB66
    String        = "\xee\xae\x8d ", -- U+EB8D
    Struct        = "\xee\xaa\x91 ", -- U+EA91
    Text          = "\xee\xaa\x93 ", -- U+EA93
    TypeParameter = "\xee\xaa\x92 ", -- U+EA92
    Unit          = "\xee\xaa\x96 ", -- U+EA96
    Value         = "\xee\xaa\x93 ", -- U+EA93
    Variable      = "\xee\xaa\x88 ", -- U+EA88
  },

  status = {
    Lsp           = "\xef\x82\x85 ",                   -- U+F085
    Vim           = "\xee\x9f\x85 ",                   -- U+E7C5
    Mode          = "\xef\x8c\x8c ",                   -- U+F30C
    Lock          = "\xef\x80\xa3 ",                   -- U+F023
    Debug         = "\xee\xab\x98 ",                   -- U+EAD8
    Directory     = "\xee\x97\xbf ",                   -- U+E5FF
    DirectoryAlt  = "\xf3\xb0\x89\x96 ",               -- U+F0256
    Ellipsis      = "\xe2\x80\xa6",                    -- U+2026
    Separator     = { Breadcrumb = "\xee\xaa\xb6" },   -- U+EAB6
    Failure       = "\xef\x91\xa7 ",                   -- U+F467
    Canceled      = "\xf3\xb0\x9c\xba ",               -- U+F073A
    Success       = "\xf3\xb0\x84\xb4 ",               -- U+F0134
    Running       = "\xf3\xb0\x91\xae ",               -- U+F046E
    FoldClose     = "\xef\x91\xa0",                    -- U+F460
    FoldOpen      = "\xef\x91\xbc",                    -- U+F47C
    FoldSeparator = " ",
  },

  powerline = {
    vertical_bar_thin        = "\xe2\x94\x82", -- U+2502
    vertical_bar             = "\xe2\x94\x83", -- U+2503
    block                    = "\xe2\x96\x88", -- U+2588
    left                     = "\xee\x82\xb3", -- U+E0B3
    left_filled              = "\xee\x82\xb2", -- U+E0B2
    right                    = "\xee\x82\xb1", -- U+E0B1
    right_filled             = "\xee\x82\xb0", -- U+E0B0
    slant_left               = "\xee\x82\xba", -- U+E0BA
    slant_left_thin          = "\xee\x82\xbb", -- U+E0BB
    slant_right              = "\xee\x82\xb8", -- U+E0B8
    slant_right_thin         = "\xee\x82\xb9", -- U+E0B9
    slant_left_inverse       = "\xee\x82\xbe", -- U+E0BE
    slant_left_inverse_thin  = "\xee\x82\xbf", -- U+E0BF
    slant_right_inverse      = "\xee\x82\xbc", -- U+E0BC
    slant_right_inverse_thin = "\xee\x82\xbd", -- U+E0BD
    left_rounded             = "\xee\x82\xb6", -- U+E0B6
    left_rounded_thin        = "\xee\x82\xb7", -- U+E0B7
    right_rounded            = "\xee\x82\xb4", -- U+E0B4
    right_rounded_thin       = "\xee\x82\xb5", -- U+E0B5
    trapezoid_left           = "\xee\x83\x92", -- U+E0D2
    trapezoid_right          = "\xee\x83\x94", -- U+E0D4
    line_number              = "\xee\x82\xa1", -- U+E0A1
    column_number            = "\xee\x82\xa3", -- U+E0A3
  },
}
