return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- Completion
  use {
    'hrsh7th/nvim-cmp',
    requires = {
      { 'hrsh7th/cmp-buffer', after = 'nvim-cmp' },
      { 'hrsh7th/cmp-path', after = 'nvim-cmp' },
      { 'hrsh7th/cmp-nvim-lua', after = 'nvim-cmp' },
    },
    config = [[require('config.cmp')]],
    event = 'InsertEnter *',
  }

  -- Commenting
  use {
    'numToStr/Comment.nvim',
    config = [[require('config.comment')]],
  }

  -- Surround
  use {
    'kylechui/nvim-surround',
    config = [[require('config.surround')]],
  }

  -- Auto-pair
  use {
    'windwp/nvim-autopairs',
    config = [[require('config.autopairs')]],
  }

  -- Matching pairs
  use { 'andymass/vim-matchup' }

  -- Telescope
  use {
    'nvim-telescope/telescope.nvim', tag = '0.1.0',
    requires = { {'nvim-lua/plenary.nvim'} },
    config = [[require('config.telescope')]],
  }

  -- Eunuch
  use { 'tpope/vim-eunuch' }

  -- Explorer
  use { 'justinmk/vim-dirvish' }

  -- Statusline
  use {
    'nvim-lualine/lualine.nvim',
    config = [[require('config.lualine')]],
  }

  -- Readline
  use {
    'linty-org/readline.nvim',
    config = [[require('config.readline')]],
  }

  -- Increment / Decrement
  use {
    'monaqa/dial.nvim',
    config = [[require('config.dial')]],
  }

  -- Registers
  use 'tversteeg/registers.nvim'

  -- Treesitter
  use {
    'nvim-treesitter/nvim-treesitter',
    config = [[require('config.treesitter')]],
  }

  -- Neorg
  use {
    'nvim-neorg/neorg',
    requires = { { 'nvim-lua/plenary.nvim' } },
    config = [[require('config.neorg')]],
  }

  -- Colors
  use { 
    'RRethy/vim-hexokinase',
    run = 'make hexokinase'
  }

  -- Colorscheme
  use { 
    'catppuccin/nvim',
    as = 'catppuccin',
    config = [[require('config.catppuccin')]],
  }

  -- Unimpaired
  use { 'tpope/vim-unimpaired' }

  -- Text objects
  use { 
    'kana/vim-textobj-entire',
    requires = { { 'kana/vim-textobj-user' } },
  }
  use { 
    'kana/vim-textobj-line',
    requires = { { 'kana/vim-textobj-user' } },
  }

  -- Git
  use {
    'TimUntersberger/neogit',
    requires = 'nvim-lua/plenary.nvim'
  }
  
  -- Git gutter
  use { 'airblade/vim-gitgutter' }

  -- Linter
  use { 'w0rp/ale' }

  -- Grepper
  use { 'mhinz/vim-grepper' }
end)
