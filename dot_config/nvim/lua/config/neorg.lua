require('neorg').setup {
  load = {
    ['core.defaults'] = {},
    ['core.norg.dirman'] = {
      config = {
        workspaces = {
          work = '~/notes/work',
          home = '~/notes/home',
        },
        autochdir = true,
        index = 'index.norg',
      }
    },
    ['core.gtd.base'] = {
      config = { 
        workspace = 'home',
      }
    }
  }
}
