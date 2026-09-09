local T = require("tests.helpers")

-- The lang specs that add filetype patterns are loaded with the registry
-- calls stubbed, so only vim.filetype.add runs.
local function load_lang(name)
  local real = { lang = Lib.lang, parsers = Lib.parsers }
  Lib.lang = { setup = function(spec) return spec end }
  Lib.parsers = { add = function() end }
  local ok, err = pcall(dofile, vim.fn.getcwd() .. "/lua/config/plugins/lang/" .. name .. ".lua")
  Lib.lang, Lib.parsers = real.lang, real.parsers
  if not ok then error(err, 0) end
end

T.describe("filetype patterns", function()
  load_lang("ansible")
  load_lang("docker")

  local cases = {
    { "/home/u/proj/playbooks/site.yml", "yaml.ansible" },
    { "/home/u/proj/playbook/site.yaml", "yaml.ansible" },
    { "/home/u/proj/roles/web/tasks/main.yml", "yaml.ansible" },
    { "/home/u/proj/roles/web/handlers/main.yml", "yaml.ansible" },
    { "/home/u/proj/group_vars/all.yml", "yaml.ansible" },
    { "/home/u/proj/host_vars/db1.yml", "yaml.ansible" },
    { "/home/u/proj/ansible/inventory.yml", "yaml.ansible" },
    { "/home/u/deploy-playbook.yml", "yaml.ansible" },
    { "/home/u/playbook-deploy.yml", "yaml.ansible" },
    { "/home/u/proj/docker-compose.yml", "yaml.docker-compose" },
    { "/home/u/proj/docker-compose.prod.yaml", "yaml.docker-compose" },
    { "/home/u/proj/compose.yaml", "yaml.docker-compose" },
    { "/home/u/proj/compose.override.yml", "yaml.docker-compose" },
    { "/home/u/proj/other.yaml", "yaml" },
    { "/home/u/proj/.github/workflows/ci.yml", "yaml" },
    { "/home/u/proj/roles/web/defaults/main.yml", "yaml" },
    { "/home/u/proj/composer.yml", "yaml" },
  }
  for _, c in ipairs(cases) do
    T.it(c[1] .. " is " .. c[2], function()
      T.eq(vim.filetype.match({ filename = c[1] }), c[2])
    end)
  end
end)
