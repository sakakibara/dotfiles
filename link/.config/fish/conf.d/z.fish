if test -d ~/.z.lua && command -v lua &>/dev/null
  source (lua ~/.z.lua/z.lua --init fish | psub)
end
