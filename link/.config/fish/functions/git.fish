function git
  if test (command -v hub)
    command hub $argv
  else
    command git $argv
  end
end
