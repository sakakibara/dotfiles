function hide_desktop
  # Treat it as unknown command if the os isn't the specifcied one
  if not test $OSNAME = macos
    echo "fish: Unknown command '$_'"
    return 127
  end

  defaults write com.apple.finder CreateDesktop -bool false
  and killall Finder
end
