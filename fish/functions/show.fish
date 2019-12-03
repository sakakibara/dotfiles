function show
  # Treat it as unknown command if the os isn't the specifcied one
  if not test $OS = macos
    echo "fish: Unknown command '$_'"
    return 127
  end

  defaults write com.apple.finder AppleShowAllFiles -bool true
  and killall Finder
end
