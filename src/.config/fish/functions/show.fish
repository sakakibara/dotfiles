# mox: when os=darwin
function show -d "Show dotfiles in Finder"
    defaults write com.apple.finder AppleShowAllFiles -bool true
    and killall Finder
end
