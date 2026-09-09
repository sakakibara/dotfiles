# mox: when os=darwin
function hide -d "Hide dotfiles in Finder"
    defaults write com.apple.finder AppleShowAllFiles -bool false
    and killall Finder
end
