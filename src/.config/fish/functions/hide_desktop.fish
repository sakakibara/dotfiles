# mox: when os=darwin
function hide_desktop -d "Hide the Finder desktop icons"
    defaults write com.apple.finder CreateDesktop -bool false
    and killall Finder
end
