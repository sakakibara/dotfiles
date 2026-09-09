# mox: when os=darwin
function show_desktop -d "Show the Finder desktop icons"
    defaults write com.apple.finder CreateDesktop -bool true
    and killall Finder
end
