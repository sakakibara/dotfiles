function weather -d "Shows the weather forecast"
    if test (count $argv) -eq 0
        curl 'wttr.in?F'
    else
        for a in $argv
            curl "wttr.in/$a?F"
        end
    end
end
