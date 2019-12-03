function path_prepend
    for i in $argv
        if test -d $i; and not contains $i $PATH
              set PATH $i $PATH
        end
    end
end
