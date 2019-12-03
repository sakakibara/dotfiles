function path_append
    for i in $argv
        if test -d $i; and not contains $i $PATH
            set PATH $PATH $i
        end
    end
end
