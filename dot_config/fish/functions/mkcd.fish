function mkcd --description 'Create directory and cd into it'
    if test (count $argv) -ne 1
        echo "This command requires only one argument" >&2
        return 64
    end

    if mkdir $argv[1]
        cd $argv[1]
    end
end
