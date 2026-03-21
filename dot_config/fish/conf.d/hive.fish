function h
    if test (count $argv) -eq 0
        echo "Usage: h <query>" >&2
        return 1
    end
    set -l dir (hive path $argv)
    and cd $dir
end

function hi
    set -l dir (hive open)
    and cd $dir
end
