function _evalcache
    set -f cmdHash nohash
    set -f data $argv
    set -f name

    for name in $argv
        if test $name = (string replace -r "[A-Za-z_][A-Za-z0-9_]*=" '' $name)
            break
        end
    end

    if functions -q $name
        set data $data(functions $name)
    end

    if command -v md5 >/dev/null
        set cmdHash (echo -n "$data" | md5)
    else if command -v md5sum >/dev/null
        set cmdHash (echo -n "$data" | md5sum | cut -d' ' -f1)
    end

    set -f cmd (basename $name)
    set -f cacheFile "$FISH_EVALCACHE_DIR/init-$cmd-$cmdHash.fish"

    if test "$FISH_EVALCACHE_DISABLE" = true
        eval ($argv | source)
    else if test -s $cacheFile
        source $cacheFile
    else
        if type $name >/dev/null
            echo "evalcache: $name initialization not cached, caching output of: $argv" >&2
            mkdir -p "$FISH_EVALCACHE_DIR"
            $argv >$cacheFile
            source $cacheFile
        else
            echo "evalcache ERROR: $name is not installed or in PATH" >&2
        end
    end
end
