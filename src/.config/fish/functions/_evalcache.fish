function _evalcache
    if test -z "$FISH_EVALCACHE_DIR"
        set -l _cache $XDG_CACHE_HOME
        test -z "$_cache"; and set _cache $HOME/.cache
        set -gx FISH_EVALCACHE_DIR $_cache/fish-eval
    end

    set -f name
    for name in $argv
        if test $name = (string replace -r "[A-Za-z_][A-Za-z0-9_]*=" '' $name)
            break
        end
    end

    # Staleness signal: the file that defines $name -- a function's
    # definition file, else the resolved binary. Symlink resolution puts
    # the versioned Cellar path into the key, so a brew upgrade changes
    # the key; the -nt guard catches in-place binary replacement.
    set -f signal
    if functions -q $name
        set -l src (functions -D $name)
        if test -f "$src"
            set signal $src
        else
            set signal $__fish_config_dir/config.fish
        end
    else
        set -l bin (command -v $name)
        test -n "$bin"; and set signal (path resolve $bin)
    end

    set -f key (string join _ -- $argv $signal | string replace -ra "[^A-Za-z0-9._-]" _)
    if test (string length -- $key) -gt 160
        set key (string sub -l 160 -- $key)_(string length -- $key)
    end
    set -f cacheFile "$FISH_EVALCACHE_DIR/init-$key.fish"

    if test "$FISH_EVALCACHE_DISABLE" = true
        $argv | source
    else if test -s $cacheFile; and not test "$signal" -nt $cacheFile
        source $cacheFile
    else
        if type -q $name
            echo "evalcache: caching output of: $argv" >&2
            mkdir -p "$FISH_EVALCACHE_DIR"
            $argv >$cacheFile
            source $cacheFile
        else
            echo "evalcache ERROR: $name is not installed or in PATH" >&2
        end
    end
end
