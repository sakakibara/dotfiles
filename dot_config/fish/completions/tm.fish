function __fish_tm_needs_command
    set cmd (commandline -opc)
    if not set -q cmd[2]
        return 0
    end
    return 1
end

function __fish_tmux_sessions --description 'available sessions'
    tmux list-sessions -F "#S	#{session_windows} windows created: #{session_created_string} [#{session_width}x#{session_height}]#{session_attached}" 2>/dev/null | sed 's/0$//;s/1$/ (attached)/'
end

complete -f -c tm
complete -f -c tm -n __fish_tm_needs_command -a '(__fish_tmux_sessions)'
