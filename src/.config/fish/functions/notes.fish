function notes -d "Search the zk notebook and open a note at the matching line"
    set -l dir $ZK_NOTEBOOK_DIR
    test -z "$dir"; and set dir $HOME/Notes

    set -l notes_find_command "command rg --colors 'match:none' --line-number --no-heading --color=always --smart-case '^.' '$dir' 2>/dev/null"

    set -l result
    begin
        eval "$notes_find_command | sed 's:$dir/::' | fzf --delimiter : --nth 3.. --height 40% --multi --reverse" | while read -l r
            set --append result $r
        end
    end

    if [ -z "$result" ]
        commandline -f repaint
        return
    else
        # Remove last token from commandline.
        commandline -t ""
    end

    set -l target (string split --max 2 : $result)
    set -l file $dir/$target[1]
    set -l startup_exec "+$target[2] | normal zMzvzz"
    command $EDITOR $file $startup_exec
end
