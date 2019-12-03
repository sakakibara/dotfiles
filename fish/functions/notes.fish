function notes -d "List notes in ~/notes directory"
  set dir $HOME/notes

  set notes_find_command "
  command rg --colors 'match:none' --line-number --no-heading --color=always --smart-case '^.' '$dir' 2>/dev/null"

  begin
    eval "$notes_find_command | sed 's:$dir/::' | fzf --delimiter : --nth 3.. --height 40% --multi --reverse" | while read -l r; set --append result $r; end
  end

  if [ -z "$result" ]
    commandline -f repaint
    return
  else
    # Remove last token from commandline.
    commandline -t ""
  end

  set target (string split --max 2 : $result)
  set file $dir/$target[1]
  set startup_exec "+$target[2] | normal zMzvzz"
  command $EDITOR $file $startup_exec
end
