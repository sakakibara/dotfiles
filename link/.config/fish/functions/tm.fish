function tm
  set -l change
  set -l session
  set -l sessions
  set -q TMUX; and set change "switch-client"; or set change "attach-session"
  if test -n "$argv"
    command tmux $change -t "$argv" ^/dev/null; or begin command tmux new-session -d -s $argv; and command tmux $change -t "$argv"; end; return
  end
  set sessions (command tmux list-sessions -F "#{session_name}" 2>/dev/null)
  if test (count $sessions) -eq 0
    echo "No sessions found."
  else
    set session (printf '%s\n' $sessions | fzf --height 40% --reverse --exit-0); and command tmux $change -t "$session"
  end
end
