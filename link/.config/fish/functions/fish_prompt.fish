function fish_prompt
  set -l exit_code $status

  set -l normal_color     (set_color normal)
  set -l success_color    (set_color magenta)
  set -l error_color      (set_color red)
  set -l directory_color  (set_color blue)
  set -l repository_color (set_color white)
  set -l ahead_color      (set_color yellow)
  set -l behind_color     (set_color cyan)

  set -l cwd (prompt_pwd)

  set -l dirty "*"
  set -l ahead "⇡"
  set -l behind "⇣"
  set -l prompt "❯"

  if set -l venv_array (string split '/' "$VIRTUAL_ENV")
    echo -ns "(" $venv_array[-2] ")" " "
  end

  # Print current working directory
  echo -ns $directory_color $cwd $normal_color

  # Check if git is installed, and if the directory is git repository
  if begin test (which git ^/dev/null); and test (command git rev-parse --is-inside-work-tree ^/dev/null); end
    # Get git branch name
    set -l git_branch_name (command git symbolic-ref --short HEAD ^/dev/null; or command git show-ref --head -s --abbrev | head -n1 ^/dev/null)

    # Check if git repository is dirty
    set -l is_git_dirty (command git status --porcelain --ignore-submodules ^/dev/null)

    # Set git_arrows if upstream is configured
    test (command git rev-parse --abbrev-ref "@{upstream}" ^/dev/null); and set -l git_arrows

    # Run if git_arrows variable has been declared
    if set -q git_arrows
      # Check for git status
      set -l git_status (string split \t (command git rev-list --count --left-right "HEAD...@{upstream}" ^/dev/null))

      if begin set -q git_status[1]; and test $git_status[1] -gt 0; end
        set git_arrows $git_arrows $ahead_color $ahead
      end
      if begin set -q git_status[2]; and test $git_status[2] -gt 0; end
        set git_arrows $git_arrows $behind_color $behind
      end
    end

    # Print current branch name
    echo -ns $repository_color ":" $git_branch_name

    # Print dirty symbol if the repository is dirty
    if test -n "$is_git_dirty"
      echo -ns $dirty
    end

    echo -ns $normal_color

    # Print arrow to show if the local repository is ahead or behind of remote
    if test -n "$git_arrows"
      echo -ns " " $git_arrows $normal_color
    end
  end

  # Print prompt symbol
  if test $exit_code -eq 0
    echo -ns " " $success_color $prompt $normal_color
  else
    echo -ns " " $error_color $prompt $normal_color
  end

  echo -ns " "
end
