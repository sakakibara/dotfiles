function __fish_hub_needs_command
  set cmd (commandline -opc)
  if [ (count $cmd) -eq 1 ]
    return 0
  else
    return 1
  end
end

function  __fish_hub_using_command
  set cmd (commandline -opc)
  if [ (count $cmd) -gt 1 ]
    if [ $argv[1] = $cmd[2] ]
      return 0
    end
  end
  return 1
end

complete -f -c hub -n '__fish_hub_needs_command' -a alias -d "Show shell instructions for wrapping git"
complete -f -c hub -n '__fish_hub_needs_command' -a browse -d "Browse the project on GitHub"
complete -f -c hub -n '__fish_hub_needs_command' -a compare -d "Lookup commit in GitHub Status API"
complete -f -c hub -n '__fish_hub_needs_command' -a create -d "Create new repo on GitHub for the current project"
complete -f -c hub -n '__fish_hub_needs_command' -a fork -d "Fork origin repo on GitHub"
complete -f -c hub -n '__fish_hub_needs_command' -a pull-request -d "Open a pull request on GitHub"
complete -f -c hub -n '__fish_hub_needs_command' -a ci-status -d "Display GitHub Status information for a commit"

# alias
complete -f -c hub -n ' __fish_hub_using_command alias' -a 'bash zsh sh ksh csh fish' -d "Output shell script suitable for eval"
# pull-request
complete -f -c hub -n ' __fish_hub_using_command pull-request' -s f -d "Skip the check for unpushed commits"
complete -f -c hub -n ' __fish_hub_using_command pull-request' -s -m -d "Use the first line of <MESSAGE> as pull request title, and the rest as pull request description"
complete -f -c hub -n ' __fish_hub_using_command pull-request' -s F -d "Read the pull request title and description from <FILE>"
complete -f -c hub -n ' __fish_hub_using_command pull-request' -s o -d "Open the new pull request in a web browser"
complete -f -c hub -n ' __fish_hub_using_command pull-request' -l browse -d "Open the new pull request in a web browser"
complete -f -c hub -n ' __fish_hub_using_command pull-request' -s p -d "Push the current branch to <HEAD> before creating the pull request"
complete -f -c hub -n ' __fish_hub_using_command pull-request' -s b -d 'The base branch in "[OWNER:]BRANCH" format'
complete -f -c hub -n ' __fish_hub_using_command pull-request' -s h -d 'The head branch in "[OWNER:]BRANCH" format'
complete -f -c hub -n ' __fish_hub_using_command pull-request' -s a -d 'A comma-separated list of GitHub handles to assign to this pull request'
complete -f -c hub -n ' __fish_hub_using_command pull-request' -s M -d "Add this pull request to a GitHub milestone with id <ID>"
complete -f -c hub -n ' __fish_hub_using_command pull-request' -s l -d "Add a comma-separated list of labels to this pull request"
# fork
complete -f -c hub -n ' __fish_hub_using_command fork' -l no-remote -d "Skip adding a git remote for the fork"
# browse
complete -f -c hub -n ' __fish_hub_using_command browse' -s u -d "Print the URL instead of opening it"
complete -f -c hub -n ' __fish_hub_using_command browse' -s c -d "Put the URL in clipboard instead of opening it"
complete -f -c hub -n ' __fish_hub_using_command browse' -a commits -d 'Commits'
complete -f -c hub -n ' __fish_hub_using_command browse' -a contributors -d 'Contributors'
complete -f -c hub -n ' __fish_hub_using_command browse' -a issues -d 'Issues'
complete -f -c hub -n ' __fish_hub_using_command browse' -a pulls -d 'Pull requests'
complete -f -c hub -n ' __fish_hub_using_command browse' -a wiki -d 'Wiki'
# compare
complete -f -c hub -n ' __fish_hub_using_command compare' -s u -d 'Print the URL instead of opening it'
# create
complete -f -c hub -n ' __fish_hub_using_command create' -s o -d "Open the new repository in a web browser"
complete -f -c hub -n ' __fish_hub_using_command create' -l browse -d "Open the new repository in a web browser"
complete -f -c hub -n ' __fish_hub_using_command create' -s p -d "Create a private repository"
complete -f -c hub -n ' __fish_hub_using_command create' -s c -d "Put the URL of the new repository to clipboard instead of printing it."
complete -f -c hub -n ' __fish_hub_using_command create' -l copy -d "Put the URL of the new repository to clipboard instead of printing it."
# ci-status
complete -f -c hub -n ' __fish_hub_using_command ci-status' -s v -d "Print detailed report of all status checks and their URLs"
