[user]
	email = sho@sakakibara.dev
	signingkey = <data.signing.personal_key>
# mox: when use_1password_ssh_agent=true
[commit]
	gpgsign = true
[tag]
	gpgsign = true
# mox: end
# Read by ~/.config/git/account-token (gh shim and credential helper) to
# pick the GitHub account for `gh` and HTTPS git in personal repos. The
# login is already public in this file's [includeIf] glob, so it is fine
# to commit.
[github]
	account = sakakibara
