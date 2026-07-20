# mox: for id in "data/git_identities.toml"
# mox: for url in id.match_urls
[includeIf "hasconfig:remote.*.url:<url>"]
	path = ~/.config/git/id-<id.slug>.inc
# mox: end
# mox: end
