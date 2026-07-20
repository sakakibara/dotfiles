
# mox: for entry in "data/abbreviations.toml" where (not entry.shells or entry.shells has "fish") and (not entry.when or tool = entry.when)
# abbr -a -- <entry.key> <entry.expansion>
# mox: end
