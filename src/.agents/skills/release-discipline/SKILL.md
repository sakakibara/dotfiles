---
name: release-discipline
description: Use when tagging, publishing, or cutting a release - a git tag, a release/publish workflow, or shipping a package or binary. Tags only a commit already landed on the branch it releases from, and verifies its CI passed on that exact commit before publishing.
---

# Release discipline

Tag only a commit already on main or a line or series branch (a version branch has no tags; landing on it is its release); those branches receive commits only through CI (`repo-workflow`). Before tagging, check that the commit is on that branch and that the `ci-ok` of its push run passed (`repo-workflow`'s "Releases"); the release workflow re-verifies both before publishing. Nothing is inferred from a branch's history. In a repo without CI, tag only a commit pushed after the local checks passed.

Tagging and publishing each need the user's yes. When one fix ships on several branches, release each as soon as its own CI passes, in any order; a port's base precedes its port, and a security advisory waits as `published-compatibility`'s "Fixes" says. Mark a release Latest only when it is the highest version published so far (`gh release create --latest=false`, or the release workflow's `make_latest: false`, otherwise).

Release notes, binary assets, and the release workflow's checks follow `repo-workflow`'s "Releases" section.
