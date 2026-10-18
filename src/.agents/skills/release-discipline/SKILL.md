---
name: release-discipline
description: Use when tagging, publishing, or cutting a release - a git tag, a release/publish workflow, or shipping a package or binary. Ensures CI is green on every platform for the exact commit before the tag is pushed.
---

# Release discipline

Never tag or publish a release until CI has gone green on all platforms for the exact commit being tagged. Push the release commit, watch CI to completion, and only tag once every job passes.

Why the gate matters:
- A release workflow that builds and uploads binaries typically does NOT run the test suite - it will happily publish a broken build. CI is your only gate.
- Local testing only covers your host platform; the failures that matter most (Windows path separators, Linux directory-iteration order) surface only in CI.
- The trap is parallelism: pushing the tag triggers the release build immediately, so a tag pushed before CI is green races it, and a platform-specific bug ships in an immutable release - forcing an extra patch version. "Push main and tag together" is exactly the mistake.

Sequence it: push the release branch (main, a maintenance line, or a series branch) -> CI green on every platform -> then tag. When one fix ships on several branches, release each as soon as its own CI passes, in any order; a port's base precedes its port, and a security advisory waits as published-compatibility's "Fixes" says. On a version branch, where pushing is the release, open a PR into it and merge only after its CI passes (creating the PR and merging each need the user's yes, with the merge method confirmed). Merges from a security advisory's temporary private fork are the exception: CI runs after the merge, each branch's tag waits only for its own CI, and a branch whose CI fails is fixed forward while the others release (published-compatibility, "Fixes"). Mark a release Latest only when it is the highest version published so far (`gh release create --latest=false`, or the release workflow's `make_latest: false`, otherwise).
