---
name: repo-workflow
description: Land commits only through CI, keeping the user's signed linear history; set up and maintain a repo's CI workflow, rulesets, Dependabot, and security settings; create release branches; land contributor and Dependabot PRs; and read the upkeep report. Use before pushing to a repo of the user's, when creating a repo, a release branch, or changing CI or repo settings, when landing a PR, and at the start of a session that will push, land, or work in a published project.
---

# Repo workflow

Applies to the user's repos whose `ci.yml` has `ci-ok` and triggers on `ci/**` (the rulesets enforce it once applied); from the commit that installs that `ci.yml` on, that commit included, land through `ci/<target>`. In any other repo, follow its existing flow; for a published repo of the user's that lacks the setup, propose bringing it in.

The guarantee: the tip of every branch that releases - main, and `published-compatibility`'s lines, series branches, and version branches - is always a commit whose `ci-ok` passed on every platform the project claims, holding signed commits in a linear history; every release tag points at such a commit. CI tests only the tip of each landing, so a tip whose CI failed never lands, and commits below the tip are untested; tags are verified per commit, never inferred from a branch's history. The rulesets accept any verified signature (GitHub's and Dependabot's included), so that the commits are the user's own rests on the `%G?` check in the steps below.

Every outward step needs the user's yes. The landing yes is the one combined yes allowed; it lapses when the session ends. Landing needs GitHub credentials: inside agent-sandbox, queue it for the host session. Every landing waits for a full CI run, documentation fixes included; that is the price of a branch that releases never being red.

## Landing

The target is a branch that releases: main, or a line, series branch, or version branch. Its staging branch is `ci/<target>` (`ci/main`, `ci/zig-0.16`); staging branches have no consumers and are outside the rulesets.

1. Work on a local topic branch, one per change; local main only mirrors origin main (fast-forward it when origin moves, never commit on it). Run the local checks: formatter and linter, the full test suite, shell tests under the OS's own `/bin/bash`. Then commit (signed) on the topic branch. For committing, these are the verification; CI on other platforms gates landing, not committing.
2. Rebase the agent's own unpushed commits onto the target's tip (`git fetch origin`). Pin committer dates and compare every field as the history-rewrite rule requires. Where the project wants every commit green, land one commit per landing.
3. Ask for the landing yes. Show the target, `git log --format='%h %G? %an %s' origin/<target>..<sha>`, and any change to `.github/` (any workflow's `permissions:` included), to the build's test step, or to a script CI runs. Every commit shown must be `G`; otherwise stop. The yes covers creating or resetting `ci/<target>` to that SHA with `--force-with-lease`, one rerun of a setup failure, fast-forwarding the target to that SHA when `ci-ok` passes (or creating it there, for a new release branch the yes names), and deleting `ci/<target>` afterwards when it is not `ci/main`. Rebasing again or adding a commit needs a new yes.
4. If a `ci.yml` run on `ci/<target>` is queued or in progress (`gh run list --branch ci/<target> --workflow ci.yml --json headSha,status`), another landing is in flight: stop and ask. If `ci/<target>` already holds the SHA, a push starts no run: ask to rerun that run instead. Otherwise `git push --force-with-lease=ci/<target>:<current or empty> origin <sha>:refs/heads/ci/<target>`; a completed run's commits were already reported and may be replaced.
5. Find the run (`gh run list --commit <sha> --workflow ci.yml --json databaseId,status,conclusion`), polling at most 5 minutes; if none appears, report the cause (a skip marker such as `[skip ci]` in the tip's message, a disabled workflow, an outage) and stop. Never land a commit whose message holds a skip marker. Wait with `gh run watch <id> --exit-status` (every job sets `timeout-minutes`, which bounds the wait), then decide by the latest `ci-ok` on that SHA from GitHub Actions: `gh api 'repos/{owner}/{repo}/commits/<sha>/check-runs?check_name=ci-ok&filter=latest' --jq '[.check_runs[] | select(.app.id == 15368)][0].conclusion'`.
6. `success`: `git push origin <sha>:refs/heads/<target>`. A non-fast-forward rejection means the target moved: restart at step 2 with a new yes. Confirm `origin/<target>` is the SHA, then delete `ci/<target>` unless it is `ci/main` (`git push --force-with-lease=ci/<target>:<sha> origin :refs/heads/ci/<target>`). Fast-forward the local target branch to the SHA and delete the topic branch.
7. Anything else: read the jobs. A failed setup step (toolchain or tool install, checkout) or a lost runner gets one `gh run rerun <id> --failed`, then step 5 again. A failed build or test step is real: fix it, amend the fix into the commit that broke it (pinning dates, comparing fields), and restart at step 1 with a new yes. A cancelled job is reported, not retried silently. The target never moves until `ci-ok` passes.

Never use GitHub's merge buttons for the user's own commits: rebase-merge leaves them unverified, squash and merge commits carry GitHub's signature. Never create a commit status or check run by hand.

## Landing several targets at once

One fix for several targets (a backport, or a security fix at disclosure): one landing yes may name every target and SHA. Push them in one `git push` to their staging branches, so their CI runs in parallel, and fast-forward each target as its own `ci-ok` passes; a failure on one does not hold the others.

## Creating a release branch

A new line, series branch, or version branch (published-compatibility): first confirm the branch ruleset targets the new name (`gh api 'repos/{owner}/{repo}/rules/branches/<name>'` lists the `ci-ok` check and the signed-commits, linear-history, non-fast-forward, and deletion rules); if it does not, extend the ruleset's targets with the user's yes and verify again before creating the branch. Ask for the branch and its landing together, naming both. Make its first commit on the base - `ci.yml` to the current standard with the branch's toolchain pinned, and the release workflow with the verification below (not for a version branch, which has no tags) - and land it with the base in place of `origin/<target>`; the push in step 6 creates the branch at a commit whose `ci-ok` passed. The ruleset's required check applies to creation too (`do_not_enforce_on_create: false`), so a branch cannot be created at an untested commit; its signature check does not run at creation, so the `%G?` check in step 3 is the gate there.

## The rulesets

Applied after main exists, no bypass actors; applying, changing, or temporarily disabling one (for example, for a commit the user chose to leave unsigned) is a security setting needing the user's yes each time.

- Branches: targets the default branch and every pattern of branches that release (`zig-*`, `v*.x`, the project's version-branch names), never `ci/**`. Require status check `ci-ok` from GitHub Actions (`integration_id` 15368), with `do_not_enforce_on_create: false` and `strict_required_status_checks_policy: false` set explicitly; require linear history; require signed commits; block force pushes; restrict deletions.
- Tags: targets `v*`. Require status check `ci-ok` from GitHub Actions, with `do_not_enforce_on_create: false`; restrict updates and deletions.
- Workflow token: the repo's default workflow permissions are read-only and workflows cannot approve pull requests (`gh api -X PUT 'repos/{owner}/{repo}/actions/permissions/workflow' -f default_workflow_permissions=read -F can_approve_pull_request_reviews=false`). The app pin on `ci-ok` excludes statuses set by people and other apps; it cannot stop a workflow holding a write token from creating a passing `ci-ok` and a verified commit itself, which the read-only token and the CI workflow's `permissions:` prevent.

Verify read-only: `gh api 'repos/{owner}/{repo}/rulesets/{id}'` shows `enforcement: active`, `bypass_actors: []`, `current_user_can_bypass: never`, the targets above, and the check with its `integration_id` and both flags; `gh api 'repos/{owner}/{repo}/actions/permissions/workflow'` shows `read` and `false`; `gh api 'repos/{owner}/{repo}/rules/branches/ci/main'` returns no status-check rule. Before requiring signatures, confirm the current tip shows `.commit.verification.verified == true`. The first landing tests the branch ruleset. The tag ruleset is tested once, in a scratch repository with the same rulesets and no release workflow, using the tag kind releases use (annotated and signed): a tag on an untested commit is rejected, one on a landed commit is accepted; the scratch repository is deleted afterwards, each step with the user's yes.

## CI workflow

- `ci.yml` runs `on: push: branches: ['ci/**']` and `pull_request`. No `paths:` filter. It sets `permissions: {}` at the top and gives each job only what it reads (`contents: read`; `ci-ok` adds `actions: read`); no workflow that runs on `ci/**` or `pull_request` grants any write permission. Every job sets `timeout-minutes`. Scheduled, fuzz, docs-deploy, and benchmark jobs live in other workflows.
- Every job runs on every push; nothing is skipped, and no job uses `continue-on-error`. Every job is in `ci-ok`'s `needs`, so `ci-ok` starts after all of them.
- The final job, the only required check, checks every other job of the run as of its latest attempt, so a job missing from `needs` or skipped by a partial rerun still fails it:

  ```yaml
  ci-ok:
    if: always()
    needs: [lint, test, cross]
    runs-on: ubuntu-latest
    timeout-minutes: 5
    permissions: {actions: read}
    steps:
      - shell: bash
        env: {GH_TOKEN: "${{ github.token }}"}
        run: |
          set -o pipefail
          gh api "repos/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID/jobs?filter=all&per_page=100" --paginate --jq '.jobs[]' \
            | jq -rs '[.[] | select(.name != "ci-ok")] | group_by(.name) | map(max_by(.run_attempt).conclusion) | unique[]' > c
          [ "$(cat c)" = success ] || { echo "jobs ended:"; cat c; exit 1; }
  ```

  `if: always()` exactly, since with `!cancelled()` a cancelled run skips `ci-ok` and a skipped check counts as success.
- Concurrency: `group: ci-${{ github.ref }}`, `cancel-in-progress: true`.
- Toolchain: every version `published-compatibility` requires CI to test (the range's oldest and newest on a compatible toolchain; a breaking toolchain's one toolchain series, each line its own), pinned and cached, from the official source; a mirror download is verified with minisign against the pinned Zig key, and an unverified download fails the job.
- Matrix: every platform the project claims - Linux x64 and arm64, macOS arm64, Windows x64 - plus cross builds for 32-bit, big-endian, wasm, and Windows arm64 where the language allows; shell code also under macOS's `/bin/bash`. The README claims nothing CI does not test.
- `.gitattributes` `* text=auto eol=lf` (check `git ls-files --eol` first where CRLF fixtures are intentional).
- Actions pinned by full commit SHA with a version comment (`uses: actions/checkout@<sha> # v5`): this forgoes Dependabot's security alerts for Actions, which cover only version-tag refs, in exchange for refs nobody can move; Dependabot version updates keep them current.

## Releases

Tag only a commit already on main or a line or series branch (a version branch has no tags; landing on it is its release). Before tagging, check `git merge-base --is-ancestor <sha> origin/<branch>` and that the latest `ci-ok` from GitHub Actions on the SHA is `success`; the tag ruleset enforces the second. The release workflow repeats both before publishing anything:

```yaml
permissions: {contents: write, checks: read}
steps:
  - uses: actions/checkout@<sha> # v5
    with: {fetch-depth: 0}
  - env: {GH_TOKEN: "${{ github.token }}"}
    run: |
      b=$(git branch -r --contains "$GITHUB_SHA" --format='%(refname:lstrip=3)' | grep -Ex 'main|zig-[0-9.]+|v[0-9.]+x' || true)
      [ -n "$b" ] || { echo "tag is on no branch that releases"; exit 1; }
      c=$(gh api "repos/$GITHUB_REPOSITORY/commits/$GITHUB_SHA/check-runs?check_name=ci-ok&filter=latest" \
            --jq '[.check_runs[] | select(.app.id == 15368)][0].conclusion')
      [ "$c" = success ] || { echo "ci-ok on the tagged commit: $c"; exit 1; }
```

It also fails unless the tag is `v<major>.<minor>.<patch>` and equals the version in the project's manifest (`build.zig.zon`, `Cargo.toml`, ...). Adjust the branch pattern to the project's release branches. This replaces re-running the test matrix on the tag.

## Contributor and Dependabot PRs

CI runs on the PR (its `ci-ok` there does not count for landing). Review with `review-pull-request`.

- **Who changes what.** Ask the contributor, in the review, for changes to design, behavior, or tests; make the rest yourself at landing - a rebase over a moved base, conflicts, toolchain or CI bumps the change needs, formatting, the commit message in the repo's convention. Finish substantive changes yourself only if the contributor asks, gives no answer within 14 days, or the user decides so.
- **Method**, named in the landing yes. Rebase (`git rebase --force-rebase origin/<target>`, re-signing every commit): the contributor stays author with author fields unchanged, the user becomes committer with the landing's own committer dates, which the signature requires; your own changes follow as separate commits. Fold instead when a contributor commit would not pass CI on its own, so no commit on the target is red: one commit carrying the contributor's authorship (`git cherry-pick` or `git commit -C <their commit>`, never `--author`), with `Co-authored-by:` for the user when your changes add code beyond the rebase and message. A multi-commit PR squashes the same way. The user becomes author, with the contributor credited by `Co-authored-by:` with the address their commit used, only when most of the landed code is not the contributor's.
- Each landed commit must show `%G?` = G. Then land as above. The PR shows as Closed, not Merged, since re-signing gives every landed commit a new SHA, so its head never reaches the target: post the closing comment and close the PR, each with the user's yes. The comment links the landed commits and states what landed as written, what changed, and why - giving the technical reason, never the user's private rules, which no contributor has been shown.

Reply to every new issue and PR from a person (not Dependabot or another bot) within 7 days: draft it, post with the user's yes. Dependabot PRs land like contributor PRs; `dependabot.yml` groups each ecosystem's updates into one PR per repo, monthly.

## The upkeep report

Each account that owns the user's published repos has a private `upkeep` repo holding its projects' process state (`published-compatibility`, "State in upkeep"); clone it with `holt repo get <account>/upkeep`, propose creating it if it does not exist, and run `gh` for it from inside one of that account's repos so the `gh` shim picks the account. It runs one scheduled workflow daily, off the hour (private repos are exempt from the 60-day schedule pause; it uses a few of the account's free private minutes, Linux runners only). It lists the account's public, non-fork, non-archived repos and, for each, opens one issue per finding - labeled with the repo's name, the label created if missing - and closes it when resolved:

- main's toolchain behind the latest stable, or a compatible toolchain's newest stable above the range's newest tested: Zig from `https://ziglang.org/download/index.json` (Zig is on Codeberg; GitHub cannot see its releases), Neovim and tree-sitter from their latest non-prerelease release;
- a pin of another of the user's projects behind that project's latest release;
- an entry in any `projects/` file whose `due`, `review`, or `raise again` date is on or before the run date, whatever the repo's state;
- an issue or PR from a person without a reply from the user for 3 days (overdue at 7);
- `ci-ok` not `success` (failing or absent) on the tip of a branch that releases, or a workflow disabled;
- any API error, or a repo the run could not read, as an item of its own.

Its first run proves it can read every listed repo with `GITHUB_TOKEN`; if it cannot, it uses a fine-grained read-only token limited to public repositories and warns 30 days before that token expires. A status issue, labeled `status`, holds the last run's date in its title. The run exits nonzero on any API error, a failed issue write included, so GitHub's failure email reaches the user. Security reports are not polled: the user watches every owned repo with all activity and email, which delivers them.

At the start of a session that will push, land, or work in a published project, pull the upkeep clone (when it is writable) and check the status issue's date: more than 2 days old, or no status issue once the checker workflow exists, means the checker stopped or its runs were dropped - tell the user; before the workflow exists, say once per session that it is not built yet. At the start of work in a published project, read its upkeep file and its open `upkeep` issues (`gh issue list -R <owner>/upkeep --label <repo>`) and treat them as overdue items (`published-compatibility`, "State in upkeep"). Inside agent-sandbox, queue all of these for the host session.

## Bringing a repo in

A new published repo starts with all of it; an existing one is brought in once, one step at a time, each with the user's yes: the read-only workflow token; `ci.yml` with `ci-ok` on main and on every existing line and branch, each landed through its `ci/<target>` (the commit installing it included); `.gitattributes`; the release workflow's verification; README Compatibility; `SECURITY.md` and private vulnerability reporting; Dependabot (version updates for Actions and the repo's Go, Rust, or npm ecosystem; alerts and security updates); watch with all activity; then the rulesets, last, verified as above. Do the first repo end to end before the rest.
