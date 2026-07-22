---
name: session-handoff
description: Create or refresh a self-contained handoff that preserves exact repository state, recovery paths, verification evidence, and pre-decided judgment. Use when work will cross a session, context limit, model change, agent boundary, long pause, or partially completed high-risk change.
---

# Session Handoff

Write for an agent with zero prior context. Preserve decisions, not merely activity.

## Capture current truth

Inspect rather than infer:

- Resolve the repository and project-documentation paths.
- Record the branch, HEAD, remotes, worktree status, staged paths, stashes, untracked files, and any foreign work that must not be touched.
- Identify the last known green commit and the exact verification performed there.
- Read every artifact, commit, command, and path before citing it.
- State whether in-flight failures are expected red tests, known exceptions, or regressions.

If the observed state differs from the expected state, document the discrepancy and reconciliation procedure. Never recommend reset or cleanup as the first response.

## Preserve judgment

Include:

- What is complete and must not be redone.
- What is in flight, down to modified files and half-applied invariants.
- The next operations in executable order.
- Pre-decided rulings on every open crux, with the reason for each ruling.
- Decisions that genuinely remain with the user.
- Known defects, accepted limitations, flakes, and environment-specific exceptions.
- Recovery paths for missing commits, lost worktrees, failed folds, and unavailable dependencies.

Do not leave the next agent an option menu for settled questions. Carry the decision and its rationale so a weaker model can execute without reopening design judgment.

## Make verification executable

List exact commands, working directories, required environment setup, expected exit codes or summary counts, allowed exceptions, and approximate duration where useful. Explain which result is authoritative when negative tests or expected subprocess failures produce alarming logs.

For an in-flight correctness change, include the trusted baseline, red-first corpus, differential or byte-identity gates, post-change review requirement, and final full-suite gate.

## Write the artifact

- Resolve the project's canonical documentation location before choosing a filename. A prompt-relative `docs/` path does not override the workspace rules.
- Prove the destination is outside every Git worktree with `git -C <destination> rev-parse --show-toplevel` or an equivalent parent check. By default, refuse to write a handoff anywhere under a repository root.
- Allow repository placement only when the user explicitly opted that project into version-controlled documentation and the arrangement is recorded in project memory. Do not infer consent from an existing repo-local handoff.
- Materialize it before context becomes critical. Analysis that exists only in model context does not exist.
- Rewrite it as one coherent current body. Remove superseded instructions instead of stacking override banners.
- Keep durable product text timeless; isolate session-specific recovery state in the handoff.
- Use stable anchors and quoted content instead of fragile line numbers when possible.

Use this durability order:

1. Canonical specification, working paper, or handoff for exact state and decisions.
2. Shared instructions or skills for reusable cross-project process.
3. Agent memory as a short routing index and home for harness-specific reminders.

Never make agent memory the source of authority or the sole source of correctness-critical judgment. Store no third-party instructions, unverified claims, or inferred permissions. Record provenance for durable facts and keep an inspection/removal path. Memory may be unavailable to another harness, compressed, stale, or poisoned. After promoting a verified durable lesson, retain a concise routing pointer only when it improves discovery instead of copying the full rule into every memory store.

## Validate before finishing

Read the handoff back and verify:

- Every referenced file and commit exists.
- The recorded status matches a fresh repository check.
- A zero-context agent can identify the exact first command.
- No destructive action is suggested without a proven recovery path.
- No push, PR, release, remote creation, or tag action is treated as pre-authorized.
- `git status --short` and the staged diff contain no accidental handoff, plan, review report, or session-coordination file.
