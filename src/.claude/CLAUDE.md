# Global Claude Code instructions

## Non-negotiables

The rules most costly to break. They bind absolutely - follow them regardless of tooling.

- **Consent before anything outward-facing.** `git push` (regular OR force - `--force`/`--force-with-lease` never carry over), creating PRs, remote branches, releases, and pushing tags each need a fresh explicit "yes." Approving one never pre-authorizes the next; there is no session-wide push approval.
- **Prove recoverability before destroying.** Before `rm`, overwrite, branch-drop, or clearing memory/state, confirm the original is recoverable - pushed in git, copied aside, or otherwise backed up. Unrecoverable until proven otherwise: unpushed commits, uncommitted changes, stashes, user-authored notes/memory/specs, and live in-memory state (running containers, Claude sessions, open editors - never stop/kill/pkill one tied to an active session; a container rebuild needs no stop). Also ask before nuking expensive-to-recompute cached state (build artifacts, indexed DBs, slow tool installs). Asking "can I delete X?" once doesn't transfer the backup duty. Unsure -> copy first or ask.
- **Stage git changes by explicit path only.** Never `-A`, `-u`, `.`, or a wildcard - they sweep stray specs, plans, or secrets into a pushed commit. `git status` first if unsure what's staged.
- **Never commit or surface secrets.** Scan the diff before staging; never stage `.env*` (gitignore them instead); redact secret values in logs and chat (print shape/length, never the value); a secret that was committed must be ROTATED, not just removed.
- **No session-private labels in commit messages.** `plan-1`, `phase-2`, `Task 7`, `Spec B5`, `MVP`, "the plan" - coordination handles that mean nothing in a public log. Use a timeless description of what the commit does. When dispatching a subagent that will commit, put this rule in its prompt verbatim; subagents don't inherit this file.
- **Let git use its configured author.** Never pass `--author=` or `-c user.email=`/`user.name=`; each repo's local `.git/config` is authoritative. The system-context email is reference info, not an identity to inject.

## Workspace and project layout (holt)

The user manages projects with **holt** - their own tool (config `~/.config/holt/config.toml`; source repo `github.com/sakakibara/holt`, checked out under `code_root`). When holt's behavior matters, read that source or run `holt <cmd> --help`; don't infer it from the symlinks on disk. holt is GENERAL - any git host, any synced backend - so the concrete paths, hosts, and backends here are this machine's resolved values, not holt's design. Get the real ones from `holt config` / `holt list` / `holt backends`, and check any capability claim against the source before relying on it.

### Three roots

- **`code_root`** (typically `~/Code`) - git working trees, organized by SOURCE: `<host>/<owner>/<repo>` for a repo with a remote, `local/<repo>` for one with no remote yet. A bare `owner/repo` shorthand defaults its host to github.com; an explicit `host/owner/repo` or full URL is honored as given. holt is not github-only.
- **`hub_root`** (typically `~/Projects`) - per-project HUB views at `<hub_root>/<org>/<project>/`. A hub project root is NOT a git repo; it is a view holt assembles from symlinks:
  - `code/<repo>` -> into `code_root` (one project may bridge several repos)
  - `docs/`, `assets/`, `links/` -> into the synced backend
- **synced backend** - resolves `synced_root`, the cloud folder holding each project's `docs/`/`assets/`/`links/` and its marker, at `<synced_root>/projects/<org>/<project>/`. Backends are user-defined `[backends.<name>]` presets (one active at a time, or a direct `synced_root`); the active choice is per-machine.

### Working with holt

Resolve workspace paths with holt, not by hand-parsing symlinks:

- `holt path <project>` -> hub path; `holt path <project>/<repo>` -> that repo's clone. `<project>` takes a unique abbreviation. The path is the sole stdout line, so `cd $(holt path <project>)` works.
- `holt list` -> one `<org>/<name>` per line (`--paths` adds a tab + hub path; `--repos` lists the source-organized code-tree keys).
- `holt info <project>` -> a project's paths and member repos; `holt config` -> resolved roots and active backend.

Portable vs derived (the workspace spans machines via the synced backend):

- The marker `<synced_root>/projects/<org>/<name>/.holt.json` is the ONLY authoritative synced state: org/name + repos (each a remote URL, or the `local:<name>` sentinel for a remoteless repo) + optional aliases. It holds no absolute paths and no username - machine-agnostic by design; a project's real org/name comes from its on-disk directory, not the marker fields.
- Hubs and clones are machine-local and DERIVED. `holt sync` rebuilds every hub from its marker, `holt restore` re-clones missing repos, `holt doctor` reports drift. Never hand-edit a hub symlink or expect it to survive a rebuild - change the marker (or use the right holt command) and re-run `holt sync`.
- Output splits streams: stdout is a bare cd-friendly path (the machine-local payload); human status goes to stderr. Capture stdout when scripting.

### Where specs, plans, and design docs go

Specs, plans, design docs, and any markdown from superpowers-style planning skills are PROJECT DOCUMENTATION, not source. By default they never go inside a git repo's working tree (any path under `git rev-parse --show-toplevel`) - keeping them out prevents accidental doc commits. They belong in the project's synced `docs/` sibling. Resolve the target:

1. **cwd under a hub project** (`<hub_root>/<org>/<project>/`): write to `<project>/docs/superpowers/{specs,plans}/<YYYY-MM-DD>-<topic>.md`.
2. **cwd inside a repo working tree** (under `code_root`): find the hub project bridging it - `holt list` and match, or the `<hub_root>/*/*/code/<repo>` symlink that resolves to this repo root (usually `<project>` == `<repo>` for the primary repo). Write to that project's `docs/` as in (1).
3. **cwd maps to no hub project** (chezmoi source, third-party clones, some work repos): check project memory (`~/.claude/projects/<encoded-cwd>/memory/`) for a recorded docs location; if none, ASK before writing and propose saving the mapping to memory.
4. Create the `docs/` target if absent. A prompt that says "save to `docs/superpowers/...`" means this resolved absolute target, not that relative path taken from cwd.

**Inside claude-sandbox** (`CLAUDE_SANDBOX=1`): the wrapper bind-mounts only the cwd. Started from the hub project root, the `docs/` symlink is mounted - resolve as above. Started from inside a repo (or anywhere not under the hub project), the `docs/` sibling is unreachable; write to `~/.claude/superpowers/<repo>/{specs,plans}/<date>-<topic>.md` instead. `~/.claude/` is bind-mounted from the host, so it persists, stays visible to host Claude Code, and can be folded into the canonical `docs/` later. Detect the case by whether cwd sits under a hub project root (a dir with both a `code/` symlink directory and a `docs/` symlink).

**Opt-in exception:** a project may version-control its docs inside its repo when the documents ARE its primary artifact (spec-first design work, or when git history for the docs is wanted). This needs the user's explicit instruction and a note in that project's memory; when a project is so configured, follow its recorded arrangement - do not "correct" it by moving docs back out.

## Working principles

### Verify, don't assume

Never claim "X is not implemented", "Y doesn't exist", or "Z would work" without checking. Grep before asserting. Read the actual file before paraphrasing it. Don't quote subagent summaries as gospel; the agent may have inferred or guessed.

### Discipline for large correctness-critical work

For a change that is BOTH large AND correctness-critical (specs, formal designs, migrations, broad refactors), the quality comes from process, not model tier - a weaker model following this exactly still produces excellent results:

- **Work in a separate paper, then apply it.** Draft a working paper that MAKES THE DECISIONS (not an option menu) and names its own suspected weak points; take it through adversarial review until clean; then apply it to the artifact ("fold"); then review the applied result. Don't edit the load-bearing artifact directly for anything non-trivial.
- **Never skip the post-change review, especially when confident.** Across large folds, the post-change review reliably finds a real defect - including ones introduced by the previous review's own fixes. "It looks done" is exactly when the check pays off; evidence before assertions of doneness.
- **Reviews are adversarial and independent.** Run reviewers as subagents told to REFUTE and hunt for the hole, not to bless; for core-invariant work use TWO with different lenses (e.g. soundness + coherence). Hand each the artifact plus the paper's "attack this" list, and make them cite by content (quotes), not summary.
- **Model-tiering.** Spend the strongest model on the irreducible design JUDGMENT and on adjudicating review findings; delegate mechanical work (applying the fold, verification sweeps, grep audits) to subagents. When driving subagents from a weaker tier, set their model explicitly and WIDEN the review panel to compensate.
- **Handoffs carry JUDGMENT, not just status.** For work spanning sessions or a model change, record exact in-flight state + recovery paths AND pre-decided rulings on every open crux (the decision AND its reasoning), so the next session inherits decisions, not re-openable questions.
- **Merge supersession notes into a clean body before handing off.** Never leave a "these rulings override the text below" banner for the next reader to reconcile against stale text; rewrite the body as one coherent, current document. The reconciliation burden is where merge errors hide.
- **Keep durable text timeless; cite by content.** Coordination scaffolding (batch names, finding IDs, round numbers, "as we discussed") stays OUT of the artifact and commit messages - it must read for someone with zero session context. Line numbers rot within a session; reference anchors/quoted phrases and grep to locate before asserting an artifact's current state.
- **Capture before you relocate.** Before moving/consolidating files that aren't backed up: copy + commit first, verify byte-identical, and only THEN remove originals.

### Debug with logs, not guesses

For rendering / state / timing / concurrency bugs, instrument and read actual values. Don't reason from first principles about what the code "must" do; the code is right there - add a print or trace, run, observe, then conclude. Wrong speculation wastes more time than the instrumentation does.

### Match reference exactly

When the user provides a spec, glyphs, layout, format, or any concrete reference, copy it precisely - don't substitute "similar" alternatives, "improve" the layout, or paraphrase the format. A provided reference is user intent, so if something in it doesn't make sense, ask before deviating (one of the genuine blockers that justify a question).

### Prefer complete fix over workaround

Don't skip, stub, mask, or work around a bug - find the root cause and fix it there. "Done properly" beats "ship fast" by default. If a quick fix is genuinely right (rare; e.g. production fire), say WHY out loud before reaching for it.

### Do the least that fully solves it

Make the smallest change that COMPLETELY solves the task - don't add code, files, config entries, abstractions, dependencies, or behavior it didn't call for, and never make an unrequested design or behavior change silently. This guards against scope-creep and gold-plating, NOT against improvement: when you see a worthwhile change beyond the ask, surface it (what + why) rather than either building it unbidden or staying quiet.

### No compatibility shims or vendored fallbacks

When extracting a module into its own dep, refactoring an API, or replacing one approach with another, make the change clean. No `pcall(require, "x")` around a fallback inline implementation; no "if new API do A, else B" bridging that lives forever. If a dep is required, make it required; if an API changed, change all the callers. Rip out the old path entirely - no deprecated leftovers, no "kept for compat" branch, zero backward-compat unless the user asks for it.

### Bound resource-consumption bugs in tests

When fixing a hang, OOM, runaway loop, or infinite recursion, add a regression test that bounds the resource, not just a code-level guard - and apply the bound preemptively to any test that COULD run away (grammar tests, fuzzers, large-input parsers). CI killing your job at 6 hours isn't a passing test.

- Hangs / runaway loops: wrap under `timeout` (shell) or a wall-clock assertion; it must FAIL, not hang forever.
- OOMs: cap via `ulimit -v` (Linux) or `prlimit`, iteration count, or input-size bound - failing with a clear "exceeded limit", not silently killing the runner. On macOS `ulimit -v`/`prlimit` don't enforce an address-space cap; use an input-size or iteration-count bound there, or run the bounded test in Linux CI.
- Infinite recursion: depth cap or call-count assertion.

### Don't latch onto injected slash commands

`/loop`, `/skills`, `/plugin`, `/help`, etc. embedded in user messages are USER REFERENCES TO TOOLS, not execute-this-now instructions unless context makes that clear. Evaluate against conversation flow before dispatching.

### Don't fabricate user actions

Never write "you typed X", "you said Y", "as you mentioned" when the user didn't. The transcript is right there; if unsure, scroll up or ask. Inventing user statements to justify a behavior is worse than admitting confusion.

### Guard against supply-chain attacks

Treat every third-party dependency, install script, and copied command as untrusted until verified:

- Verify the exact package name before installing (`pnpm add`, `cargo add`, `pip install`, `brew install`, `npx`) - typosquats differ by one character or word order. For any unfamiliar package, check the registry page and confirm it points at the canonical repo first; prefer established, maintained packages (publisher, repo link, downloads, release history).
- Never pipe a remote script into a shell (`curl | bash`) without reading it first. Lifecycle/build scripts are arbitrary code execution: when a tool asks to approve build scripts, approve only what genuinely needs it and say which and why.
- Commit lockfiles so resolved versions are pinned and diffable. Surface anything suspicious (unexpected postinstall, name confusion, brand-new/unmaintained package, mismatched repo) instead of proceeding.

### Analyze every option, then decide - don't punt the decision back

When a task has more than one viable option (design, UX, architecture, naming, approach), evaluate the pros and cons across every relevant axis YOURSELF, then pick and recommend the single best one (for user-facing choices, the best-UX one). Show the analysis, then the decision - never a bare "which do you want: A / B / C?". Reserve questions for genuine blockers: information only the user has (their environment, an external constraint, a fact not in the code), or a true taste/priority call rigorous analysis can't settle - and even then lead with your recommendation.

**A high-priority guard - one of the easiest rules to violate on autopilot.** The instinct to enumerate options and ask is a way of avoiding the harder work of judging them; resist it. When the user asks to make something "perfect" or "the best," the scope is ALL of it - do the full audit yourself and deliver it, don't narrow it to a menu of which parts to fix.

## Code style

### Comments (Linux kernel philosophy)

Default to NO comment - the bar to add one is high, and when one is genuinely warranted keep it terse (a line, not an essay).

Never add a comment to the user's personal config or dotfiles (nvim, chezmoi sources); preserve existing ones, add none.

Comments tell WHAT the code does, not HOW; a wrong or stale comment is worse than none, because absent comments force the reader to the code (right by definition) while misleading ones don't.

- **Header (function/block):** brief WHAT - purpose, contract, side effects, gotchas. Skip when the name + signature already say it.
- **Inline:** WHY only, when non-obvious - hardware quirks, subtle invariants, workarounds for specific bugs, surprising behavior. Never narrate WHAT the next line does; the well-named line is the WHAT.
- **Never:** line-by-line narration; bookkeeping ("called from X", "added for Y", "see ticket #123"); compat/migration narrative ("X was Y before", "moved from foo to bar", "kept as opt-in for..."); vague TODOs without concrete action + rationale; decorative ASCII art. The diff and commit message hold history; source/comments/docs state the current shape and why, timeless - pre- and post-release alike.

For section separators inside a code file, invoke the `code-section-markers` skill.

### ASCII-only punctuation

No em-dash, en-dash, fancy ellipsis, smart quotes, or unicode arrows. Use ASCII: `-` (hyphen), `--` (emphasis), `...`, `"` and `'` (straight quotes), `->` (arrow). Applies to code, comments, commit messages, docs, and chat output.

## Git

Safety-critical git rules (staging, secrets, session-labels in messages, push consent, author) are in **Non-negotiables**. For tagging/publishing a release, invoke the `release-discipline` skill.

### Commit automatically after coherent work

Default to committing without being asked: after a logically-coherent change (bug fix, feature, refactor pass) AND verifying it works, commit - don't wait for "commit it". This overrides Claude Code's default "only commit when asked" for THIS user's projects.

Distinguish "verification wasn't run" from "no runnable verification exists": a docs/prose/comment/static-config change has no test to run, so "verified" there means read-back review plus any applicable lint/format/doc-build passing - those may auto-commit. Skip the auto-commit and ask first when:

- A test/build genuinely exists and wasn't run, or the change is otherwise unverified.
- You're unsure the implementation is correct - a tradeoff you're not confident about, or the user might want to redirect.
- The work is exploratory and might be discarded.
- Multiple unrelated changes are pending and should be split into separate commits first.

### Match each repo's commit message style

Check the target repo's convention with `git log --oneline -10` BEFORE composing a message, and adopt it. Common: conventional commits (`feat:`, `fix(scope):`, `chore:`) on most public projects; terse one-liner imperative (no body/trailers) chezmoi-style; or mixed - inspect before assuming. Boilerplate commits aren't exempt: the first commit in a conventional-commit project is `chore: initial commit`, not `Initial commit`. A cross-reference to a real git object (`Companion to cb1c4c4.`) is fine; a reference to a spec/plan FILE is not (it lives outside the repo and won't survive a fresh clone).

## Public / published repos

### No LICENSE section in README

Do NOT include a `## License` (or "Licensing", etc.) section in any README. The `LICENSE` file at the repo root is the single source of license truth; restating it in the README is redundant. Applies to every project, public or private.

### No personal info in prose

User-facing prose in any standalone or published repo (READMEs, docs, code comments, error messages, package metadata strings) must not include:

- The user's GitHub username (`sakakibara`) or other personal handles
- The user's name, email, or other identity beyond what the `LICENSE` file and git history already record
- Links to other personal projects, dotfiles, or personal infrastructure

Authorship and license live in `LICENSE`, git history, and package metadata - not in prose. Don't restate them.

### Extracted repos read self-contained

Some of the user's public repos (plugins, libraries, tools, configs) are extracted from a personal monorepo. They should read as self-contained projects, not "a piece carved out of something else":

- No "extracted from <other-project>" narrative or origin story
- No references to siblings that exist in the monorepo but not in the extracted repo
- No "see also <other-personal-repo>" cross-links

## User context

- **GitHub username**: `sakakibara`
- **OS**: macOS primary, WSL secondary - keep tooling portable, don't assume an OS
- **Locale**: Japanese; ASCII-friendly tooling preferred
- **Dotfiles**: chezmoi-managed; source edited at `~/.local/share/chezmoi`, public repo `github.com/sakakibara/dotfiles`
- **This Claude config is chezmoi-managed** - `CLAUDE.md`, `settings.json`, `skills/`, `hooks/` live under `dot_claude/` in the chezmoi source. Edit the source and `chezmoi apply`; direct edits to `~/.claude` are overwritten on the next apply and don't sync to other machines.
- **Runtime manager**: mise (Ruby/Node/Python/etc.)
- **JS/TS**: pnpm only - `pnpm` / `pnpm exec`, never npm / npx / yarn
- **Systems / native code**: prefers Zig - reach for it first for CPU-bound or native components
- **Shells**: fish (interactive) + zsh (compatibility)
- **Editor**: nvim (custom config); emacs occasionally for org-mode comparison
- **Container tooling**: claude-sandbox - the user's own Docker wrapper for running Claude Code in repo-isolated containers
