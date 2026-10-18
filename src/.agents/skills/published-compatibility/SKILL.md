---
name: published-compatibility
description: Apply the compatibility rules for published projects - libraries, plugins, grammars, and tool CLIs and file formats. Use before changing anything a published project exports, documents, writes to disk, or prints for scripts; before renaming or removing anything public; when changing a supported toolchain or runtime range; when cutting a release that may break consumers; when bumping a published dependency; and when reviewing a contribution that adds version-conditional code.
---

# Published compatibility

## Public surface

Only surface carries promises. Surface is what the project documents or exports, plus what a consumer must touch to use it:

- Zig library: root-module exports and their types, documented behavior, the values and errors its functions return (including exit codes a framework hands back), `build.zig` module names and options. Adding a member to an exported error set or exhaustive enum, or a field without a default to an exported struct, is breaking: exhaustive `switch` and struct literals stop compiling.
- Lua plugin: documented `require` modules and functions, `setup()` keys and their defaults, commands, `<Plug>` mappings, autocmd events, highlight groups.
- Grammar: node types, field names, aliases, and the tree-sitter ABI the generated parser targets.
- CLI tool: commands, flags, exit codes, machine-readable output, config file format, on-disk state, and the release artifact layout installers read. Stdout of a command meant to be piped or captured (lists, paths) is machine-readable unless documented as human-only.
- A dependency whose types appear in the surface is part of it: bumping it across a breaking change is breaking.

Not surface: undocumented or `_`-prefixed internals, human-facing message wording, help layout, performance, and anything documented as experimental until that mark is removed. Export only what is meant to be supported; an accidental export is surface until a breaking release removes it.

## Classifying a change

- **Breaking:** removes or renames surface; changes documented behavior or a documented default; adds an exhaustive-type member or a defaultless field (above); rejects input that was accepted; changes machine-readable output, exit codes, return values, or files other programs read. This holds even for a fix and even when the new behavior matches the docs. An internal state format may change without breaking only while older data is still read correctly (see Persisted formats).
- **Fix:** corrects behavior with none of those effects (patch). Record visible effects under "Fixed".
- **0.0.z:** a project at 0.0.z makes its next break 0.1.0, after which the rules above apply.
- **Toolchain range:** raising the minimum takes a minor bump (0.(y+1).0 before 1.0, 0.1.0 from 0.0.z, x.(y+1).0 from 1.0), never a patch, and is not a break for semver (Cargo convention); follow the ecosystem's own rule where it differs (npm: major). Widening the range is an addition (see Additions). Mark a raised minimum's commit `!` where the repo uses conventional subjects - users tracking `main` on the dropped version break on their next update.
- **Additions:** new surface, or a widened range, may take a patch bump before 1.0 (0.y.(z+1)) and need a minor bump from 1.0 (x.(y+1).0).
- **Who decides:** propose any breaking change the user did not ask for, and prefer a route that needs no deprecation (an opt-in flag, a new option or command) when one exists - never a second name for the same thing. A fix that must apply by default (a security limit) has no such route; it is breaking.
- **Marking:** in a repo with conventional commit subjects, every breaking commit carries `!` (`feat!:`, `fix(scope)!:`) - users tracking `main` see it in lazy.nvim, which detects only `^%S+!:`; a release also lists breaks in the CHANGELOG's breaking section and its release notes. "Conventional subjects" means 90% or more of recent subjects, the commit hook's threshold. A repo without them marks breaks in the CHANGELOG and notes only (the hook rejects a `type!:` prefix there). A project without releases uses conventional subjects; moving a repo to them is the user's call - until then, name each break to the user when committing it.
- **Security exception:** a security fix that cannot be made without breaking may ship in a patch only with the user's agreement; mark it breaking and say why.
- With a CHANGELOG, every user-visible change gets an entry; a breaking one gets a one-line migration (old -> new). Batch breaking changes into as few releases as practical; a release that already breaks (or adopts a new toolchain) is a good carrier.

## Deprecating a name

Stages: warning, then error, then deletion. Each stage lasts at least one release and at least one month; without releases, at least three months. Unless the user says otherwise, use the earliest dates those minimums allow. Every message names the replacement (or says there is none) and the next stage's date or release. The stages apply even when the user asked for the rename and even when the only known consumer is the user's own repo (that repo still adapts in its pin-bump commit); skipping them needs the user's explicit say-so.

- **Warning stage** (any release; not a break; without releases, a commit not marked `!`): the old name keeps working and warns once per session or run - Lua `vim.deprecate` or `vim.notify` at WARN; a CLI flag, command, or config key on stderr.
- **Error stage** (a breaking release; its commit marked `!` where the repo uses conventional subjects): the old name fails with an error naming the replacement. A deprecated `setup()` or config key at this stage is reported at ERROR and ignored; it never aborts startup.
- **Deletion** (any later release; without releases, a later commit not marked `!` - the error stage already broke): remove the stub.
- Unknown `setup()` or config keys warn and show in `:checkhealth` (or the tool's doctor command); they never abort startup.
- **Zig** has no warnings (`@deprecated` is not in 0.16 or 0.17), so it starts at the error stage in the breaking release that renames: `pub const Old = @compileError("Old was renamed to New in 0.7.0; removed in 0.8.0");`. It errors only where used, but `std.testing.refAllDecls` on its namespace trips it - grep for `refAllDecls` and reference that namespace's other declarations explicitly instead.
- **Grammar** node or field renames cannot warn or error. Ship them as breaking: list each rename in the CHANGELOG if kept, and in the commit body where the repo's history uses bodies (in a subject-only repo with no CHANGELOG, in the README Compatibility section). Update the consumer's queries in its pin-bump commit.

## Toolchain range

Declare it (`minimum_zig_version`, README, `engines`, `rust-version`) and test its oldest and newest in CI; without CI, declare only what was tested locally and propose CI to the user. The README claims no more than was tested. Nightly or dev builds may be tested but are not supported.

The range covers at least the ecosystem's floor:

- Zig before 1.0: the latest stable series (0.N.x). `minimum_zig_version` is the oldest patch tested. A new series, once the user approves it, is adopted with a minor bump.
- Neovim: the current stable minor and the previous one.
- Grammars: the ABI the oldest supported consumer runtime loads (Neovim 0.10 loads up to ABI 14; 0.11 and 0.12 load 15). Generate with an explicit `--abi N` and pin the tree-sitter CLI version that generates `parser.c`.
- Others: the ecosystem's convention (Go: two latest releases; Python: SPEC 0; Node: active and maintenance LTS; Rust: a declared MSRV).

When the floor moves (a new toolchain release), propose the range change to the user for every affected project at once; do not adopt or drop a version yourself. Until the user decides, the declared range stands and is compliant. A range wider than the floor is allowed when the user chose it, CI covers it, and its code stays isolated; a range already declared counts as chosen. "X+" in a README is accurate when CI tests X and the current stable. Never remove in-range compat code as cleanup.

## Multiple versions: one branch inside the range, tags outside

- Inside the range, the default branch serves every version. Conditional code lives in one compat module per artifact (plus the build script where the build API itself differs). Key it on the version when the version decides (`vim.fn.has("nvim-0.12")`), on a feature test otherwise (`@hasDecl(std.Build.Step.Run, "addPassthruArgs")`, `vim.uv or vim.loop`, `has("win32")`), with a one-line marker naming the version it serves and its deletion trigger (the release the user chose, or "when <version> leaves the range" until they choose).
- If a declared version cannot be served from one compat module, the range is too wide: propose narrowing to the user and keep supporting the version until they decide.
- Outside the range, the last release supporting a version stays tagged. Backports only for security or data loss, on a `zig-0.N` / `nvim-0.N` branch cut from that tag, with the user's go-ahead (it is a remote branch).

## Persisted and published formats

- Every persisted format carries a format version from its first release. Existing data without one is version 1; the next format change adds the field.
- Older data, in order of preference: read it correctly (the reader stays while that format is supported); or keep it apart (versioned path or marker), never read it, and tell the user once what to do with it; or refuse with a message naming the upgrade step. Deleting old data is acceptable only when the message says so and nothing is lost silently. Before 1.0, migration code is optional; detection, the message, and the documented step are not. Which option to use, and when to stop reading an old format, is the user's decision; the reader names the release they chose.
- Newer data: refuse ("written by a newer <tool>; upgrade").
- A script that accepts an old release as input (installer, updater) handles that release's published layout for as long as it accepts that version, or refuses the version by name. Stopping acceptance of an old release is the user's decision, as is the installer release that does it; the code names that release.

## Releases

- Never move or delete a published tag: Zig consumers pin a content hash. Fix a bad release with a new patch, say so in the notes, and propose the ecosystem's retraction where one exists (Go `retract`, `cargo yank`; Zig has none, so the release notes carry the warning); publishing, yanking, and retracting each need the user's yes. Tagging follows the `release-discipline` skill.
- 1.0 is the user's decision, independent of the toolchain's own version.

## Consumers

A published project consuming another published project pins a tag or revision it was tested with and bumps the pin in the same commit that adapts to the new version; it never tracks a dependency's moving tip. The user's own consumer repos are callers you control: adapt them, rather than keeping compatibility code for them. A consumer that exposes a dependency's surface to its own users (a plugin whose users write queries against its grammar) inherits that dependency's breaks: mark its pin-bump commit or release as breaking. A personal config follows its lockfile.

## README

A short `## Compatibility` section: the supported range, the versioning rule, and (for projects without releases that use conventional subjects) that breaking commits are marked with `!` in the subject. Add or update it when the range or the versioning rule changes, or when the user asks.

## Outside contributions

Review with the `review-pull-request` skill. Recommend declining version-conditional code for a version outside the declared range, citing this policy, unless the user widens the range; post or close only with the user's yes. Take the substance (a port, a fix) where useful, and ask the user whether to credit the contributor with a `Co-authored-by:` trailer.
