# Support windows: the evidence (2026-10-18)

The research behind the "Support windows" rules of the
`published-compatibility` skill. Read this before reopening any of those
rules; revisit only on one of the triggers listed at the end, or when the user
asks.

## Zig (a toolchain without compatibility between releases)

- Minors every 1.5-9 months (0.10 2022-10-31, 0.11 2023-08-04, 0.12
  2024-04-20, 0.13 2024-06-07, 0.14 2025-03-05, 0.15.1 2025-08-19, 0.16
  2026-04-13, 0.17 2026-10-01; mean about 6.7 months). Every minor breaks
  code. Upstream ships patch releases only before the next minor (0.11.1,
  0.12.2, 0.14.2, 0.15.3 were planned and never shipped); release notes say
  there is no bug policy before 1.0. ziglang.org/download/index.json and
  each release's notes.
- Zig recommends tagged releases for projects with dependencies, master only
  for contributors to Zig (ziglang.org/learn/getting-started; andrewrk on
  Ziggit, 2025-01-21).
- Library norm: one Zig minor per branch; older minors on `zig-0.X` branches
  or Zig-named tags. A minority backport to one previous minor for weeks to
  about 6 months (zls, ziglua, zigimg, zig-sqlite, http.zig; http.zig was
  praised on Ziggit for it). In-tree support for two minors is rare and
  short-lived (zig-clap 0.9.1, ziglua's 0.14 CI). Ziggit 2026-08-12: "most
  use source control branches rather than anything inline".
- Adopters ask for version-named branches and tags and a README line naming
  the Zig version (jok #77, known-folders #53); they complain most about
  libraries tracking dev builds (Ziggit 2026-01-30 "constantly changing (or
  being left behind)").
- User lag: libraries port in days to weeks (zls +3 days, zig-clap +7);
  applications lag a minor 3-6+ months (Ghostty +99 days, TigerBeetle +168
  days, skipping 0.15). Distros ship several minors side by side (Homebrew
  `zig@0.N`, nixpkgs `zig_0_N`, Ubuntu `zig0.N`); Ubuntu 26.04 LTS defaults
  to 0.14; Debian ships none.
- 0.17 broke reflection (`fields` split into `field_names`/`field_types`/
  `field_attrs`), the build API (`pathFromRoot`, `Build.args`), `**` on
  arrays, and the formatter (0.17's `zig fmt` rejects files 0.16's accepts),
  so one tree cannot serve both behind one format gate (yaml-zig PR #2,
  verified 2026-10-18).

## Toolchains with compatibility promises

- Go: each release supported until two newer exist (about 1 year); Go 1
  promise; `go` line enforced with automatic toolchain download. Libraries
  mirror it: two latest releases (grpc-go, prometheus client_golang);
  golang.org/x raises its `go` line to N-1 automatically (go.dev/issue/69095).
- Rust: 6-week releases, stability promise since 1.0. Cargo: raising MSRV is
  "generally recommended" as a minor change. tokio and hyper: MSRV at least 6
  months old; clap: last two minor releases. RFC 3537: crates.io traffic was
  47% on the latest Rust, 74% within N-2, 93% within about 6 months. Cargo
  1.84's MSRV-aware resolver moved old-toolchain cost onto resolution.
- Python: CPython 2 years of bug fixes then security to 5 years (PEP 602);
  SPEC 0 drops Python versions 3 years after release, core dependencies 2
  years; `Requires-Python` strands old interpreters on old releases.
- Node: LTS 12 months active + 18 maintenance; library norm "support Node LTS
  lines"; from Node 27 one major a year, all LTS, citing backport burden.
- Neovim: a minor about yearly (0.9 2023-04, 0.10 2024-05, 0.11 2025-03,
  0.12 2026-03); only the latest minor gets patch releases. Plugins cluster
  at latest stable (+ nightly), a few test the previous one or two minors;
  distros lag (Debian 13: 0.10.4, Ubuntu 26.04: 0.11.6). Large rewrites keep
  a frozen branch or old major for the previous minor (nvim-treesitter
  `master`, blink.cmp v1).
- Pattern: library windows track the toolchain's own support window and how
  cheap upgrading is.

## Release lines and backports

- Fixes land on main first and are cherry-picked (Kubernetes, CPython,
  Neovim, Electron, Node, Rust), often by a label-triggered bot.
- Tiered support is the common shape: newest line all bug fixes, older lines
  security / data loss / crashes / regressions only (Django, Kubernetes,
  Electron, Python, Node maintenance, LLVM).
- Single-maintainer norm: only the latest line, plus a strong compatibility
  promise (attrs, curl, Pallets with "security fixes may be backported one
  version upon request"), or a short previous-major tail for critical fixes
  (ESLint 6 months; Fastify 6 months). Backporting is rare and mostly done by
  long-lived active packages; most vulnerabilities affecting several major
  trains are fixed only in the newest (Decan et al., IEEE TSE 2022).
- A maintenance line whose next minor belongs to main can only take patch
  releases (semantic-release's branch model).
- Communication norm: absolute end dates at a stable URL, supported versus
  unsupported clearly marked (endoflife.date/recommendations); GitHub's
  SECURITY.md supported-versions table.

## Adopters

- Developers weight "maturity and stability" highest when choosing a library
  (62% highly influential; release frequency 3%) (Larios Vargas et al., FSE
  2020). Stability 88%, security 86% "extremely or very important" (GitHub
  Open Source Survey 2017).
- Abandoned packages are common; dependents move off faster when end of life
  is declared explicitly (ICSE 2025). 96% of vulnerable downloads had a fixed
  version available (Sonatype 2023): fixes reach users only if they take
  patch releases.
- Kubernetes moved to about 14 months of support because business cycles are
  annual (KEP-1498).

## Revisit triggers

Reopen a rule only when one of these happens or the user asks; otherwise
apply the rules.

1. Zig declares a compatibility promise (expected at 1.0): Zig projects move
   to the compatible-toolchain rules through the port the skill describes;
   no research needed.
2. Zig's package manager starts honoring `minimum_zig_version` when resolving
   (as Cargo's MSRV-aware resolver did): reconsider the length of Zig
   maintenance lines.
3. A project gains co-maintainers or funding: the user may lengthen windows.
4. A new kind of published project fits none of the kinds in the skill (for
   example a project distributed only through an app store).
5. Evidence that the windows fail adopters in practice (repeated requests for
   longer support).
