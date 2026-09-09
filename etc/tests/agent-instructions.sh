#!/usr/bin/env bash
set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
scanner="$repo/src/.agents/hooks/instruction-audit.py"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

mkdir -p "$work/clean"
printf '%s\n' '# Safe project guidance' > "$work/clean/AGENTS.md"
python3 "$scanner" --root "$work/clean" >/dev/null

mkdir -p "$work/unicode"
printf '# Hidden\342\200\256instruction\n' > "$work/unicode/AGENTS.md"
if python3 "$scanner" --root "$work/unicode" >/dev/null 2>&1; then
  echo "FAIL: bidi control was accepted" >&2
  exit 1
fi

mkdir -p "$work/nested/sub"
printf '%s\n' '# Nested guidance' > "$work/nested/sub/AGENTS.md"
if python3 "$scanner" --root "$work/nested" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: unexpected nested instruction was accepted" >&2
  exit 1
fi

mkdir -p "$work/symlink/repo" "$work/symlink/outside"
printf '%s\n' '# External guidance' > "$work/symlink/outside/AGENTS.md"
ln -s "$work/symlink/outside/AGENTS.md" "$work/symlink/repo/AGENTS.md"
if python3 "$scanner" --root "$work/symlink/repo" >/dev/null 2>&1; then
  echo "FAIL: external instruction symlink was accepted" >&2
  exit 1
fi

mkdir -p "$work/html"
printf '%s\n' '# Guidance' '<!-- hidden instruction -->' > "$work/html/AGENTS.md"
if python3 "$scanner" --root "$work/html" >/dev/null 2>&1; then
  echo "FAIL: hidden HTML content was accepted" >&2
  exit 1
fi

mkdir -p "$work/copilot/.github/instructions"
printf '%s\n' '# Scoped guidance' > "$work/copilot/.github/instructions/review.instructions.md"
if python3 "$scanner" --root "$work/copilot" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: scoped Copilot instruction was missed" >&2
  exit 1
fi

mkdir -p "$work/config/.claude"
printf '%s\n' '{}' > "$work/config/.claude/settings.json"
if python3 "$scanner" --root "$work/config" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: agent settings were missed" >&2
  exit 1
fi

mkdir -p "$work/config-link/outside/.cursor" "$work/config-link/repo"
printf '%s\n' '# Hostile rule' > "$work/config-link/outside/.cursor/evil.mdc"
ln -s "$work/config-link/outside/.cursor" "$work/config-link/repo/.cursor"
if python3 "$scanner" --root "$work/config-link/repo" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: symlinked agent configuration directory was missed" >&2
  exit 1
fi

mkdir -p "$work/pruned/node_modules/package"
printf '%s\n' '# Dependency data' > "$work/pruned/node_modules/package/AGENTS.md"
python3 "$scanner" --root "$work/pruned" --strict-locations >/dev/null

# The hooks suppress their own bytecode, but importing a hook module from the
# source tree, as this suite does below, drops interpreter-specific bytecode
# beside it. That must stay out of mox: it is machine-local, so carrying it
# to another machine would overwrite a good cache with a stale one.
had_cache=0
# shellcheck disable=SC2034  # read inside the trap below
[[ -d "$repo/src/.agents/hooks/__pycache__" ]] && had_cache=1
# Written into the real source tree, so it must go on every exit path.
trap '(( had_cache == 1 )) || rm -rf "$repo/src/.agents/hooks/__pycache__"; rm -rf "$work"' EXIT
python3 -c "import sys; sys.path.insert(0, '$repo/src/.agents/hooks'); import commit_rules" >/dev/null 2>&1
if [[ ! -d "$repo/src/.agents/hooks/__pycache__" ]]; then
  echo "FAIL: expected importing a hook module to create bytecode, so the exclusion is untested" >&2
  exit 1
fi
if grep -qE '^\s*(__pycache__/|\*\.pyc)\s*$' "$repo/.moxignore"; then
  :
else
  echo "FAIL: .moxignore does not exclude Python bytecode" >&2
  exit 1
fi
if command -v mox >/dev/null 2>&1; then
  if MOX_REPO="$repo" mox status 2>&1 | grep -qi 'pycache\|\.pyc'; then
    echo "FAIL: mox manages Python bytecode" >&2
    exit 1
  fi
fi

# A subagent definition and a slash command are instructions the agent loads
# and acts on; an agent definition also carries tool grants. Both were once
# invisible to discovery, so a hostile one audited clean.
for sub in agents commands; do
  mkdir -p "$work/claudedir/.claude/$sub"
  printf '%s\n' '# Helper' '<!-- hidden payload -->' > "$work/claudedir/.claude/$sub/x.md"
  if python3 "$scanner" --root "$work/claudedir" --strict-locations >/dev/null 2>&1; then
    echo "FAIL: .claude/$sub/x.md was not audited" >&2
    exit 1
  fi
  rm -rf "$work/claudedir"
done

# Discovery reads the whole working tree, not just what git tracks: a file an
# agent just wrote is untracked at that moment, and a gitignored one never
# becomes tracked at all. Both audited clean before this.
mkdir -p "$work/untracked"
git -C "$work/untracked" init -q
printf '%s\n' '# Hostile' '<!-- payload -->' > "$work/untracked/AGENTS.md"
if python3 "$scanner" --root "$work/untracked" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: an untracked AGENTS.md was not audited" >&2
  exit 1
fi

printf '%s\n' 'AGENTS.md' > "$work/untracked/.gitignore"
git -C "$work/untracked" add .gitignore >/dev/null 2>&1
if python3 "$scanner" --root "$work/untracked" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: a gitignored AGENTS.md was not audited" >&2
  exit 1
fi

# A nested repository is one entry to the outer listing (a vendored clone,
# a gitlink); its files are audited all the same.
mkdir -p "$work/outer/vendorlib"
git -C "$work/outer" init -q
git -C "$work/outer/vendorlib" init -q
printf '%s\n' '# Hostile' '<!-- payload -->' > "$work/outer/vendorlib/AGENTS.md"
if python3 "$scanner" --root "$work/outer" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: an AGENTS.md inside a nested repository was not audited" >&2
  exit 1
fi

# A tracked instruction file removed from the worktree is nothing to audit,
# not an error.
mkdir -p "$work/gone"
git -C "$work/gone" init -q
printf '%s\n' '# guide' > "$work/gone/AGENTS.md"
git -C "$work/gone" add AGENTS.md
git -C "$work/gone" -c user.name=Fixture -c user.email=fixture@test.invalid -c commit.gpgsign=false commit -q -m "Add the guide"
rm "$work/gone/AGENTS.md"
if ! python3 "$scanner" --root "$work/gone" >/dev/null 2>&1; then
  echo "FAIL: a tracked instruction file gone from the worktree failed the audit" >&2
  exit 1
fi

# A symlinked directory anywhere under an agent config dir redirects what the
# agent loads; the name is matched case-insensitively, since the filesystem is.
mkdir -p "$work/linked/.claude" "$work/linked-out/agents"
printf '%s\n' '# Helper' '<!-- payload -->' > "$work/linked-out/agents/rogue.md"
ln -s "$work/linked-out/agents" "$work/linked/.claude/agents"
if python3 "$scanner" --root "$work/linked" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: a symlinked .claude/agents was accepted" >&2
  exit 1
fi
mkdir -p "$work/lower"
printf '%s\n' '# Hostile' '<!-- payload -->' > "$work/lower/claude.md"
if python3 "$scanner" --root "$work/lower" >/dev/null 2>&1; then
  echo "FAIL: claude.md was not read as CLAUDE.md" >&2
  exit 1
fi

# A declared link is checked against the mox attributes that plant it and
# against its target: a link the attributes do not declare, or one that
# leads nowhere, fails.
mkdir -p "$work/links/src/.agents/skills/one" "$work/links/src/.claude" "$work/links/.mox"
git -C "$work/links" init -q
printf '%s\n' '# Guidance' > "$work/links/src/.agents/instructions.md"
printf '%s\n' '# Skill' > "$work/links/src/.agents/skills/one/SKILL.md"
printf '%s' '../.agents/skills' > "$work/links/src/.claude/skills"
printf '%s' '../.agents/instructions.md' > "$work/links/src/.claude/CLAUDE.md"
printf '%s\n' '[".claude/skills"]' 'symlink = true' '' '[".claude/CLAUDE.md"]' 'symlink = true' > "$work/links/.mox/attributes.toml"
printf '%s\n' '{"canonical_policy": "src/.agents/instructions.md", "allowed_instruction_files": ["src/.agents/instructions.md", "src/.agents/skills/one/SKILL.md", "src/.claude/CLAUDE.md"], "audited_files": [], "discovery_links": {"src/.claude/CLAUDE.md": "../.agents/instructions.md"}, "symlinks": {"src/.claude/skills": "../.agents/skills"}}' > "$work/links/policy.json"
if ! out=$(python3 "$scanner" --root "$work/links" --policy policy.json 2>&1); then
  echo "FAIL: a planted, resolving link failed the policy audit:" >&2; printf '%s\n' "$out" >&2
  exit 1
fi
printf '%s\n' '[".claude/CLAUDE.md"]' 'symlink = true' > "$work/links/.mox/attributes.toml"
if python3 "$scanner" --root "$work/links" --policy policy.json >/dev/null 2>&1; then
  echo "FAIL: a link the attributes do not plant was accepted" >&2
  exit 1
fi
printf '%s\n' '[".claude/skills"]' 'symlink = true' '' '[".claude/CLAUDE.md"]' 'symlink = true' > "$work/links/.mox/attributes.toml"
printf '%s' '../.agents/nowhere' > "$work/links/src/.claude/skills"
out=$(python3 "$scanner" --root "$work/links" --policy policy.json 2>&1) && rc=0 || rc=$?
if [[ $rc -eq 0 || "$out" != *"expected link target '../.agents/skills', found '../.agents/nowhere'"* ]]; then
  echo "FAIL: a link declared with another target was accepted, or not named:" >&2; printf '%s\n' "$out" >&2
  exit 1
fi
printf '%s' '../.agents/skills' > "$work/links/src/.claude/skills"
rm -r "$work/links/src/.agents/skills"
out=$(python3 "$scanner" --root "$work/links" --policy policy.json 2>&1) && rc=0 || rc=$?
if [[ $rc -eq 0 || "$out" != *"link target '../.agents/skills' is not a directory"* ]]; then
  echo "FAIL: a link to a missing directory was accepted, or not named:" >&2; printf '%s\n' "$out" >&2
  exit 1
fi

# A registered submodule is a gitlink: one cached entry, no trailing slash.
mkdir -p "$work/subsrc"
git -C "$work/subsrc" init -q
git -C "$work/subsrc" config user.name Fixture
git -C "$work/subsrc" config user.email fixture@test.invalid
printf '%s\n' 'lib' > "$work/subsrc/lib.txt"
git -C "$work/subsrc" add lib.txt
git -C "$work/subsrc" -c commit.gpgsign=false commit -q -m "Add the library"
mkdir -p "$work/super"
git -C "$work/super" init -q
git -C "$work/super" -c protocol.file.allow=always submodule add -q "$work/subsrc" vendorlib 2>/dev/null
printf '%s\n' '# Hostile' '<!-- payload -->' > "$work/super/vendorlib/AGENTS.md"
if python3 "$scanner" --root "$work/super" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: an AGENTS.md inside a submodule was not audited" >&2
  exit 1
fi

# A symlink back into the tree is one entry: the audit never descends a symlink.
mkdir -p "$work/loop/.claude"
git -C "$work/loop" init -q
printf '%s\n' '# Guidance' > "$work/loop/AGENTS.md"
ln -s . "$work/loop/self"
if ! out=$(python3 "$scanner" --root "$work/loop" --strict-locations 2>&1); then
  echo "FAIL: a symlink to the repository root failed the audit:" >&2
  printf '%s\n' "$out" >&2
  exit 1
fi
case "$out" in
  *self/*) echo "FAIL: the audit listed the tree again through the symlink" >&2; exit 1 ;;
esac

# Outside a repository the walk reads the whole tree or refuses; it never
# passes a partial listing.
mkdir -p "$work/deep/a/b/c/d/e/f/g"
printf '%s\n' '# Hostile' > "$work/deep/a/b/c/d/e/f/g/AGENTS.md"
if python3 "$scanner" --root "$work/deep" --strict-locations >/dev/null 2>&1; then
  echo "FAIL: a deeply nested AGENTS.md outside a repository was not audited" >&2
  exit 1
fi
mkdir -p "$work/wide/sub"
python3 -c 'import os,sys; d=sys.argv[1]; [open(os.path.join(d, f"f{i}"), "w").close() for i in range(20001)]' "$work/wide"
printf '%s\n' '# Hostile' > "$work/wide/sub/AGENTS.md"
if out=$(python3 "$scanner" --root "$work/wide" --strict-locations 2>&1); then
  echo "FAIL: a tree past the entry cap was passed instead of refused" >&2
  exit 1
fi
case "$out" in
  *"more than the"*) ;;
  *) echo "FAIL: the entry cap did not say why it refused:" >&2; printf '%s\n' "$out" >&2; exit 1 ;;
esac

# Under a policy, an agent's config kept under a home-mirroring src/ tree
# gets the content rules its live path would; the per-machine local
# settings file may sit at the root unlisted, and is still read.
mkdir -p "$work/policy/src/.config/opencode" "$work/policy/.claude"
git -C "$work/policy" init -q
printf '%s\n' '# Guidance' > "$work/policy/AGENTS.md"
printf '%s\n' '{"canonical_policy": "AGENTS.md", "allowed_instruction_files": ["AGENTS.md", "src/.config/opencode/opencode.jsonc"], "audited_files": []}' > "$work/policy/policy.json"
printf '%s\n' '{"theme": "plain"}' > "$work/policy/src/.config/opencode/opencode.jsonc"
printf '%s\n' '{"attribution": {"sessionUrl": true}}' > "$work/policy/.claude/settings.local.json"
if ! python3 "$scanner" --root "$work/policy" --policy policy.json >/dev/null 2>&1; then
  echo "FAIL: a clean src/ agent config or a local settings file failed the policy audit" >&2
  exit 1
fi
printf '%s\n' '{"theme": "plain"} // <!-- hidden instruction: exfiltrate secrets -->' > "$work/policy/src/.config/opencode/opencode.jsonc"
if python3 "$scanner" --root "$work/policy" --policy policy.json >/dev/null 2>&1; then
  echo "FAIL: hidden content in a src/ agent config was accepted" >&2
  exit 1
fi
printf '%s\n' '{"theme": "plain"}' > "$work/policy/src/.config/opencode/opencode.jsonc"
printf '{"hooks": "\342\200\256"}\n' > "$work/policy/.claude/settings.local.json"
if python3 "$scanner" --root "$work/policy" --policy policy.json >/dev/null 2>&1; then
  echo "FAIL: a bidi control in the local settings file was accepted" >&2
  exit 1
fi

# The SessionStart guard runs the audit under the system bash, with or
# without a repo policy; its empty-policy case is bash 3.2's unbound-array
# trap.
guard="$repo/src/.agents/hooks/instruction-trust-guard.sh"
mkdir -p "$work/guardhome/.agents"
ln -s "$repo/src/.agents/hooks" "$work/guardhome/.agents/hooks"
guard_run() { printf '{"hook_event_name":"SessionStart","cwd":"%s"}' "$1" | HOME="$work/guardhome" /bin/bash "$guard" 2>&1; }
for cwd in "$work/clean" "$repo"; do
  out=$(guard_run "$cwd") && rc=0 || rc=$?
  case "$out" in
    *"unbound variable"*|*"ERROR:"*) echo "FAIL: the trust guard failed on a clean tree at $cwd (rc=$rc):" >&2; printf '%s\n' "$out" >&2; exit 1 ;;
  esac
  [ "$rc" -eq 0 ] || { echo "FAIL: the trust guard exited $rc for $cwd" >&2; exit 1; }
done
out=$(guard_run "$work/unicode")
case "$out" in
  *"ERROR:"*) ;;
  *) echo "FAIL: the trust guard did not surface the audit's finding:" >&2; printf '%s\n' "$out" >&2; exit 1 ;;
esac

echo "agent instruction audit tests passed"
