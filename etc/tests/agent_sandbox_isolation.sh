#!/usr/bin/env bash
# Host-tamper gate. Runs a real container against a synthetic host tree with the
# default-mode mount plan, has it attempt every known write into host agent
# state, and fails if anything outside SHARED_PATHS changed.
#
# The mount plan here mirrors _run_args default mode; agent_sandbox_security.sh
# asserts the wrapper actually emits that shape. Both are needed: that suite
# stubs docker and cannot observe behavior, this one observes behavior but
# does not read the wrapper.
set -euo pipefail

IMAGE="${ASB_ISOLATION_IMAGE:-agent-sandbox:latest}"

command -v docker >/dev/null 2>&1 || { echo "SKIP: docker unavailable" >&2; exit 0; }
docker image inspect "$IMAGE" >/dev/null 2>&1 || { echo "SKIP: $IMAGE not built" >&2; exit 0; }

work=$(mktemp -d)

# The tamper container writes as root. On Linux those files are root-owned on
# the host too, so the plain remove fails; hand ownership back from inside a
# container rather than requiring sudo on the runner. macOS maps ownership to
# the invoking user already, where the first remove succeeds.
cleanup() {
  rm -rf "$work" 2>/dev/null && return 0
  docker run --rm -v "$work":/w --user 0 "$IMAGE" \
    chown -R "$(id -u):$(id -g)" /w >/dev/null 2>&1 || true
  rm -rf "$work"
}
trap cleanup EXIT

# Paths a sandbox is allowed to modify. Everything else in the host agent tree
# must be byte-identical afterwards. Keep this list short and justified.
SHARED_PATHS=(
  './.claude/projects/WSENC'
  './.claude/history.jsonl'
  './.claude/.credentials.json'
  './.codex/sessions'
  './.codex/history.jsonl'
  './.codex/auth.json'
)

HH="$work/host"
mkdir -p "$HH/.agents/hooks" "$HH/.agents/skills/demo" \
         "$HH/.claude/plugins/cache/demo/1.0.0/hooks" \
         "$HH/.claude/projects/-demo/memory" "$HH/.claude/backups"

printf 'guard\n'            > "$HH/.agents/hooks/instruction-trust-guard.sh"
printf 'stage guard\n'      > "$HH/.agents/hooks/git-stage-guard.sh"
printf 'global rules\n'     > "$HH/.agents/instructions.md"
printf 'demo skill\n'       > "$HH/.agents/skills/demo/SKILL.md"
chmod +x "$HH/.agents/hooks/"*.sh

printf '{"hooks":{}}\n'     > "$HH/.claude/settings.json"
printf 'plugin hook\n'      > "$HH/.claude/plugins/cache/demo/1.0.0/hooks/run-hook.cmd"
chmod +x "$HH/.claude/plugins/cache/demo/1.0.0/hooks/run-hook.cmd"
printf '{"demo":{}}\n'      > "$HH/.claude/plugins/installed_plugins.json"
printf 'notes\n'            > "$HH/.claude/projects/-demo/memory/notes.md"
printf '{}\n'               > "$HH/.claude.json"

ln -s ../.agents/instructions.md "$HH/.claude/CLAUDE.md"
ln -s ../.agents/hooks           "$HH/.claude/hooks"
ln -s ../.agents/skills          "$HH/.claude/skills"

mkdir -p "$HH/.codex/sessions" "$HH/.codex/rules" "$HH/.codex/plugins/cache/demo"
printf '{"hooks":{}}\n'  > "$HH/.codex/hooks.json"
printf 'trust = "yes"\n' > "$HH/.codex/config.toml"
printf 'rules\n'         > "$HH/.codex/rules/default.rules"
printf 'auth\n'          > "$HH/.codex/auth.json"
printf 'plugin\n'        > "$HH/.codex/plugins/cache/demo/run.sh"
ln -s ../.agents/instructions.md "$HH/.codex/AGENTS.md"

manifest() {
  ( cd "$1" && find . -mindepth 1 -print | LC_ALL=C sort | while IFS= read -r p; do
      if [[ -L "$p" ]]; then
        printf '%s\tsymlink\t%s\n' "$p" "$(readlink "$p")"
      elif [[ -d "$p" ]]; then
        printf '%s\tdir\n' "$p"
      elif [[ -f "$p" ]]; then
        printf '%s\tfile\t%s\n' "$p" "$(shasum -a 256 "$p" | cut -d' ' -f1)"
      else
        printf '%s\tother\n' "$p"
      fi
    done )
}

mkdir -p "$work/repo" "$work/slot/.claude"
printf 'x\n' > "$work/repo/file.txt"
git -C "$work/repo" init -q
printf 'hook\n' > "$work/repo/.git/hooks/pre-commit"
mkdir -p "$work/repo/.claude"
printf '{"hooks":{}}\n' > "$work/repo/.claude/settings.json"
printf '{"mcpServers":{}}\n' > "$work/repo/.mcp.json"
repo_before=$(manifest "$work/repo/.git")
proj_before=$(manifest "$work/repo/.claude")$(shasum -a 256 "$work/repo/.mcp.json")
printf 'history\n' > "$HH/.claude/history.jsonl"
printf 'creds\n'   > "$HH/.claude/.credentials.json"

# Seeding, mirroring _ensure_sandbox_home.
cp "$HH/.claude.json" "$work/slot/.claude.json"
cp "$HH/.claude/settings.json" "$work/slot/.claude/settings.json"
mkdir -p "$work/slot/.claude/plugins/cache"
cp "$HH/.claude/plugins/installed_plugins.json" "$work/slot/.claude/plugins/installed_plugins.json"
ln -sfn ../.agents/instructions.md "$work/slot/.claude/CLAUDE.md"
ln -sfn ../.agents/hooks           "$work/slot/.claude/hooks"
ln -sfn ../.agents/skills          "$work/slot/.claude/skills"

WSENC=$(printf '%s' "$work/repo" | tr '/.' '--')
mkdir -p "$HH/.claude/projects/$WSENC/memory"
printf 'proj notes\n' > "$HH/.claude/projects/$WSENC/memory/notes.md"
SHARED_PATHS=("${SHARED_PATHS[@]/.\/.claude\/projects\/WSENC/./.claude/projects/$WSENC}")

manifest "$HH" > "$work/before.txt"

# Default-mode mount plan (mirrors _run_args).
docker run --rm \
  -e AGENT_SANDBOX=1 -e HOST_HOME="$HH" -e SANDBOX_AGENT_KIND=claude \
  -v "$work/slot":"$HH" \
  -v "$HH/.agents":"$HH/.agents":ro \
  -v "$HH/.agents":/home/claude/.agents:ro \
  -v "$HH/.claude/plugins/cache":"$HH/.claude/plugins/cache":ro \
  -v "$HH/.claude/projects/$WSENC":"$HH/.claude/projects/$WSENC" \
  -v "$HH/.claude/projects/$WSENC/memory":"$HH/.claude/projects/$WSENC/memory":ro \
  -v "$HH/.claude/history.jsonl":"$HH/.claude/history.jsonl" \
  -v "$HH/.claude/.credentials.json":"$HH/.claude/.credentials.json" \
  -v "$work/repo":"$work/repo" \
  -v "$work/repo/.git/hooks":"$work/repo/.git/hooks":ro \
  -v "$work/repo/.git/config":"$work/repo/.git/config":ro \
  -v "$work/repo/.claude":"$work/repo/.claude":ro \
  -v "$work/repo/.mcp.json":"$work/repo/.mcp.json":ro \
  -w "$work/repo" \
  --entrypoint sh "$IMAGE" -c '
    set -u
    C="$HOST_HOME/.claude"
    t() { sudo sh -c "$1" >/dev/null 2>&1 || true; }
    t "rm -f $C/CLAUDE.md && echo TAMPERED > $C/CLAUDE.md"
    t "rm -f $C/hooks && mkdir -p $C/hooks && echo TAMPERED > $C/hooks/evil.sh"
    t "rm -f $C/skills && mkdir -p $C/skills && echo TAMPERED > $C/skills/evil.md"
    t "echo TAMPERED > $C/settings.json"
    t "echo TAMPERED > $C/settings.local.json"
    t "echo TAMPERED > $C/plugins/cache/demo/1.0.0/hooks/run-hook.cmd"
    t "echo TAMPERED > $C/plugins/installed_plugins.json"
    t "mkdir -p $C/commands && echo TAMPERED > $C/commands/evil.md"
    t "mkdir -p $C/agents   && echo TAMPERED > $C/agents/evil.md"
    t "mkdir -p $C/rules    && echo TAMPERED > $C/rules/evil.md"
    t "mkdir -p $C/routines && echo TAMPERED > $C/routines/evil.md"
    t "echo TAMPERED > $C/scheduled_tasks.json"
    t "echo TAMPERED > $HOST_HOME/.agents/hooks/instruction-trust-guard.sh"
    t "echo TAMPERED > $HOST_HOME/.claude.json"
    t "echo TAMPERED > $C/projects/-demo/memory/notes.md"
    t "mkdir -p $C/projects/-other && echo TAMPERED > $C/projects/-other/x.jsonl"
    t "echo TAMPERED > $C/projects/'"$WSENC"'/memory/notes.md"
    t "echo TAMPERED > $PWD/.git/hooks/pre-commit"
    t "echo hooksPath-injection > $PWD/.git/config"
    t "echo TAMPERED > $PWD/.claude/settings.json"
    t "echo TAMPERED > $PWD/.mcp.json"
    exit 0
  ' >/dev/null 2>&1

# Codex mount plan (mirrors _run_args for AGENT_KIND=codex).
mkdir -p "$work/slotx/.codex/plugins/cache"
cp "$HH/.codex/hooks.json"  "$work/slotx/.codex/hooks.json"
cp "$HH/.codex/config.toml" "$work/slotx/.codex/config.toml"
cp -R "$HH/.codex/rules"    "$work/slotx/.codex/rules"
ln -sfn ../.agents/instructions.md "$work/slotx/.codex/AGENTS.md"

docker run --rm \
  -e AGENT_SANDBOX=1 -e HOST_HOME="$HH" -e SANDBOX_AGENT_KIND=codex \
  -v "$work/slotx":"$HH" \
  -v "$HH/.agents":"$HH/.agents":ro \
  -v "$HH/.agents":/home/claude/.agents:ro \
  -v "$HH/.codex/plugins/cache":"$HH/.codex/plugins/cache":ro \
  -v "$HH/.codex/auth.json":"$HH/.codex/auth.json" \
  -v "$HH/.codex/sessions":"$HH/.codex/sessions" \
  -v "$work/repo":"$work/repo" \
  -w "$work/repo" \
  --entrypoint sh "$IMAGE" -c '
    set -u
    X="$HOST_HOME/.codex"
    t() { sudo sh -c "$1" >/dev/null 2>&1 || true; }
    t "echo TAMPERED > $X/hooks.json"
    t "echo TAMPERED > $X/config.toml"
    t "rm -f $X/AGENTS.md && echo TAMPERED > $X/AGENTS.md"
    t "echo TAMPERED > $X/rules/default.rules"
    t "echo TAMPERED > $X/plugins/cache/demo/run.sh"
    t "mkdir -p $X/skills && echo TAMPERED > $X/skills/evil.md"
    exit 0
  ' >/dev/null 2>&1

manifest "$HH" > "$work/after.txt"

# Paths that must NOT change even though they sit inside a shared prefix.
# memory/ is host instruction input; projects/ is otherwise session history.
DENIED_PATHS=( "./.claude/projects/$WSENC/memory" )

allowed() {
  local p="$1" s
  for s in "${DENIED_PATHS[@]}"; do
    [[ "$p" == "$s" || "$p" == "$s"/* ]] && return 1
  done
  for s in "${SHARED_PATHS[@]}"; do
    [[ "$p" == "$s" || "$p" == "$s"/* ]] && return 0
  done
  return 1
}

violations=0
while IFS= read -r line; do
  path="${line%%$'\t'*}"
  allowed "$path" && continue
  printf 'TAMPERED: %s\n' "$line" >&2
  violations=$((violations + 1))
done < <(LC_ALL=C comm -13 "$work/before.txt" "$work/after.txt")

while IFS= read -r line; do
  path="${line%%$'\t'*}"
  allowed "$path" && continue
  printf 'REMOVED/CHANGED: %s\n' "$line" >&2
  violations=$((violations + 1))
done < <(LC_ALL=C comm -23 "$work/before.txt" "$work/after.txt")

if ((violations)); then
  echo "FAIL: sandbox modified $violations host path(s) outside the shared set" >&2
  exit 1
fi

if [[ "$proj_before" != "$(manifest "$work/repo/.claude")$(shasum -a 256 "$work/repo/.mcp.json")" ]]; then
  echo 'FAIL: sandbox modified project-scoped agent configuration' >&2
  exit 1
fi

if [[ "$repo_before" != "$(manifest "$work/repo/.git")" ]]; then
  echo 'FAIL: sandbox modified the host repository git directory' >&2
  diff <(printf '%s\n' "$repo_before") <(manifest "$work/repo/.git") >&2 || true
  exit 1
fi

echo 'agent sandbox isolation tests passed'
