#!/usr/bin/env bash
set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
script="$repo/src/.local/bin/agent-sandbox"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/home/.config/git" "$work/cache" "$work/fixture"
printf '%s\n' test > "$work/fixture/input.txt"
printf '[user]\n\tname = Fixture User\n\temail = fixture@test.invalid\n' > "$work/home/.config/git/config"
printf 'signers\n' > "$work/home/.config/git/allowed_signers"
printf 'ignored.txt\n' > "$work/home/.config/git/ignore"
printf 'SECRET-TOKEN-VALUE\n' > "$work/home/.config/git/account-token"

help=$(bash "$script" help)
for value in run-untrusted --repo-setup --signing 'claude|codex'; do
  [[ "$help" == *"$value"* ]] || { echo "FAIL: help missing $value" >&2; exit 1; }
done

cat > "$work/bin/docker" <<'EOF'
#!/usr/bin/env bash
{
  printf 'docker'
  [[ -n "${HERDR_AGENT:-}" ]] && printf ' {HERDR_AGENT=%s}' "$HERDR_AGENT"
  printf ' <%q>' "$@"
  printf '\n'
} >> "$ASB_TEST_LOG"
case "$1 $2" in
  "volume create") printf '%s\n' test-volume ;;
  "container inspect")
    if [[ "${ASB_TRACK_CONTAINERS:-0}" == 1 ]] && grep -Fxq "${!#}" "$ASB_CONTAINER_STATE" 2>/dev/null; then
      exit 0
    fi
    [[ "${ASB_CONTAINER_EXISTS:-0}" == 1 ]] || exit 1
    case "$*" in
      *agent-sandbox.agent*) printf '%s\n' "${ASB_CONTAINER_AGENT:-claude}" ;;
      *agent-sandbox.iso*) printf '%s\n' "${ASB_CONTAINER_ISO:-default}" ;;
      *agent-sandbox.workspace*) printf '%s\n' "${ASB_CONTAINER_WORKSPACE:-}" ;;
      *State.Running*) printf '%s\n' true ;;
    esac
    ;;
esac
if [[ "$1" == "run" && "${ASB_TRACK_CONTAINERS:-0}" == 1 ]]; then
  while (($#)); do
    if [[ "$1" == --name ]]; then
      printf '%s\n' "$2" >> "$ASB_CONTAINER_STATE"
      break
    fi
    shift
  done
fi
if [[ "$1" == "run" && "${ASB_DOCKER_EXIT:-0}" != 0 ]]; then
  exit "$ASB_DOCKER_EXIT"
fi
exit 0
EOF
chmod +x "$work/bin/docker"
cat > "$work/bin/codex" <<'EOF'
#!/usr/bin/env bash
python3 - <<'PY'
import os
import tomllib
with open(os.path.join(os.environ["CODEX_HOME"], "config.toml"), "rb") as handle:
    config = tomllib.load(handle)
keymap = config["tui"]["keymap"]
assert keymap["composer"]["history_search_previous"] == "ctrl-r"
assert keymap["editor"]["delete_backward_word"] == "ctrl-w"
assert keymap["editor"]["kill_line_start"] == "ctrl-u"
PY
echo 'Error: no transport configured; use --listen or enable remote control' >&2
exit 1
EOF
chmod +x "$work/bin/codex"
cat > "$work/bin/launchctl" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
chmod +x "$work/bin/launchctl"
ln -s "$script" "$work/bin/agent-sandbox"

run_sandbox() {
  ASB_TEST_LOG="$work/docker.log" \
  ASB_CONTAINER_STATE="$work/containers" \
  HOME="$work/home" \
  XDG_CACHE_HOME="$work/cache" \
  XDG_DATA_HOME="$work/data" \
  XDG_CONFIG_HOME="$work/home/.config" \
  MOX_REPO="$repo" \
  PATH="$work/bin:$PATH" \
    bash "$script" "$@"
}

: > "$work/docker.log"
run_sandbox run-untrusted --image test-image "$work/fixture" -- sh -c 'test -f input.txt'
log=$(cat "$work/docker.log")
for value in '--network> <none' '--cap-drop> <ALL' 'no-new-privileges' '--read-only' '--memory> <4g' '--cpus> <4' '--pids-limit> <512' '/workspace:rw'; do
  [[ "$log" == *"$value"* ]] || { echo "FAIL: untrusted runner missing $value" >&2; exit 1; }
done
for value in '.claude' '.codex' 'SANDBOX_AGENT' 'host-1password' '.config/git'; do
  [[ "$log" != *"$value"* ]] || { echo "FAIL: untrusted runner exposed $value" >&2; exit 1; }
done
[[ "$log" != *'<volume> <create>'* ]] || { echo 'FAIL: untrusted runner persisted a workspace volume' >&2; exit 1; }

grep -q 'ASB_REPO_SETUP' "$repo/etc/sandbox/entrypoint.sh" || { echo 'FAIL: repository setup is not gated' >&2; exit 1; }

: > "$work/docker.log"
ln -s "$work/home" "$work/fixture/external-home"
run_sandbox "$work/fixture"
log=$(cat "$work/docker.log")
[[ "$log" == *'<claude> <--remote-control>'*'<--permission-mode> <acceptEdits>'* ]] || { echo 'FAIL: default mode is not Claude acceptEdits' >&2; exit 1; }
[[ "$log" == *'{HERDR_AGENT=claude} <run>'* ]] || { echo 'FAIL: docker run lacks the herdr agent hint, so a sandbox pane shows no agent identity or status' >&2; exit 1; }
for value in 'ASB_REPO_SETUP' 'SANDBOX_AGENT_HOST' 'SANDBOX_AGENT_SOCK' "$work/home:$work/home" '.codex'; do
  [[ "$log" != *"$value"* ]] || { echo "FAIL: default mode exposed optional capability $value" >&2; exit 1; }
done

: > "$work/docker.log"
mkdir -p "$work/home/.agents"
run_sandbox "$work/fixture"
log=$(cat "$work/docker.log")
[[ "$log" == *"data/agent-sandbox/home/agent-sandbox-fixture:$work/home"* ]] || { echo 'FAIL: per-slot agent home not mounted at the host home path' >&2; exit 1; }
[[ "$log" != *"$work/home/.claude.json:"* ]] || { echo 'FAIL: .claude.json is still bind-mounted as a single file' >&2; exit 1; }
[[ "$log" == *"$work/home/.agents:$work/home/.agents:ro"* ]] || { echo 'FAIL: ~/.agents not mounted read-only at the host path' >&2; exit 1; }
[[ "$log" == *"$work/home/.agents:/home/claude/.agents:ro"* ]] || { echo 'FAIL: ~/.agents not mounted read-only for hook resolution' >&2; exit 1; }
[[ -f "$work/data/agent-sandbox/home/agent-sandbox-fixture/.claude.json" ]] || { echo 'FAIL: per-slot agent home was not seeded with .claude.json' >&2; exit 1; }
[[ "$log" == *"$work/data/agent-docs:$work/data/agent-docs"* ]] || { echo 'FAIL: the agent docs directory is not mounted, so sandbox specs cannot reach the host' >&2; exit 1; }
[[ "$log" == *'<GIT_CONFIG_KEY_1=user.email>'*'<GIT_CONFIG_VALUE_1=fixture@test.invalid>'* ]] || { echo 'FAIL: git identity is not passed as env config, so the sandbox cannot commit and `git config user.email` reads empty' >&2; exit 1; }
want_name=$(printf '%q' 'GIT_CONFIG_VALUE_0=Fixture User')
[[ "$log" == *'<GIT_CONFIG_KEY_0=user.name>'*"$want_name"* ]] || { echo 'FAIL: user.name is not passed as env config' >&2; exit 1; }
[[ "$log" == *'<GIT_CONFIG_COUNT=2>'* ]] || { echo 'FAIL: GIT_CONFIG_COUNT is missing, so git ignores the identity env config' >&2; exit 1; }
[[ "$log" != *'GH_TOKEN'* ]] || { echo 'FAIL: a GitHub token reached the sandbox' >&2; exit 1; }
[[ "$log" != *"$work/home/.config/git:"* ]] || { echo 'FAIL: the whole git config directory is mounted, exposing credential helpers and the account token' >&2; exit 1; }
[[ "$log" != *'account-token'* ]] || { echo 'FAIL: the git account token is exposed to the sandbox' >&2; exit 1; }
[[ "$log" == *"$work/home/.config/git/ignore:/home/claude/.config/git/ignore:ro"* ]] || { echo 'FAIL: the global git ignore is not mounted' >&2; exit 1; }

# The default mode's host-state binds: every host path that must be
# read-only or absent is emitted exactly so.
: > "$work/docker.log"
mkdir -p "$work/home/.config/git/hooks" "$work/home/.claude/plugins/cache" "$work/gitrepo/.git/hooks" "$work/gitrepo/.claude"
printf 'hook\n' > "$work/home/.config/git/hooks/commit-msg"
printf '[core]\n' > "$work/gitrepo/.git/config"
printf '{}\n' > "$work/gitrepo/.mcp.json"
printf 'history\n' > "$work/home/.claude/history.jsonl"
printf 'creds\n' > "$work/home/.claude/.credentials.json"
enc=$(printf '%s' "$work/gitrepo" | tr '/.' '--')
mkdir -p "$work/home/.claude/projects/$enc"
mkdir -p "$work/etc-target" "$work/gitrepo/links" "$work/home/.ssh"
ln -s "$work/home/.config/git" "$work/gitrepo/links/gitconfig"
ln -s "$work/home/.ssh" "$work/gitrepo/links/ssh"
ln -s /etc "$work/gitrepo/links/etc"
outside=$(mktemp -d)
trap 'rm -rf "$work" "$outside"' EXIT
ln -s "$outside" "$work/gitrepo/links/outside"
# A stub holt names roots that admit none of these links, so the roots
# filter, not the absence of holt, is what keeps them out.
mkdir -p "$work/real" "$work/hub"
# The holt roots are the only places a workspace symlink may lead: a stub
# holt names them, since nothing else about the fixture is a real workspace.
cat > "$work/bin/holt" <<EOF
#!/bin/sh
[ "\$1" = config ] || exit 1
printf 'code_root = %s\\nhub_root = %s\\nsynced_root = %s\\n' "$work/real" "$work/hub" "$work/real"
EOF
chmod +x "$work/bin/holt"
run_sandbox "$work/gitrepo"
log=$(cat "$work/docker.log")
for value in \
  "$work/home/.config/git/hooks:/home/claude/.config/git/hooks:ro" \
  "$work/home/.config/git/hooks:$work/home/.config/git/hooks:ro" \
  "$work/home/.claude/plugins/cache:$work/home/.claude/plugins/cache:ro" \
  "$work/home/.claude/projects/$enc:$work/home/.claude/projects/$enc" \
  "$work/home/.claude/history.jsonl:$work/home/.claude/history.jsonl" \
  "$work/home/.claude/.credentials.json:$work/home/.claude/.credentials.json" \
  "$work/gitrepo/.git/hooks:$work/gitrepo/.git/hooks:ro" \
  "$work/gitrepo/.git/config:$work/gitrepo/.git/config:ro" \
  "$work/gitrepo/.claude:$work/gitrepo/.claude:ro" \
  "$work/gitrepo/.mcp.json:$work/gitrepo/.mcp.json:ro"; do
  [[ "$log" == *"$value"* ]] || { echo "FAIL: default mode mount plan lacks $value" >&2; exit 1; }
done
[[ "$log" == *'<GIT_CONFIG_KEY_2=core.hooksPath>'*'<GIT_CONFIG_VALUE_2=/home/claude/.config/git/hooks>'*'<GIT_CONFIG_COUNT=3>'* ]] || { echo 'FAIL: core.hooksPath is not passed as env config, so the commit-msg check is absent inside the container' >&2; exit 1; }
for value in "$work/home/.config/git:$work/home/.config/git" "$work/home/.ssh:" '</etc:/etc>' "$outside:"; do
  [[ "$log" != *"$value"* ]] || { echo "FAIL: a workspace symlink mounted $value" >&2; exit 1; }
done
rm -f "$work/bin/holt"

: > "$work/docker.log"
run_sandbox "$work/fixture" -- --debug --verbose
log=$(cat "$work/docker.log")
[[ "$log" == *'<--permission-mode> <acceptEdits> <--debug> <--verbose>'* ]] || { echo 'FAIL: agent passthrough did not reach the agent' >&2; exit 1; }
[[ "$log" == *"$work/fixture:$work/fixture"* ]] || { echo 'FAIL: agent passthrough swallowed the repo path' >&2; exit 1; }

: > "$work/docker.log"
mkdir -p "$work/hub/code" "$work/real/repo" "$work/real/design docs"
# The holt roots are the only places a workspace symlink may lead: a stub
# holt names them, since nothing else about the fixture is a real workspace.
cat > "$work/bin/holt" <<EOF
#!/bin/sh
[ "\$1" = config ] || exit 1
printf 'code_root = %s\\nhub_root = %s\\nsynced_root = %s\\n' "$work/real" "$work/hub" "$work/real"
EOF
chmod +x "$work/bin/holt"
ln -s "$work/real/repo" "$work/hub/code/repo"
ln -s "$work/real/design docs" "$work/hub/docs"
# A link whose literal target reaches a root only through an alias outside
# it: the physical path is inside, the literal one is not, and only paths
# inside the roots are mounted.
mkdir -p "$work/outside"
ln -s "$work/real" "$work/outside/alias"
ln -s "$work/outside/alias/repo" "$work/hub/sneaky"
# A literal target inside a root that is itself a link out of every root:
# docker would follow it host-side, so neither path of the pair is mounted.
mkdir -p "$work/secret"
ln -s "$work/secret" "$work/real/alias"
ln -s "$work/real/alias" "$work/hub/leak"
# A literal target that leaves a root through `..` is not inside it, whatever
# the prefix says; one that stays inside a root through `..` is still not
# mounted as written, since the container would resolve it host-side.
ln -s "$work/real/../outside" "$work/hub/dotdot"
mkdir -p "$work/real/sub"
ln -s "$work/real/sub/../sub" "$work/hub/dotdot-inside"
run_sandbox "$work/hub"
log=$(cat "$work/docker.log")
real_phys=$(cd "$work/real" && pwd -P)
[[ "$log" == *"$real_phys/repo:$real_phys/repo"* ]] || { echo 'FAIL: hub code symlink target not mounted' >&2; exit 1; }
[[ "$log" == *"$work/real/repo:$work/real/repo"* ]] || { echo 'FAIL: hub code symlink literal target not mounted' >&2; exit 1; }
[[ "$log" != *"$work/outside/alias"* ]] || { echo 'FAIL: a literal target outside the roots was mounted through an alias' >&2; exit 1; }
[[ "$log" != *"$work/real/../outside"* ]] || { echo 'FAIL: a literal target leaving a root through .. was mounted' >&2; exit 1; }
[[ "$log" != *"$work/real/sub/../sub"* && "$log" != *"$work/real/sub:"* ]] || { echo 'FAIL: a literal target reaching a root through .. was mounted' >&2; exit 1; }
[[ "$log" != *"$work/real/alias"* && "$log" != *"$work/secret"* ]] || { echo 'FAIL: a link inside a root that points out of it was mounted' >&2; exit 1; }
want_spaced=$(printf '%q' "$real_phys/design docs:$real_phys/design docs")
[[ "$log" == *"$want_spaced"* ]] || { echo 'FAIL: hub docs symlink target with a space not mounted' >&2; exit 1; }
[[ "$log" != *"$work/home:$work/home"* ]] || { echo 'FAIL: a symlink to the host home was followed' >&2; exit 1; }
# A sibling of the workspace is not a project root: with no holt roots
# admitting it, the link is dropped.
: > "$work/docker.log"
mkdir -p "$work/sibling/proj/links" "$work/sibling/private"
ln -s "$work/sibling/private" "$work/sibling/proj/links/private"
mv "$work/bin/holt" "$work/bin/holt.off"
run_sandbox "$work/sibling/proj"
mv "$work/bin/holt.off" "$work/bin/holt"
log=$(cat "$work/docker.log")
[[ "$log" != *"$work/sibling/private:"* ]] || { echo 'FAIL: a workspace symlink mounted a sibling directory' >&2; exit 1; }

: > "$work/docker.log"
( cd "$work/fixture" && run_sandbox --bypass -- --debug )
log=$(cat "$work/docker.log")
[[ "$log" == *'<--dangerously-skip-permissions> <--debug>'* ]] || { echo 'FAIL: passthrough without a positional repo did not reach the agent' >&2; exit 1; }
[[ "$log" == *"$work/fixture:$work/fixture"* ]] || { echo 'FAIL: passthrough without a positional repo lost the cwd workspace' >&2; exit 1; }

: > "$work/docker.log"
run_sandbox codex "$work/fixture" -- --search
log=$(cat "$work/docker.log")
[[ "$log" == *'<on-request> <--search>'* ]] || { echo 'FAIL: Codex agent passthrough did not reach the agent' >&2; exit 1; }

: > "$work/docker.log"
run_sandbox --signing "$work/fixture"
log=$(cat "$work/docker.log")
[[ "$log" == *"$work/home/.config/git/config:/home/claude/.config/git/config:ro"* ]] || { echo 'FAIL: signing mode omitted Git configuration' >&2; exit 1; }
[[ "$log" == *"$work/home/.config/git/allowed_signers:/home/claude/.config/git/allowed_signers:ro"* ]] || { echo 'FAIL: signing mode omitted the signer list' >&2; exit 1; }
[[ "$log" != *'account-token'* ]] || { echo 'FAIL: signing mode exposes the git account token' >&2; exit 1; }

: > "$work/docker.log"
mkdir -p "$work/home/.codex"
printf '{"hooks":{}}
' > "$work/home/.codex/hooks.json"
ln -sfn ../.agents/instructions.md "$work/home/.codex/AGENTS.md"
run_sandbox codex "$work/fixture"
log=$(cat "$work/docker.log")
[[ "$log" == *'<--label> <agent-sandbox.agent=codex>'* ]] || { echo 'FAIL: Codex agent label missing' >&2; exit 1; }
[[ "$log" == *'<codex> <--sandbox> <workspace-write> <--ask-for-approval> <on-request>'* ]] || { echo 'FAIL: Codex launch arguments missing' >&2; exit 1; }
[[ "$log" != *"$work/home/.codex:$work/home/.codex"* ]] || { echo 'FAIL: Codex state directory is mounted wholesale' >&2; exit 1; }
[[ "$log" == *"data/agent-sandbox/home/agent-sandbox-fixture-codex:$work/home"* ]] || { echo 'FAIL: Codex per-slot agent home not mounted' >&2; exit 1; }
[[ -f "$work/data/agent-sandbox/home/agent-sandbox-fixture-codex/.codex/hooks.json" ]] || { echo 'FAIL: Codex hook declarations not seeded into the slot' >&2; exit 1; }
[[ -L "$work/data/agent-sandbox/home/agent-sandbox-fixture-codex/.codex/AGENTS.md" ]] || { echo 'FAIL: Codex instruction symlink not recreated' >&2; exit 1; }
for value in '.claude.json' '.claude:'; do
  [[ "$log" != *"$value"* ]] || { echo "FAIL: Codex launch exposed $value" >&2; exit 1; }
done
[[ "$log" == *'{HERDR_AGENT=codex} <run>'* ]] || { echo 'FAIL: docker run lacks the herdr agent hint, so a sandbox pane shows no agent identity or status' >&2; exit 1; }
grep 'HERDR_AGENT=' "$work/docker.log" | grep -qv '{HERDR_AGENT=codex} <run>' && { echo 'FAIL: the herdr agent hint leaked beyond the agent run command' >&2; exit 1; }

: > "$work/docker.log"
mkdir -p "$work/home/.config/opencode" "$work/home/.local/share/opencode"
printf '{"$schema":"https://opencode.ai/config.json","provider":{"local":{"options":{"baseURL":"http://127.0.0.1:19999/v1"}}}}\n' > "$work/home/.config/opencode/opencode.jsonc"
printf 'token\n' > "$work/home/.local/share/opencode/auth.json"
run_sandbox opencode "$work/fixture"
log=$(cat "$work/docker.log")
slot="$work/data/agent-sandbox/home/agent-sandbox-fixture-opencode"
[[ "$log" == *'<--label> <agent-sandbox.agent=opencode>'* ]] || { echo 'FAIL: opencode agent label missing' >&2; exit 1; }
# opencode allows everything unless told otherwise, so --auto must supply the
# permission block; a bare launch would be a fully permissive agent.
[[ "$log" == *'OPENCODE_CONFIG_CONTENT='* ]] || { echo 'FAIL: opencode launch carried no inline config' >&2; exit 1; }
[[ "$log" == *'permission'*'edit'*'allow'*'bash'*'ask'* ]] || { echo 'FAIL: opencode default mode did not restrict permissions' >&2; exit 1; }
# These ride on the injected config, which outranks every file, so they hold
# even on a machine with no opencode configuration to seed from.
[[ "$log" == *'share'*'disabled'* ]] || { echo 'FAIL: opencode session sharing was not disabled for the sandbox' >&2; exit 1; }
[[ "$log" == *'autoupdate'*'false'* ]] || { echo 'FAIL: opencode self-update was not disabled for the sandbox' >&2; exit 1; }
[[ "$log" == *'<opencode>'* ]] || { echo 'FAIL: opencode was not launched' >&2; exit 1; }
[[ "$log" != *'<opencode> <--auto>'* ]] || { echo 'FAIL: opencode default mode used the bypass flag' >&2; exit 1; }
[[ "$log" != *"$work/home/.config/opencode:"* ]] || { echo 'FAIL: opencode config directory is mounted wholesale' >&2; exit 1; }
[[ "$log" != *"$work/home/.local/share/opencode:"* ]] || { echo 'FAIL: opencode data directory is mounted wholesale' >&2; exit 1; }
[[ "$log" == *"$work/home/.local/share/opencode/auth.json:$work/home/.local/share/opencode/auth.json"* ]] || { echo 'FAIL: opencode credentials not shared with the host' >&2; exit 1; }
[[ -f "$slot/.config/opencode/opencode.jsonc" ]] || { echo 'FAIL: opencode config not seeded into the slot' >&2; exit 1; }
[[ -d "$slot/.local/state/opencode" ]] || { echo 'FAIL: opencode state directory not seeded into the slot' >&2; exit 1; }
grep -q 'host.docker.internal:19999' "$slot/.config/opencode/opencode.jsonc" || { echo 'FAIL: a host loopback model endpoint was not rewritten for the container' >&2; exit 1; }
grep -q '127.0.0.1' "$slot/.config/opencode/opencode.jsonc" && { echo 'FAIL: the slot config still points at the container loopback' >&2; exit 1; }
grep -q '127.0.0.1' "$work/home/.config/opencode/opencode.jsonc" || { echo 'FAIL: the host opencode config was rewritten and must not be' >&2; exit 1; }
for value in '.claude.json' '.codex:'; do
  [[ "$log" != *"$value"* ]] || { echo "FAIL: opencode launch exposed $value" >&2; exit 1; }
done
[[ "$log" == *'{HERDR_AGENT=opencode} <run>'* ]] || { echo 'FAIL: docker run lacks the herdr agent hint for opencode' >&2; exit 1; }

: > "$work/docker.log"
mv "$work/home/.config/opencode/opencode.jsonc" "$work/home/.config/opencode/.stashed"
run_sandbox opencode "$work/fixture"
log=$(tail -1 "$work/docker.log")
[[ "$log" == *'share'*'disabled'* ]] || { echo 'FAIL: sharing is disabled only when a host config exists to seed' >&2; exit 1; }
[[ "$log" == *'autoupdate'*'false'* ]] || { echo 'FAIL: self-update is disabled only when a host config exists to seed' >&2; exit 1; }
mv "$work/home/.config/opencode/.stashed" "$work/home/.config/opencode/opencode.jsonc"

# A loopback address means the host, which is not the container's loopback.
# Every form it can take must be rewritten, and nothing that merely contains the
# word must be.
rw=$(mktemp -d)
check_rewrite() {
  printf '{"baseURL":"%s"}\n' "$1" > "$rw/c.json"
  sed -n '/^_rewrite_loopback/,/^}/p' "$script" > "$rw/fn.sh"
  bash -c '. "$1"; _rewrite_loopback "$2"' _ "$rw/fn.sh" "$rw/c.json"
  local got; got=$(sed 's/.*"baseURL":"\([^"]*\)".*/\1/' "$rw/c.json")
  if [[ "$2" == rewrite ]]; then
    [[ "$got" == *host.docker.internal* ]] || { echo "FAIL: $1 was not redirected to the host" >&2; exit 1; }
  else
    [[ "$got" == "$1" ]] || { echo "FAIL: $1 was redirected and should not be" >&2; exit 1; }
  fi
}
check_rewrite 'http://127.0.0.1:1234/v1'          rewrite
check_rewrite 'http://localhost:1234/v1'          rewrite
check_rewrite 'http://[::1]:1234/v1'              rewrite
check_rewrite 'http://localhost/v1'               rewrite
check_rewrite 'http://127.0.0.1'                  rewrite
check_rewrite 'https://api.example.com/v1'        leave
check_rewrite 'https://localhost.example.com/v1'  leave
check_rewrite 'https://my-localhost-proxy.net/v1' leave
rm -rf "$rw"

# A model id reaches injected JSON, so an id that would break that document is
# refused. Ids arriving from a server are filtered by the same rule.
idfn=$(mktemp)
sed -n '/^_valid_model_id/,/^}/p' "$script" > "$idfn"
id_ok() { bash -c '. "$1"; _valid_model_id "$2"' _ "$idfn" "$1"; }
for good in 'vendor.2:30b-q4' 'namespace/Model-Name' 'a_b.c-d@e+f'; do
  id_ok "$good" || { echo "FAIL: a legitimate model id was rejected: $good" >&2; exit 1; }
done
for bad in 'bad","x":"y' 'has space' 'quote"inside' 'new
line'; do
  id_ok "$bad" && { echo "FAIL: a model id that breaks injected JSON was accepted: $bad" >&2; exit 1; }
done

# The branch name must not depend on a variable the caller happens to have in
# scope: git rejects a ref that ends in a slash, so an empty one is fatal.
brfn=$(mktemp)
sed -n '/^_worktree_branch/,/^}/p' "$script" > "$brfn"
branch=$(bash -c 'NAME_PREFIX=agent-sandbox
. "$1"
_worktree_branch /repo agent-sandbox-fixture-wt' _ "$brfn")
rm -f "$brfn"
[[ "$branch" == "agent-sandbox/fixture-wt" ]] || { echo "FAIL: worktree branch name is '$branch', not derived from the container name" >&2; exit 1; }
[[ "$branch" != */ ]] || { echo 'FAIL: worktree branch name ends in a slash, which git refuses' >&2; exit 1; }

: > "$work/docker.log"
run_sandbox opencode --model local-model-x "$work/fixture"
log=$(tail -1 "$work/docker.log")
[[ "$log" == *'local-model-x'* ]] || { echo 'FAIL: a named model never reached the opencode configuration' >&2; exit 1; }
[[ "$log" == *'host.docker.internal:8080'* || "$log" == *'host.docker.internal:11434'* ]] || { echo 'FAIL: the model provider does not point at the host server' >&2; exit 1; }

# a model name must not be a flag the other agents silently discard
: > "$work/docker.log"
run_sandbox claude --model a-claude-model "$work/fixture"
log=$(tail -1 "$work/docker.log")
[[ "$log" == *'<--model> <a-claude-model>'* ]] || { echo 'FAIL: --model is silently ignored for Claude' >&2; exit 1; }
: > "$work/docker.log"
run_sandbox codex --model a-codex-model "$work/fixture"
log=$(tail -1 "$work/docker.log")
[[ "$log" == *'<--model> <a-codex-model>'* ]] || { echo 'FAIL: --model is silently ignored for Codex' >&2; exit 1; }

# a model id that would break the injected JSON takes every setting in that
# document down with it, so it is refused rather than emitted
set +e
out=$(run_sandbox opencode --model 'bad","x":"y' "$work/fixture" 2>&1)
rc=$?
set -e
[[ $rc -ne 0 && "$out" == *'--model accepts'* ]] || { echo 'FAIL: a model id that breaks the injected JSON was accepted' >&2; exit 1; }
set +e
out=$(run_sandbox claude --model-port 9999 "$work/fixture" 2>&1); rc=$?
set -e
[[ $rc -ne 0 && "$out" == *'opencode only'* ]] || { echo 'FAIL: --model-port was accepted for an agent that ignores it' >&2; exit 1; }

: > "$work/docker.log"
run_sandbox opencode --bypass "$work/fixture"
log=$(tail -1 "$work/docker.log")
[[ "$log" == *'<opencode> <--auto>'* ]] || { echo 'FAIL: opencode bypass did not auto-approve' >&2; exit 1; }
[[ "$log" != *'permission'* ]] || { echo 'FAIL: opencode bypass still sent a permission block' >&2; exit 1; }

: > "$work/docker.log"
run_sandbox opencode --continue "$work/fixture"
log=$(tail -1 "$work/docker.log")
[[ "$log" == *'<opencode> <--continue>'* ]] || { echo 'FAIL: opencode continue lost its flag' >&2; exit 1; }

: > "$work/docker.log"
run_sandbox opencode --resume=abc123 "$work/fixture"
log=$(tail -1 "$work/docker.log")
[[ "$log" == *'<opencode> <--session> <abc123>'* ]] || { echo 'FAIL: opencode named resume did not map to --session' >&2; exit 1; }

: > "$work/docker.log"
run_sandbox codex --resume "$work/fixture"
log=$(tail -1 "$work/docker.log")
[[ "$log" == *'<codex> <resume> <--sandbox>'* && "$log" != *"<''>"* ]] || { echo 'FAIL: bare Codex resume emitted a session argument' >&2; exit 1; }

: > "$work/docker.log"
run_sandbox codex --resume=session-name "$work/fixture"
log=$(tail -1 "$work/docker.log")
[[ "$log" == *'<codex> <resume> <session-name> <--sandbox>'* ]] || { echo 'FAIL: named Codex resume lost its argument' >&2; exit 1; }

: > "$work/docker.log"
set +e
ASB_DOCKER_EXIT=23 run_sandbox codex "$work/fixture"
status=$?
set -e
[[ $status -eq 23 ]] || { echo "FAIL: agent exit status changed to $status" >&2; exit 1; }

git -C "$work/fixture" init -q
git -C "$work/fixture" add input.txt
git -C "$work/fixture" -c commit.gpgsign=false -c user.email=fixture@test.invalid -c "user.name=fixture" commit -qm fixture
: > "$work/docker.log"
run_sandbox --worktree "$work/fixture"
claude_log=$(tail -1 "$work/docker.log")
: > "$work/docker.log"
run_sandbox codex --worktree "$work/fixture"
codex_log=$(tail -1 "$work/docker.log")
[[ "$claude_log" == *'-fixture-wt:'* && "$codex_log" == *'-fixture-codex-wt:'* ]] || { echo 'FAIL: agent worktree paths are not distinct' >&2; exit 1; }

: > "$work/docker.log"
run_sandbox --worktree --name one "$work/fixture"
first_worktree_log=$(tail -1 "$work/docker.log")
: > "$work/docker.log"
run_sandbox --worktree --name two "$work/fixture"
second_worktree_log=$(tail -1 "$work/docker.log")
[[ "$first_worktree_log" == *'-fixture-wt-one:'* && "$second_worktree_log" == *'-fixture-wt-two:'* ]] || { echo 'FAIL: parallel same-agent worktrees are not distinct' >&2; exit 1; }

claude_worktree=$(find "$work/cache/agent-sandbox/worktrees" -type d -name '*-fixture-wt' -print -quit)
codex_worktree=$(find "$work/cache/agent-sandbox/worktrees" -type d -name '*-fixture-codex-wt' -print -quit)
[[ -n "$claude_worktree" && -n "$codex_worktree" ]] || { echo 'FAIL: expected agent worktrees are missing' >&2; exit 1; }
ASB_CONTAINER_EXISTS=1 ASB_CONTAINER_AGENT=codex ASB_CONTAINER_ISO=worktree ASB_CONTAINER_WORKSPACE="$work/fixture" \
  run_sandbox stop --purge agent-sandbox-fixture-codex-wt
[[ -d "$claude_worktree" && ! -e "$codex_worktree" ]] || { echo 'FAIL: explicit Codex purge selected the wrong agent worktree' >&2; exit 1; }

ASB_CONTAINER_EXISTS=1 ASB_CONTAINER_AGENT=claude ASB_CONTAINER_ISO=worktree ASB_CONTAINER_WORKSPACE="$work/fixture" \
  run_sandbox stop --purge agent-sandbox-fixture-wt-one
named_worktree=$(find "$work/cache/agent-sandbox/worktrees" -type d -name '*-fixture-wt-one' -print -quit)
[[ -z "$named_worktree" ]] || { echo 'FAIL: named worktree purge left its worktree behind' >&2; exit 1; }

: > "$work/docker.log"
ASB_CONTAINER_EXISTS=1 ASB_CONTAINER_AGENT=claude ASB_CONTAINER_ISO=strict ASB_CONTAINER_WORKSPACE="$work/fixture" \
  run_sandbox stop --purge agent-sandbox-fixture-strict-one
log=$(cat "$work/docker.log")
[[ "$log" == *'<volume> <rm> <agent-sandbox-fixture-strict-one-ws> <agent-sandbox-fixture-strict-one-cl>'* ]] || { echo 'FAIL: named strict purge selected the wrong volumes' >&2; exit 1; }

: > "$work/docker.log"
: > "$work/containers"
ASB_TRACK_CONTAINERS=1 run_sandbox --strict "$work/fixture"
ASB_TRACK_CONTAINERS=1 run_sandbox --strict "$work/fixture"
log=$(cat "$work/docker.log")
[[ "$log" == *'<agent-sandbox-fixture-strict-ws:'* && "$log" == *'<agent-sandbox-fixture-strict-2-ws:'* ]] || { echo 'FAIL: numbered strict sandboxes share workspace volumes' >&2; exit 1; }
[[ "$log" == *'<agent-sandbox-fixture-strict-cl:'* && "$log" == *'<agent-sandbox-fixture-strict-2-cl:'* ]] || { echo 'FAIL: numbered strict sandboxes share agent volumes' >&2; exit 1; }

run_sandbox enable-autostart "$work/fixture"
run_sandbox codex enable-autostart "$work/fixture"
plist_count=$(find "$work/home/Library/LaunchAgents" -name 'dev.sakakibara.agent-sandbox.*.plist' | wc -l | tr -d ' ')
[[ $plist_count -eq 2 ]] || { echo 'FAIL: Claude and Codex autostart identities collide' >&2; exit 1; }
# Every flag and the agent passthrough reach the plist, escaped; a named slot
# is its own autostart, and a repeat of the name replaces it.
run_sandbox enable-autostart --bypass --worktree --signing --name one --model m1 "$work/fixture" -- --debug '--note=<a&b>'
run_sandbox enable-autostart --name two "$work/fixture"
out=$(run_sandbox enable-autostart --workspace "$work/fixture" "$work/fixture" 2>&1) && { echo 'FAIL: enable-autostart accepted --workspace' >&2; exit 1; }
[[ "$out" == *"not --workspace"* ]] || { echo "FAIL: enable-autostart --workspace refusal unnamed: $out" >&2; exit 1; }
for flag in --continue --resume=abc; do
  out=$(run_sandbox enable-autostart "$flag" "$work/fixture" 2>&1) && { echo "FAIL: enable-autostart accepted $flag" >&2; exit 1; }
  [[ "$out" == *"cannot resume a session"* ]] || { echo "FAIL: enable-autostart $flag refusal unnamed: $out" >&2; exit 1; }
done
plists=$(find "$work/home/Library/LaunchAgents" -name 'dev.sakakibara.agent-sandbox.claude.*.plist' | wc -l | tr -d ' ')
[[ $plists -eq 3 ]] || { echo "FAIL: named autostart slots collide ($plists claude plists)" >&2; exit 1; }
# A name is a filesystem-safe slot, by the rule the container name uses.
run_sandbox enable-autostart --name 'x/../../evil' "$work/fixture"
slotted=$(find "$work/home/Library/LaunchAgents" -name 'dev.sakakibara.agent-sandbox.claude.*.x_.._.._evil.plist' | wc -l | tr -d ' ')
[[ $slotted -eq 1 ]] || { echo 'FAIL: a slashed --name did not land as a sanitized plist slot' >&2; exit 1; }
[[ -z "$(find "$work/home" -name '*.plist' -not -path '*/Library/LaunchAgents/*')" ]] || { echo 'FAIL: a slashed --name wrote a plist outside LaunchAgents' >&2; exit 1; }
rm -f "$work/home/Library/LaunchAgents"/dev.sakakibara.agent-sandbox.claude.*.x_.._.._evil.plist
one=$(grep -l '<string>one</string>' "$work/home/Library/LaunchAgents"/dev.sakakibara.agent-sandbox.claude.*.plist)
for want in '<string>--bypass</string>' '<string>--worktree</string>' '<string>--signing</string>' '<string>--model</string>' '<string>m1</string>' '<string>--</string>' '<string>--debug</string>' '<string>--note=&lt;a&amp;b&gt;</string>'; do
  grep -qF "$want" "$one" || { echo "FAIL: autostart plist lacks $want" >&2; exit 1; }
done
run_sandbox enable-autostart --name one "$work/fixture"
grep -qF '<string>--bypass</string>' "$one" && { echo 'FAIL: re-enabling a named slot kept its old flags' >&2; exit 1; }
plists=$(find "$work/home/Library/LaunchAgents" -name 'dev.sakakibara.agent-sandbox.claude.*.plist' | wc -l | tr -d ' ')
[[ $plists -eq 3 ]] || { echo "FAIL: re-enabling a named slot made a new plist ($plists)" >&2; exit 1; }
# disable addresses the same slot enable wrote: the named one by name, the
# plain one without it, each leaving the others alone.
run_sandbox disable-autostart --name one "$work/fixture"
[[ -f "$one" ]] && { echo 'FAIL: disable-autostart --name one left its plist' >&2; exit 1; }
plists=$(find "$work/home/Library/LaunchAgents" -name 'dev.sakakibara.agent-sandbox.claude.*.plist' | wc -l | tr -d ' ')
[[ $plists -eq 2 ]] || { echo "FAIL: disabling one named slot removed $((3 - plists)) plists" >&2; exit 1; }
run_sandbox disable-autostart "$work/fixture"
plists=$(find "$work/home/Library/LaunchAgents" -name 'dev.sakakibara.agent-sandbox.claude.*.plist' | wc -l | tr -d ' ')
[[ $plists -eq 1 ]] || { echo "FAIL: disabling the plain slot left $plists claude plists" >&2; exit 1; }
grep -qF '<string>two</string>' "$work/home/Library/LaunchAgents"/dev.sakakibara.agent-sandbox.claude.*.plist || { echo 'FAIL: disabling the plain slot took the named one' >&2; exit 1; }

python3 - <<'PY' "$repo/src/.claude/keybindings.json"
import json
import sys
keys = json.load(open(sys.argv[1], encoding="utf-8"))
bindings = {block["context"]: block["bindings"] for block in keys["bindings"]}
expected = {
    "Global": {"ctrl+t": None, "ctrl+o": None},
    "Chat": {"ctrl+g": None, "ctrl+s": None, "ctrl+v": None, "ctrl+j": "chat:submit", "ctrl+x ctrl+k": None},
    "Task": {"ctrl+b": None, "ctrl+shift+b": "task:background"},
}
for context, values in expected.items():
    for key, action in values.items():
        assert bindings[context][key] == action
PY

# The Codex keymap is a mox partial-ownership source: the head declares the
# owned tables, the check hook, and the tool gate, and mox owns the patching
# mechanics (its own suite covers splice, idempotency, and refusals). Here we
# hold the CONTRACT: the declarations exist, the source carries the expected
# bindings, and the check script accepts a valid config and rejects garbage.
codex_src="$repo/src/.codex/config.toml"
grep -q '^# mox: own tui.keymap.global$' "$codex_src" || { echo 'FAIL: codex source does not own tui.keymap.global' >&2; exit 1; }
grep -q '^# mox: own tui.keymap.composer$' "$codex_src" || { echo 'FAIL: codex source does not own tui.keymap.composer' >&2; exit 1; }
grep -q '^# mox: own tui.keymap.editor$' "$codex_src" || { echo 'FAIL: codex source does not own tui.keymap.editor' >&2; exit 1; }
grep -q '^# mox: check "scripts/check/codex-config"$' "$codex_src" || { echo 'FAIL: codex source lacks the check hook' >&2; exit 1; }
grep -q '^# mox: when tool=codex$' "$codex_src" || { echo 'FAIL: codex source is not gated on the codex tool' >&2; exit 1; }
python3 - <<'PY' "$codex_src"
import sys
import tomllib
with open(sys.argv[1], "rb") as handle:
    body = b"".join(line for line in handle if not line.lstrip().startswith(b"# mox:"))
config = tomllib.loads(body.decode())
keymap = config["tui"]["keymap"]
assert keymap["global"]["open_transcript"] == "ctrl-shift-t"
assert keymap["composer"]["submit"] == ["enter", "ctrl-j", "ctrl-m"]
assert keymap["editor"]["move_left"] == "ctrl-b"
assert keymap["editor"]["delete_backward"] == ["backspace", "ctrl-h"]
assert keymap["editor"]["delete_backward_word"] == "ctrl-w"
assert keymap["editor"]["kill_line_start"] == "ctrl-u"
assert set(config) == {"tui"}, "codex source defines content outside the owned tables"
PY

check_dir="$work/check-candidate"
mkdir -p "$check_dir"
grep -v '^# mox:' "$codex_src" > "$check_dir/config.toml"
MOX_CHECK_DIR="$check_dir" MOX_CHECK_FILE="$check_dir/config.toml" PATH="$work/bin:$PATH" \
  bash "$repo/scripts/check/codex-config" || { echo 'FAIL: check script rejected a valid codex config' >&2; exit 1; }
# A rejecting codex (nonzero exit, no transport phrase) must propagate as a
# refusal -- the shared stub always ends with the acceptance phrase, so the
# negative path gets its own.
mkdir -p "$work/bin-reject"
cat > "$work/bin-reject/codex" <<'EOF'
#!/usr/bin/env bash
echo 'Error: config rejected by strict parser' >&2
exit 2
EOF
chmod +x "$work/bin-reject/codex"
set +e
MOX_CHECK_DIR="$check_dir" MOX_CHECK_FILE="$check_dir/config.toml" PATH="$work/bin-reject:$PATH" \
  bash "$repo/scripts/check/codex-config" >/dev/null 2>&1
check_status=$?
set -e
[[ $check_status -ne 0 ]] || { echo 'FAIL: check script accepted a rejected codex config' >&2; exit 1; }

grep -q '^prefix = "ctrl+q"$' "$repo/src/.config/herdr/config.toml" || { echo 'FAIL: herdr prefix drifted from the chosen ctrl+q' >&2; exit 1; }
grep -q '^unbind-key C-b$' "$repo/src/.tmux.conf" || { echo 'FAIL: tmux can intercept Ctrl-B' >&2; exit 1; }

echo "agent sandbox security tests passed"
