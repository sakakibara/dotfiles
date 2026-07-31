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
[[ "$log" == *'<GIT_AUTHOR_EMAIL=fixture@test.invalid>'* ]] || { echo 'FAIL: git identity is not passed, so the sandbox cannot commit' >&2; exit 1; }
want_name=$(printf '%q' 'GIT_COMMITTER_NAME=Fixture User')
[[ "$log" == *"$want_name"* ]] || { echo 'FAIL: committer identity is not passed' >&2; exit 1; }
[[ "$log" != *"$work/home/.config/git:"* ]] || { echo 'FAIL: the whole git config directory is mounted, exposing credential helpers and the account token' >&2; exit 1; }
[[ "$log" != *'account-token'* ]] || { echo 'FAIL: the git account token is exposed to the sandbox' >&2; exit 1; }
[[ "$log" == *"$work/home/.config/git/ignore:/home/claude/.config/git/ignore:ro"* ]] || { echo 'FAIL: the global git ignore is not mounted' >&2; exit 1; }

: > "$work/docker.log"
run_sandbox "$work/fixture" -- --debug --verbose
log=$(cat "$work/docker.log")
[[ "$log" == *'<--permission-mode> <acceptEdits> <--debug> <--verbose>'* ]] || { echo 'FAIL: agent passthrough did not reach the agent' >&2; exit 1; }
[[ "$log" == *"$work/fixture:$work/fixture"* ]] || { echo 'FAIL: agent passthrough swallowed the repo path' >&2; exit 1; }

: > "$work/docker.log"
mkdir -p "$work/hub/code" "$work/real/repo" "$work/real/design docs"
ln -s "$work/real/repo" "$work/hub/code/repo"
ln -s "$work/real/design docs" "$work/hub/docs"
run_sandbox "$work/hub"
log=$(cat "$work/docker.log")
real_phys=$(cd "$work/real" && pwd -P)
[[ "$log" == *"$real_phys/repo:$real_phys/repo"* ]] || { echo 'FAIL: hub code symlink target not mounted' >&2; exit 1; }
[[ "$log" == *"$work/real/repo:$work/real/repo"* ]] || { echo 'FAIL: hub code symlink literal target not mounted' >&2; exit 1; }
want_spaced=$(printf '%q' "$real_phys/design docs:$real_phys/design docs")
[[ "$log" == *"$want_spaced"* ]] || { echo 'FAIL: hub docs symlink target with a space not mounted' >&2; exit 1; }
[[ "$log" != *"$work/home:$work/home"* ]] || { echo 'FAIL: a symlink to the host home was followed' >&2; exit 1; }

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
