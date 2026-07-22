#!/usr/bin/env bash
set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
script="$repo/src/.local/bin/agent-sandbox"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/home/.config/git" "$work/cache" "$work/fixture"
printf '%s\n' test > "$work/fixture/input.txt"

help=$(bash "$script" help)
for value in run-untrusted --repo-setup --signing 'claude|codex'; do
  [[ "$help" == *"$value"* ]] || { echo "FAIL: help missing $value" >&2; exit 1; }
done

cat > "$work/bin/docker" <<'EOF'
#!/usr/bin/env bash
{
  printf 'docker'
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
cat > "$work/bin/herdr" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$ASB_HERDR_LOG"
EOF
chmod +x "$work/bin/herdr"
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
  ASB_HERDR_LOG="$work/herdr.log" \
  ASB_CONTAINER_STATE="$work/containers" \
  HOME="$work/home" \
  XDG_CACHE_HOME="$work/cache" \
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
for value in 'ASB_REPO_SETUP' 'SANDBOX_AGENT_HOST' 'SANDBOX_AGENT_SOCK' '.config/git' "$work/home:$work/home" '.codex'; do
  [[ "$log" != *"$value"* ]] || { echo "FAIL: default mode exposed optional capability $value" >&2; exit 1; }
done

: > "$work/docker.log"
run_sandbox --signing "$work/fixture"
log=$(cat "$work/docker.log")
[[ "$log" == *"$work/home/.config/git:/home/claude/.config/git:ro"* ]] || { echo 'FAIL: signing mode omitted Git configuration' >&2; exit 1; }

: > "$work/docker.log"
: > "$work/herdr.log"
mkdir -p "$work/home/.codex"
HERDR_ENV=1 HERDR_PANE_ID=pane-test run_sandbox codex "$work/fixture"
log=$(cat "$work/docker.log")
[[ "$log" == *'<--label> <agent-sandbox.agent=codex>'* ]] || { echo 'FAIL: Codex agent label missing' >&2; exit 1; }
[[ "$log" == *'<codex> <--sandbox> <workspace-write> <--ask-for-approval> <on-request>'* ]] || { echo 'FAIL: Codex launch arguments missing' >&2; exit 1; }
[[ "$log" == *"$work/home/.codex:$work/home/.codex"* ]] || { echo 'FAIL: Codex state mount missing' >&2; exit 1; }
for value in '.claude.json' '.claude:' '.config/git'; do
  [[ "$log" != *"$value"* ]] || { echo "FAIL: Codex launch exposed $value" >&2; exit 1; }
done
herdr_log=$(cat "$work/herdr.log")
[[ "$herdr_log" == *'report-agent pane-test'*'--agent codex --state unknown'* ]] || { echo 'FAIL: herdr did not receive Codex report' >&2; exit 1; }
[[ "$herdr_log" == *'release-agent pane-test'*'--agent codex'* ]] || { echo 'FAIL: herdr did not receive Codex release' >&2; exit 1; }

: > "$work/docker.log"
run_sandbox codex --resume "$work/fixture"
log=$(tail -1 "$work/docker.log")
[[ "$log" == *'<codex> <resume> <--sandbox>'* && "$log" != *"<''>"* ]] || { echo 'FAIL: bare Codex resume emitted a session argument' >&2; exit 1; }

: > "$work/docker.log"
run_sandbox codex --resume=session-name "$work/fixture"
log=$(tail -1 "$work/docker.log")
[[ "$log" == *'<codex> <resume> <session-name> <--sandbox>'* ]] || { echo 'FAIL: named Codex resume lost its argument' >&2; exit 1; }

: > "$work/docker.log"
: > "$work/herdr.log"
set +e
ASB_DOCKER_EXIT=23 HERDR_ENV=1 HERDR_PANE_ID=pane-test run_sandbox codex "$work/fixture"
status=$?
set -e
[[ $status -eq 23 ]] || { echo "FAIL: agent exit status changed to $status" >&2; exit 1; }
[[ $(cat "$work/herdr.log") == *'release-agent pane-test'* ]] || { echo 'FAIL: herdr release missing after agent failure' >&2; exit 1; }

git -C "$work/fixture" init -q
git -C "$work/fixture" add input.txt
git -C "$work/fixture" -c commit.gpgsign=false commit -qm fixture
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

codex_home="$work/codex-home"
mkdir -p "$codex_home"
cat > "$codex_home/config.toml" <<'EOF'
model = "test-model"

[projects."/tmp/example"]
trust_level = "trusted"

[tui.keymap.editor]
move_left = "broken"

[hooks.state.example]
trusted_hash = "test-hash"
EOF
CODEX_HOME="$codex_home" bash "$repo/scripts/post/codex-keybindings.sh"
first_hash=$(shasum -a 256 "$codex_home/config.toml" | cut -d' ' -f1)
CODEX_HOME="$codex_home" bash "$repo/scripts/post/codex-keybindings.sh"
second_hash=$(shasum -a 256 "$codex_home/config.toml" | cut -d' ' -f1)
[[ "$first_hash" == "$second_hash" ]] || { echo 'FAIL: Codex keymap patch is not idempotent' >&2; exit 1; }
python3 - <<'PY' "$codex_home/config.toml"
import sys
import tomllib
with open(sys.argv[1], "rb") as handle:
    config = tomllib.load(handle)
assert config["model"] == "test-model"
assert config["projects"]["/tmp/example"]["trust_level"] == "trusted"
assert config["hooks"]["state"]["example"]["trusted_hash"] == "test-hash"
keymap = config["tui"]["keymap"]
assert keymap["global"]["open_transcript"] == "ctrl-shift-t"
assert keymap["composer"]["submit"] == ["enter", "ctrl-j", "ctrl-m"]
assert keymap["editor"]["move_left"] == "ctrl-b"
assert keymap["editor"]["delete_backward_word"] == "ctrl-w"
assert keymap["editor"]["kill_line_start"] == "ctrl-u"
PY

ambiguous_home="$work/ambiguous-codex-home"
mkdir -p "$ambiguous_home"
cat > "$ambiguous_home/config.toml" <<'EOF'
model_instructions = """
[tui.keymap.editor]
This is data, not a TOML table.
"""
EOF
ambiguous_hash=$(shasum -a 256 "$ambiguous_home/config.toml" | cut -d' ' -f1)
set +e
CODEX_HOME="$ambiguous_home" bash "$repo/scripts/post/codex-keybindings.sh" >/dev/null 2>&1
ambiguous_status=$?
set -e
[[ $ambiguous_status -ne 0 ]] || { echo 'FAIL: Codex keymap patch accepted ambiguous multiline content' >&2; exit 1; }
[[ "$ambiguous_hash" == "$(shasum -a 256 "$ambiguous_home/config.toml" | cut -d' ' -f1)" ]] || { echo 'FAIL: rejected Codex config was modified' >&2; exit 1; }

grep -q '^prefix = "f12"$' "$repo/src/.config/herdr/config.toml" || { echo 'FAIL: herdr prefix occupies a readline binding' >&2; exit 1; }
grep -q '^unbind-key C-b$' "$repo/src/.tmux.conf" || { echo 'FAIL: tmux can intercept Ctrl-B' >&2; exit 1; }

echo "agent sandbox security tests passed"
