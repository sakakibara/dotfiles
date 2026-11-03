#!/usr/bin/env bash
# Run with: bash etc/tests/agent_session_name.sh   (from the repo root)
#
# The session-naming hook against fake herdr and claude commands: which
# workspace labels it may replace, when it asks for a name, that it asks only
# through the subscription login, and what it hands Claude as the title.

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
HOOK="$REPO_DIR/src/.agents/hooks/agent-session-name.py"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
fails=0; passes=0

mkdir -p "$work/bin"
cat > "$work/bin/herdr" <<'EOF'
#!/usr/bin/env python3
import json, os, sys
path = os.environ["FAKE_HERDR"]
st = json.load(open(path))
args = sys.argv[1:]
open(path + ".log", "a").write(" ".join(args) + "\n")
def out(result):
    print(json.dumps({"result": result}))
if args[:2] == ["pane", "get"]:
    pane = next((p for p in st["panes"] if p["pane_id"] == args[2]), None)
    sys.exit(1) if pane is None else out({"pane": pane})
elif args[:2] == ["pane", "list"]:
    out({"panes": st["panes"]})
elif args[:2] == ["workspace", "get"]:
    out({"workspace": {"workspace_id": args[2], "label": st["labels"][args[2]]}})
elif args[:2] == ["workspace", "rename"]:
    st["labels"][args[2]] = " ".join(args[3:])
    json.dump(st, open(path, "w"))
    out({"type": "ok"})
else:
    sys.exit(1)
EOF
cat > "$work/bin/claude" <<'EOF'
#!/bin/sh
echo "$*" >> "$FAKE_CLAUDE_LOG"
if [ "$1" = auth ]; then
  printf '{"loggedIn":true,"authMethod":"%s","apiProvider":"firstParty"}\n' "${FAKE_AUTH:-claude.ai}"
  exit 0
fi
[ -n "${ANTHROPIC_API_KEY:-}" ] && { echo "leaked API key" >&2; exit 3; }
echo "$FAKE_NAME"
EOF
chmod +x "$work/bin/herdr" "$work/bin/claude"

export PATH="$work/bin:$PATH" XDG_STATE_HOME="$work/state" CODEX_HOME="$work/codex"
export HERDR_ENV=1 HERDR_SOCKET_PATH="$work/herdr.sock" FAKE_HERDR="$work/herdr.json"
export FAKE_CLAUDE_LOG="$work/claude.log" FAKE_NAME="部会資料"

_check() {
  if [[ "$2" == "$3" ]]; then printf '  ✓ %s\n' "$1"; passes=$((passes+1))
  else printf '  ✗ %s: expected [%s], got [%s]\n' "$1" "$3" "$2"; fails=$((fails+1)); fi
}
_section() { printf '\n%s\n' "$1"; }

# herdr world: workspace w1 holds panes w1:p1 (claude) and w1:p2 (claude).
_world() {
  printf '{"panes":[{"pane_id":"w1:p1","workspace_id":"w1","agent":"claude","cwd":"/x/desk"},{"pane_id":"w1:p2","workspace_id":"w1","agent":"claude","cwd":"/x/desk"}],"labels":{"w1":"%s"}}' "$1" > "$FAKE_HERDR"
  rm -rf "$XDG_STATE_HOME" "$FAKE_CLAUDE_LOG"
}
_label() { python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["labels"]["w1"])' "$FAKE_HERDR"; }
_calls() { if [[ -f "$FAKE_CLAUDE_LOG" ]]; then grep -c -e "-p" "$FAKE_CLAUDE_LOG" || true; else echo 0; fi; }

# A Claude transcript with $2 user prompts and an optional custom title $3.
_transcript() {
  python3 - "$1" "$2" "${3:-}" <<'EOF'
import json, sys
path, count, custom = sys.argv[1], int(sys.argv[2]), sys.argv[3]
dumps = lambda entry: json.dumps(entry, ensure_ascii=False, separators=(",", ":"))
with open(path, "a") as f:
    f.write(dumps({"type": "ai-title", "aiTitle": "最初の依頼の要約"}) + "\n")
    for i in range(count):
        f.write(dumps({"type": "user", "message": {"content": f"依頼{i}"}}) + "\n")
        f.write(dumps({"type": "assistant", "message": {"content": [{"type": "text", "text": "返答"}]}}) + "\n")
    if custom:
        f.write(dumps({"type": "custom-title", "customTitle": custom}) + "\n")
EOF
}
_hook() {  # mode agent session transcript pane
  printf '{"session_id":"%s","transcript_path":"%s","cwd":"/x/desk"}' "$3" "$4" |
    HERDR_PANE_ID="$5" python3 "$HOOK" "$1" "$2"
}

_section "names a workspace that still has herdr's default label"
_world desk; t="$work/a.jsonl"; rm -f "$t"; _transcript "$t" 1
_hook update claude sa "$t" w1:p1
_check "label" "$(_label)" "部会資料"
_check "one generation" "$(_calls)" "1"

_section "keeps a label typed by hand"
_world "手で付けた名前"; rm -f "$t"; _transcript "$t" 1
_hook update claude sa "$t" w1:p1
_check "label" "$(_label)" "手で付けた名前"

_section "keeps a name another live session set, takes it once that pane has no agent"
_world desk; rm -f "$t"; _transcript "$t" 1
_hook update claude sa "$t" w1:p1
FAKE_NAME="別の作業"; tb="$work/b.jsonl"; rm -f "$tb"; _transcript "$tb" 1
_hook update claude sb "$tb" w1:p2
_check "other session kept out" "$(_label)" "部会資料"
python3 -c 'import json,sys; p=sys.argv[1]; d=json.load(open(p)); d["panes"][0]["agent"]=None; json.dump(d,open(p,"w"))' "$FAKE_HERDR"
_transcript "$tb" 5
_hook update claude sb "$tb" w1:p2
_check "taken over" "$(_label)" "別の作業"
FAKE_NAME="部会資料"

_section "asks again only after five more prompts"
_world desk; rm -f "$t"; _transcript "$t" 1
_hook update claude sa "$t" w1:p1
_transcript "$t" 4; _hook update claude sa "$t" w1:p1
_check "after four more" "$(_calls)" "1"
_transcript "$t" 1; _hook update claude sa "$t" w1:p1
_check "after five more" "$(_calls)" "2"

_section "hands the generated name to Claude once, and yields to /rename"
_world desk; rm -f "$t"; _transcript "$t" 1
_hook update claude sa "$t" w1:p1
_check "first prompt" "$(_hook prompt claude sa "$t" w1:p1)" '{"hookSpecificOutput": {"hookEventName": "UserPromptSubmit", "sessionTitle": "部会資料"}}'
_check "second prompt" "$(_hook prompt claude sa "$t" w1:p1)" ""
_transcript "$t" 0 "自分で付けた"
_hook update claude sa "$t" w1:p1
_check "workspace follows /rename" "$(_label)" "自分で付けた"
_check "no title handed back" "$(_hook prompt claude sa "$t" w1:p1)" ""

_section "uses only the subscription login"
_world desk; rm -f "$t"; _transcript "$t" 1
ANTHROPIC_API_KEY=sk-test _hook update claude sa "$t" w1:p1
_check "API key not passed on" "$(_label)" "部会資料"
_world desk; rm -f "$t"; _transcript "$t" 1
FAKE_AUTH=api_key _hook update claude sa "$t" w1:p1
_check "no generation without it" "$(_calls)" "0"
_check "falls back to the agent's title" "$(_label)" "最初の依頼の要約"

_section "does not touch herdr outside it, and names Codex sessions too"
_world desk; rm -f "$t"; _transcript "$t" 1; rm -f "$FAKE_HERDR.log"
HERDR_ENV='' _hook update claude sa "$t" w1:p1
_check "no herdr call" "$(cat "$FAKE_HERDR.log" 2>/dev/null)" ""
_world desk; mkdir -p "$CODEX_HOME"; tc="$work/c.jsonl"
printf '%s\n' '{"type":"response_item","payload":{"type":"message","role":"user","content":[{"type":"input_text","text":"# AGENTS.md instructions"}]}}' \
  '{"type":"response_item","payload":{"type":"message","role":"user","content":[{"type":"input_text","text":"依頼"}]}}' > "$tc"
_hook update codex sc "$tc" w1:p1
_check "codex label" "$(_label)" "部会資料"
_check "codex prompts counted" "$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1]))["prompts"])' "$XDG_STATE_HOME/agent-session-name/sessions/sc.json")" "1"

printf '\n%d passed, %d failed\n' "$passes" "$fails"
[[ $fails -eq 0 ]]
