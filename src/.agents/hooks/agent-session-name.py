#!/usr/bin/env python3
"""Give agent sessions and their herdr workspaces a short name.

Usage: agent-session-name.py stop|prompt|update claude|codex, with the hook
input on stdin.

stop    Stop hook. Starts `update` in the background and returns at once.
update  Reads the transcript from where it last stopped. On the first stop of
        a session, and again after every NEW_PROMPTS further user prompts,
        asks Claude (haiku) for a short name from the first and latest
        prompts and the latest reply. Renames the herdr workspace holding the
        agent's pane to the session's name.
prompt  Claude UserPromptSubmit hook. Hands a newly generated name to Claude
        Code as the session title.

A title set with /rename wins over the generated one. The workspace is
renamed only while its label is herdr's default (the directory name) or the
name this script last set for the same session, so a name typed by hand, or
one another session set, is kept. When generation fails the agent's own title
is used. Every error ends the hook silently; it never blocks the agent.
"""
import hashlib
import json
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path

NEW_PROMPTS = 5
MAX_NAME = 12

STATE = Path(os.environ.get("XDG_STATE_HOME") or Path.home() / ".local" / "state") / "agent-session-name"

PROMPT = """AIエージェントとの作業セッションに、一覧で見分けるための短い名前を付けます。
条件:
- 日本語。英字の固有名詞はそのまま使ってよい
- 全角8文字以内を目安、最大10文字
- 作業の対象が一目で分かる名詞句。「〜の〜の〜」と続けない。「・」で並べない
- 「確認」「対応」「検討」「改善」「作業」などの中身のない語で終えない。記号や句読点を入れない
- 途中で話題が変わっていたら、今の話題を優先する
例（形だけ参考、語は流用しない）: 経費精算 / 採用LP改修 / 決済API移行
名前だけを1行で出力。

"""


def load(path, default):
    try:
        return json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return default


def save(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_name(path.name + ".tmp")
    tmp.write_text(json.dumps(data, ensure_ascii=False), encoding="utf-8")
    tmp.replace(path)


def session_file(session):
    return STATE / "sessions" / f"{session}.json"


def flat(text):
    return " ".join(text.split())


def user_prompt(text):
    return text and not text.startswith(("<", "# AGENTS.md", "[Request interrupted", "Caveat:"))


def claude_entry(entry, st):
    kind = entry.get("type")
    if kind == "custom-title":
        st["custom"] = entry.get("customTitle") or st.get("custom")
    elif kind == "ai-title":
        st["own"] = entry.get("aiTitle") or st.get("own")
    elif kind in ("user", "assistant") and not (
            entry.get("isMeta") or entry.get("isSidechain") or entry.get("isCompactSummary")):
        content = (entry.get("message") or {}).get("content")
        if isinstance(content, list):
            content = " ".join(x.get("text", "") for x in content
                               if isinstance(x, dict) and x.get("type") == "text")
        if isinstance(content, str):
            yield kind, flat(content)


def codex_entry(entry, st):
    payload = entry.get("payload") or {}
    if entry.get("type") != "response_item" or payload.get("type") != "message":
        return
    role = payload.get("role")
    if role not in ("user", "assistant"):
        return
    for part in payload.get("content") or []:
        if part.get("type") in ("input_text", "output_text"):
            yield role, flat(part.get("text", ""))


def read_new(agent, path, st):
    """Fold the transcript lines added since the last read into st."""
    markers = ('"custom-title"', '"ai-title"', '"type":"user"', '"type":"assistant"') if agent == "claude" \
        else ('"role":"user"', '"role":"assistant"')
    parse = claude_entry if agent == "claude" else codex_entry
    if os.path.getsize(path) < st.get("offset", 0):
        for key in ("offset", "prompts", "first", "recent", "reply"):
            st.pop(key, None)
    with open(path, "rb") as handle:
        handle.seek(st.get("offset", 0))
        for raw in handle:
            if not raw.endswith(b"\n"):
                break
            st["offset"] = st.get("offset", 0) + len(raw)
            line = raw.decode("utf-8", "replace")
            if not any(m in line for m in markers):
                continue
            try:
                entry = json.loads(line)
            except ValueError:
                continue
            for role, text in parse(entry, st):
                if role == "assistant":
                    if text:
                        st["reply"] = text[:500]
                elif user_prompt(text):
                    st["prompts"] = st.get("prompts", 0) + 1
                    if len(st.setdefault("first", [])) < 2:
                        st["first"].append(text[:200])
                    st["recent"] = (st.get("recent", []) + [text[:200]])[-5:]


def codex_own_title(session):
    home = Path(os.environ.get("CODEX_HOME") or Path.home() / ".codex")
    title = None
    with open(home / "session_index.jsonl", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            if session in line:
                try:
                    entry = json.loads(line)
                except ValueError:
                    continue
                if entry.get("id") == session:
                    title = entry.get("thread_name") or title
    return title


def generate(st):
    exe = shutil.which("claude")
    if not exe or not st.get("recent"):
        return None
    context = ("最初の依頼:\n" + "\n".join("- " + x for x in st.get("first", []))
               + "\n最近の依頼:\n" + "\n".join("- " + x for x in st["recent"])
               + "\nAIの直近の返答（冒頭）:\n" + st.get("reply", ""))
    # The child must not run these hooks again or rename this pane's workspace,
    # and carries no system prompt, tools, settings, MCP servers or thinking, so
    # a call costs a few hundred input tokens rather than tens of thousands.
    # It runs only on the claude.ai subscription login: every variable that
    # would switch it to an API key, another endpoint or a cloud provider is
    # dropped, and without that login there is no call at all.
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(("HERDR_", "ANTHROPIC_", "CLAUDE_CODE_USE_"))}
    env["MAX_THINKING_TOKENS"] = "0"
    status = subprocess.run([exe, "auth", "status"], capture_output=True, text=True,
                            env=env, timeout=30, stdin=subprocess.DEVNULL)
    auth = json.loads(status.stdout) if status.returncode == 0 else {}
    if not auth.get("loggedIn") or auth.get("authMethod") != "claude.ai" \
            or auth.get("apiProvider") != "firstParty":
        return None
    prompt = PROMPT + context
    for _ in range(2):
        done = subprocess.run(
            [exe, "-p", "--model", "haiku", "--no-session-persistence",
             "--settings", '{"disableAllHooks":true,"alwaysThinkingEnabled":false}',
             "--setting-sources", "", "--strict-mcp-config", "--tools", "",
             "--system-prompt", "指示どおりの名前だけを1行で出力する。", prompt],
            capture_output=True, text=True, env=env, timeout=120, stdin=subprocess.DEVNULL)
        lines = done.stdout.strip().splitlines()
        name = flat(lines[-1]).strip("「」\"'`*. ") if done.returncode == 0 and lines else ""
        if 0 < len(name) <= MAX_NAME:
            return name
        prompt = PROMPT + context + f"\n\n「{name}」は長すぎます。{MAX_NAME}文字以内で。"
    return None


def herdr(*args):
    exe = shutil.which("herdr")
    if not exe:
        return None
    done = subprocess.run([exe, *args], capture_output=True, text=True, timeout=5)
    return json.loads(done.stdout).get("result") if done.returncode == 0 else None


def rename_workspace(name, session, hook):
    pane_id = os.environ.get("HERDR_PANE_ID")
    socket_path = os.environ.get("HERDR_SOCKET_PATH")
    if os.environ.get("HERDR_ENV") != "1" or not pane_id or not socket_path:
        return
    pane = (herdr("pane", "get", pane_id) or {}).get("pane") or {}
    workspace_id = pane.get("workspace_id")
    if not workspace_id:
        return
    label = ((herdr("workspace", "get", workspace_id) or {}).get("workspace") or {}).get("label")
    if label is None or label == name:
        return
    # Workspace IDs are scoped to one herdr server, so key the records by socket.
    path = STATE / f"workspaces-{hashlib.sha256(socket_path.encode()).hexdigest()[:16]}.json"
    records = load(path, {})
    record = records.get(workspace_id) or {}
    defaults = {Path(p).name for p in (pane.get("cwd"), hook.get("cwd")) if p}
    owned = record.get("session") == session and record.get("label") == label
    if label not in defaults and not owned:
        return
    if herdr("workspace", "rename", workspace_id, name) is not None:
        records[workspace_id] = {"session": session, "label": name}
        save(path, records)


def update(agent, hook, session):
    lock = STATE / "sessions" / f"{session}.lock"
    lock.parent.mkdir(parents=True, exist_ok=True)
    try:
        if time.time() - lock.stat().st_mtime < 600:
            return
        lock.unlink()
    except FileNotFoundError:
        pass
    try:
        os.close(os.open(lock, os.O_CREAT | os.O_EXCL))
    except FileExistsError:
        return
    try:
        path = session_file(session)
        st = load(path, {})
        if hook.get("transcript_path"):
            read_new(agent, hook["transcript_path"], st)
        if agent == "codex":
            st["own"] = codex_own_title(session) or st.get("own")
        custom = st.get("custom")
        if custom and custom != st.get("applied"):
            st["name"] = custom
        elif not st.get("name") or st.get("prompts", 0) - st.get("named_at", 0) >= NEW_PROMPTS:
            st["name"] = generate(st) or st.get("name") or st.get("own")
            st["named_at"] = st.get("prompts", 0)
        save(path, st)
        if st.get("name"):
            rename_workspace(st["name"], session, hook)
    finally:
        lock.unlink(missing_ok=True)


def main():
    mode, agent = (sys.argv[1:3] + ["", ""])[:2]
    raw = sys.stdin.read()
    hook = json.loads(raw or "{}")
    session = hook.get("session_id")
    if not session or hook.get("agent_id") or agent not in ("claude", "codex"):
        return
    if mode == "stop":
        child = subprocess.Popen([sys.executable, __file__, "update", agent],
                                 stdin=subprocess.PIPE, stdout=subprocess.DEVNULL,
                                 stderr=subprocess.DEVNULL, start_new_session=True)
        child.stdin.write(raw.encode())
        child.stdin.close()
    elif mode == "update":
        update(agent, hook, session)
    elif mode == "prompt" and agent == "claude":
        path = session_file(session)
        st = load(path, {})
        name = st.get("name")
        if name and name != st.get("applied") and name != st.get("custom"):
            st["applied"] = name
            save(path, st)
            print(json.dumps({"hookSpecificOutput": {
                "hookEventName": "UserPromptSubmit", "sessionTitle": name}}, ensure_ascii=False))


if __name__ == "__main__":
    try:
        main()
    except Exception:
        pass
