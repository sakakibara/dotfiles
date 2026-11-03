#!/usr/bin/env python3
"""Stop hook: name the herdr workspace after the agent session's title.

Usage: herdr-workspace-label.py claude|codex, with the hook input on stdin.

The title is the one the agent generated for the session: Claude Code's
custom-title (set by /rename) or ai-title from the transcript, or Codex's
thread_name from session_index.jsonl. The workspace holding the agent's pane
is renamed only while its label is herdr's default (the directory name) or
the title this hook last set for the same session, so a name typed by hand,
or one another agent's session set, is kept.

Does nothing outside herdr, for subagents, or when anything fails; it never
blocks the agent.
"""
import hashlib
import json
import os
import shutil
import subprocess
import sys
from pathlib import Path


def claude_title(hook):
    path = hook.get("transcript_path")
    if not path:
        return None
    custom = ai = None
    with open(path, encoding="utf-8", errors="replace") as handle:
        for line in handle:
            if '"custom-title"' not in line and '"ai-title"' not in line:
                continue
            try:
                entry = json.loads(line)
            except ValueError:
                continue
            if entry.get("type") == "custom-title":
                custom = entry.get("customTitle") or custom
            elif entry.get("type") == "ai-title":
                ai = entry.get("aiTitle") or ai
    return custom or ai


def codex_title(hook):
    session = hook.get("session_id")
    if not session:
        return None
    home = Path(os.environ.get("CODEX_HOME") or Path.home() / ".codex")
    title = None
    with open(home / "session_index.jsonl", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            if session not in line:
                continue
            try:
                entry = json.loads(line)
            except ValueError:
                continue
            if entry.get("id") == session:
                title = entry.get("thread_name") or title
    return title


def herdr(*args):
    exe = shutil.which("herdr")
    if not exe:
        return None
    done = subprocess.run([exe, *args], capture_output=True, text=True, timeout=3)
    if done.returncode != 0:
        return None
    return json.loads(done.stdout).get("result")


def state_file():
    # Workspace IDs are scoped to one herdr server, so key the record by socket.
    socket_key = hashlib.sha256(os.environ["HERDR_SOCKET_PATH"].encode()).hexdigest()[:16]
    base = Path(os.environ.get("XDG_STATE_HOME") or Path.home() / ".local" / "state")
    return base / "herdr-workspace-label" / f"{socket_key}.json"


def main():
    agent = sys.argv[1] if len(sys.argv) > 1 else ""
    hook = json.loads(sys.stdin.read() or "{}")
    if os.environ.get("HERDR_ENV") != "1" or hook.get("agent_id"):
        return
    pane_id = os.environ.get("HERDR_PANE_ID")
    session = hook.get("session_id")
    if not pane_id or not os.environ.get("HERDR_SOCKET_PATH") or not session:
        return

    title = {"claude": claude_title, "codex": codex_title}.get(agent, lambda _: None)(hook)
    title = " ".join((title or "").split())
    if not title:
        return

    pane = (herdr("pane", "get", pane_id) or {}).get("pane") or {}
    workspace_id = pane.get("workspace_id")
    if not workspace_id:
        return
    workspace = (herdr("workspace", "get", workspace_id) or {}).get("workspace") or {}
    label = workspace.get("label")
    if label is None or label == title:
        return

    path = state_file()
    try:
        records = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        records = {}
    record = records.get(workspace_id) or {}
    defaults = {Path(p).name for p in (pane.get("cwd"), hook.get("cwd")) if p}
    owned = record.get("session") == session and record.get("label") == label
    if label not in defaults and not owned:
        return

    if herdr("workspace", "rename", workspace_id, title) is None:
        return
    records[workspace_id] = {"session": session, "label": title}
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(".tmp")
    tmp.write_text(json.dumps(records, ensure_ascii=False), encoding="utf-8")
    tmp.replace(path)


if __name__ == "__main__":
    try:
        main()
    except Exception:
        pass
