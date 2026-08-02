#!/usr/bin/env python3
"""PreToolUse(Bash) guard: hold a git commit to the style the target repository
already uses. Every rule is derived from that repository's own history, never
configured here, and a rule only applies where the history is overwhelmingly
consistent - a mixed history means the project has no convention to enforce.
Exit 2 -> model rewrites the message."""
import json
import os
import re
import shlex
import subprocess
import sys

SAMPLE = 200          # commits inspected
MIN_SAMPLE = 20       # below this a repository has no convention yet
STRONG = 0.9          # a rule applies only above this share
BODY_MAX = 0.25       # bodies rarer than this means subject-only

CONVENTIONAL = re.compile(r"^[a-z]+(\([^)]*\))?!?: .")


def commit_messages(cmd):
    """Subject, body and target directory for each `git commit` in the
    command, via a real shell-word parse rather than a regex over the raw
    string. The target directory follows `git -C <path>` and any `cd <path>`
    earlier in the command, so the style is judged against the repository
    the commit actually lands in, not the session's working directory."""
    try:
        words = shlex.split(cmd)
    except ValueError:
        return []
    out, i, cwd = [], 0, None
    while i < len(words):
        if words[i] == "cd" and i + 1 < len(words) and not words[i + 1].startswith("-"):
            dest = os.path.expanduser(words[i + 1])
            cwd = dest if os.path.isabs(dest) else os.path.join(cwd or ".", dest)
            i += 2
            continue
        if words[i] != "git":
            i += 1
            continue
        j, cdir = i + 1, cwd
        while j < len(words) and words[j].startswith("-"):
            if words[j] == "-C" and j + 1 < len(words):
                dest = os.path.expanduser(words[j + 1])
                cdir = dest if os.path.isabs(dest) else os.path.join(cdir or ".", dest)
                j += 2
                continue
            if words[j] == "-c" and j + 1 < len(words):
                j += 2
                continue
            j += 1
        if j >= len(words) or words[j] != "commit":
            i += 1
            continue
        messages, k = [], j + 1
        while k < len(words):
            w = words[k]
            if w in ("git",) and messages:
                break
            if w in ("-m", "--message") and k + 1 < len(words):
                messages.append(words[k + 1])
                k += 2
                continue
            if w.startswith("--message="):
                messages.append(w.split("=", 1)[1])
            elif re.fullmatch(r"-m.+", w):
                messages.append(w[2:])
            k += 1
        if messages:
            subject, rest = messages[0], messages[1:]
            if "\n\n" in subject:
                subject, tail = subject.split("\n\n", 1)
                rest = [tail] + rest
            out.append((subject.strip(), [r for r in rest if r.strip()], cdir))
        i = k if k > i else i + 1
    return out


def history(cdir=None):
    try:
        raw = subprocess.run(
            ["git"] + (["-C", cdir] if cdir else [])
            + ["log", f"-n{SAMPLE}", "--format=%s%x00%b%x1e"],
            capture_output=True, text=True, timeout=10, check=True).stdout
    except Exception:
        return []
    entries = []
    for chunk in raw.split("\x1e"):
        if not chunk.strip("\n"):
            continue
        subject, _, body = chunk.lstrip("\n").partition("\x00")
        entries.append((subject, body.strip()))
    return entries


def share(entries, predicate):
    return sum(1 for e in entries if predicate(e)) / len(entries)


def violations(subject, body, entries):
    n = len(entries)
    found = []

    body_share = share(entries, lambda e: bool(e[1]))
    if body and body_share < BODY_MAX:
        found.append(
            f"this repository writes subject-only messages "
            f"({round(body_share * n)} of the last {n} commits have a body); "
            f"drop the body and say it in one subject line")

    conv = share(entries, lambda e: bool(CONVENTIONAL.match(e[0])))
    if conv >= STRONG and not CONVENTIONAL.match(subject):
        found.append(
            f"this repository uses conventional-commit prefixes "
            f"({round(conv * 100)}% of the last {n}); expected something like "
            f"\"feat: {subject[:40]}\"")
    elif conv <= 1 - STRONG and CONVENTIONAL.match(subject):
        found.append(
            f"this repository does not use conventional-commit prefixes "
            f"({round(conv * 100)}% of the last {n}); drop the \"type:\" prefix")

    dot = share(entries, lambda e: e[0].endswith("."))
    if dot <= 1 - STRONG and subject.endswith("."):
        found.append("this repository does not end subjects with a period")

    if conv <= 1 - STRONG and subject[:1].isalpha():
        upper = share(entries, lambda e: e[0][:1].isupper())
        if upper >= STRONG and not subject[:1].isupper():
            found.append("this repository capitalizes the subject line")
        elif upper <= 1 - STRONG and subject[:1].isupper():
            found.append("this repository writes subjects in lower case")

    return found


def main():
    try:
        cmd = json.load(sys.stdin).get("tool_input", {}).get("command", "")
    except Exception:
        return 0
    if not cmd or not re.search(r"(^|[;&|(]|&&|\|\|)\s*git\b[^;&|]*\bcommit\b", cmd):
        return 0
    proposed = commit_messages(cmd)
    if not proposed:
        return 0
    histories = {}
    for subject, body, cdir in proposed:
        if cdir not in histories:
            histories[cdir] = history(cdir)
        entries = histories[cdir]
        if len(entries) < MIN_SAMPLE:
            continue
        found = violations(subject, body, entries)
        if found:
            print("BLOCKED: commit message does not match this repository's "
                  "convention:", file=sys.stderr)
            for f in found:
                print(f"  - {f}", file=sys.stderr)
            print("  Inspect it with: git log --format='%s%x09%b' -n 20",
                  file=sys.stderr)
            return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
