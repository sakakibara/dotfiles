#!/usr/bin/env python3
"""PreToolUse(Bash) guard: hold a git commit to the rules in commit_rules.

This is the early net - it reports before the command runs, so the message can
be rewritten rather than rejected afterwards. The commit-msg hook is the
authoritative layer; it sees every commit whatever produced it.

Message sources understood here: -m, -F <file>, -F - with a heredoc, and the
message a --amend or -C/-c would reuse. A piped message cannot be read, so it is
refused rather than waved through. Commit aliases are derived from the
repository's own configuration, never assumed.
Exit 2 -> model rewrites the message."""
import json
import os
import re
import shlex
import subprocess
import sys

sys.dont_write_bytecode = True   # the hook dir is read-only inside sandboxes
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import commit_rules

HEREDOC = re.compile(r"<<-?\s*(['\"]?)([A-Za-z_][A-Za-z0-9_]*)\1")
UNVERIFIABLE = "\x00unverifiable\x00"


def commit_aliases(cdir=None):
    """{alias: extra tokens it supplies after `commit`} for every alias that
    resolves to a commit, read from the repository's own configuration. Chained
    aliases are followed. A shell alias is recognised but supplies no tokens,
    since its arguments cannot be placed reliably."""
    try:
        raw = subprocess.run(
            ["git"] + (["-C", cdir] if cdir else [])
            + ["config", "--get-regexp", r"^alias\."],
            capture_output=True, text=True, timeout=10).stdout
    except Exception:
        return {}

    defined = {}
    for line in raw.splitlines():
        key, _, value = line.partition(" ")
        if key.startswith("alias."):
            defined[key[len("alias."):]] = value.strip()

    resolved, changed = {}, True
    while changed:
        changed = False
        for name, value in defined.items():
            if name in resolved:
                continue
            if value.startswith("!"):
                if re.search(r"\bgit\b[^;&|\n]*\bcommit\b", value):
                    resolved[name], changed = [], True
                continue
            try:
                parts = shlex.split(value)
            except ValueError:
                continue
            if not parts:
                continue
            if parts[0] == "commit":
                resolved[name], changed = parts[1:], True
            elif parts[0] in resolved:
                resolved[name], changed = resolved[parts[0]] + parts[1:], True
    return resolved


def split_heredocs(cmd):
    """The command with heredoc bodies lifted out, plus those bodies in the
    order they were opened. Lifting them first keeps the shell-word parse from
    tripping over message text, and `-F -` reads its message back."""
    lines, out, bodies, i = cmd.split("\n"), [], [], 0
    while i < len(lines):
        line = lines[i]
        out.append(line)
        opened = [(m.group(2), line[m.start():m.start() + 3].startswith("<<-"))
                  for m in HEREDOC.finditer(line)]
        i += 1
        for delim, dash in opened:
            body = []
            while i < len(lines):
                probe = lines[i].lstrip("\t") if dash else lines[i]
                if probe == delim:
                    i += 1
                    break
                body.append(lines[i])
                i += 1
            bodies.append("\n".join(body))
    return "\n".join(out), bodies


def message_from(target, heredocs):
    """Message behind `-F <target>`: the first heredoc for `-`, else the file's
    contents. A pipe leaves nothing to read."""
    if target == "-":
        return heredocs[0] if heredocs else UNVERIFIABLE
    try:
        with open(os.path.expanduser(target)) as fh:
            return fh.read()
    except OSError:
        return ""


def message_from_rev(rev, cdir):
    """The message `--amend`, `-C` or `-c` would reuse."""
    try:
        return subprocess.run(
            ["git"] + (["-C", cdir] if cdir else [])
            + ["log", "-1", "--format=%B", rev],
            capture_output=True, text=True, timeout=10, check=True).stdout.strip()
    except Exception:
        return ""


def scan_args(argv, heredocs, cdir):
    """Message and bypass flag from the arguments following `commit`."""
    messages, k, amend, reuse, no_verify = [], 0, False, None, False
    while k < len(argv):
        w = argv[k]
        if w == "git" and messages:
            break
        if w in ("--no-verify", "-n"):
            no_verify = True
            k += 1
            continue
        if w == "--amend":
            amend = True
            k += 1
            continue
        if w in ("-C", "--reuse-message", "-c", "--reedit-message") and k + 1 < len(argv):
            reuse = argv[k + 1]
            k += 2
            continue
        if w.startswith(("--reuse-message=", "--reedit-message=")):
            reuse = w.split("=", 1)[1]
            k += 1
            continue
        if w in ("-m", "--message") and k + 1 < len(argv):
            messages.append(argv[k + 1])
            k += 2
            continue
        if w in ("-F", "--file") and k + 1 < len(argv):
            messages.append(message_from(argv[k + 1], heredocs))
            k += 2
            continue
        if w.startswith("--file="):
            messages.append(message_from(w.split("=", 1)[1], heredocs))
        elif re.fullmatch(r"-F.+", w):
            messages.append(message_from(w[2:], heredocs))
        elif w.startswith("--message="):
            messages.append(w.split("=", 1)[1])
        elif re.fullmatch(r"-m.+", w):
            messages.append(w[2:])
        k += 1

    messages = [m for m in messages if m.strip()]
    if not messages and (reuse or amend):
        messages = [m for m in [message_from_rev(reuse or "HEAD", cdir)] if m.strip()]
    return ("\n\n".join(messages) if messages else ""), no_verify


def commit_messages(cmd, aliases):
    """Message, target directory and bypass flag for each commit in the command.
    The target directory follows `git -C <path>` and any `cd <path>` earlier in
    the command, so the message is judged against the repository the commit
    actually lands in, not the session's working directory."""
    cmd, heredocs = split_heredocs(cmd)
    try:
        words = shlex.split(cmd)
    except ValueError:
        return []
    verbs = {"commit": []}
    verbs.update(aliases)

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
        if j >= len(words) or words[j] not in verbs:
            i += 1
            continue
        message, no_verify = scan_args(verbs[words[j]] + words[j + 1:], heredocs, cdir)
        if message or no_verify:
            out.append((message, cdir, no_verify))
        i = j + 1
    return out


def main():
    try:
        cmd = json.load(sys.stdin).get("tool_input", {}).get("command", "")
    except Exception:
        return 0
    if not cmd:
        return 0
    aliases = commit_aliases()
    verbs = "|".join(re.escape(v) for v in ["commit", *aliases])
    if not re.search(rf"\bgit\b[^;&|\n]*\b({verbs})\b", cmd):
        return 0

    for message, cdir, no_verify in commit_messages(cmd, aliases):
        if no_verify:
            print("BLOCKED: --no-verify skips the commit-msg hook, which is the "
                  "machine-wide message check. Commit without it.", file=sys.stderr)
            return 2
        if message == UNVERIFIABLE:
            print("BLOCKED: the commit message is piped in, so it cannot be "
                  "checked. Pass it with -m or a heredoc instead.", file=sys.stderr)
            return 2
        found = commit_rules.check(message, cdir)
        if found:
            print("BLOCKED: commit message does not match this repository's "
                  "convention:", file=sys.stderr)
            for f in found:
                print(f"  - {f}", file=sys.stderr)
            print("  Inspect it with: git log -10 --format='%B'", file=sys.stderr)
            return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
