"""Commit message rules, shared by the PreToolUse guard and the commit-msg hook.

Style rules are derived from the target repository's own history, never
configured here, and a rule applies only where that history is overwhelmingly
consistent - a mixed history means the project has no convention to enforce.

Three rules are absolute rather than derived. A subject is one line, because git
defines it that way and a wrapped shell string silently turns the overflow into
a body. A message carries no session-private label. And a message carries no
agent trailer, unless the repository opts back in through its own .claude
settings - the same file that enables it in the harness.
"""
import json
import os
import re
import subprocess

SAMPLE = 200          # commits inspected
MIN_SAMPLE = 20       # below this a repository has no convention yet
STRONG = 0.9          # a rule applies only above this share
BODY_MAX = 0.25       # bodies rarer than this means subject-only

CONVENTIONAL = re.compile(r"^[a-z]+(\([^)]*\))?!?: .")
AGENT_TRAILER = re.compile(
    r"^\s*(?:Claude-Session:|Co-Authored-By:\s*Claude\b|Assisted-By:\s*Claude\b)"
    r"|Generated with .{0,20}Claude Code",
    re.M | re.I)
SESSION_LABEL = re.compile(
    r"plan-[0-9]|phase-?[0-9]|sub-plan|\bPlan [A-Z][0-9]?\b|\bPhase [0-9]"
    r"|\bTask [0-9]+\b|\bSpec [A-Z][0-9]?\b|\bMVP\b",
    re.I)
# ASCII punctuation only. Restricted to substitutes for ASCII forms, so a
# subject written in another script is untouched.
NON_ASCII_PUNCT = {
    "\u2014": "em-dash",  "\u2013": "en-dash",   "\u2026": "ellipsis",
    "\u2018": "smart quote", "\u2019": "smart quote",
    "\u201c": "smart quote", "\u201d": "smart quote",
    "\u2192": "arrow", "\u2190": "arrow", "\u2191": "arrow", "\u2193": "arrow",
    "\u2194": "arrow", "\u21aa": "arrow", "\u21d2": "arrow", "\u279c": "arrow",
    "\u27f6": "arrow",
    "\u2015": "horizontal bar", "\u2212": "minus sign", "\uff0d": "fullwidth hyphen",
    "\u2025": "two-dot leader", "\u2022": "bullet", "\u00d7": "multiplication sign",
    "\u00a0": "non-breaking space", "\ufeff": "byte-order mark",
    "\ufffd": "replacement character", "\u00ad": "soft hyphen",
    "\u200b": "zero-width space", "\u200c": "zero-width space", "\u200d": "zero-width space",
    "\u2028": "line separator", "\u2029": "line separator",
    "\u2007": "non-breaking space", "\u2009": "thin space", "\u202f": "non-breaking space",
    "\u2010": "hyphen", "\u2011": "hyphen", "\u2012": "dash",
    "\u201a": "smart quote", "\u201e": "smart quote", "\u2032": "prime",
    "\u00ab": "guillemet", "\u00bb": "guillemet",
}
ASCII_FOR = {"em-dash": "--", "en-dash": "-", "ellipsis": "...",
             "smart quote": "\" or '", "arrow": "->",
             "horizontal bar": "--", "minus sign": "-", "fullwidth hyphen": "-",
             "two-dot leader": "..", "bullet": "-", "multiplication sign": "x",
             "non-breaking space": "a space", "byte-order mark": "nothing",
             "replacement character": "the intended character", "soft hyphen": "nothing",
             "zero-width space": "nothing", "line separator": "a newline", "thin space": " ",
             "hyphen": "-", "dash": "-", "prime": "'", "guillemet": "\"",
}
# Superlatives that are never a statement of behaviour. Words that are often
# literal - robust, secure, powerful - are deliberately left out: blocking them
# would reject honest subjects more often than promotional ones.
OVERCLAIM = re.compile(
    r"\b(fastest|blazing|bulletproof|production[- ]ready|world[- ]class"
    r"|seamless|performant)\b", re.I)

SCISSORS = re.compile(r"^#\s*-+\s*>8\s*-+", re.M)
# Messages git composes from history (merges, reverts, autosquash) are another
# author's text: only the trailer rule applies; the derived rules stand down.
GENERATED = re.compile(
    r"^(?:Merge (?:branch|branches|remote-tracking branch|remote-tracking branches|tag|tags|commit|pull request) "
    r"|Revert \"|Reapply \"|fixup! |squash! |amend! |Applying: |Rebasing )")


def strip_git_comments(text):
    """The message as git will keep it: comment lines gone, everything below a
    scissors line gone. The commit-msg hook sees the file before that cleanup."""
    cut = SCISSORS.search(text)
    if cut:
        text = text[:cut.start()]
    return "\n".join(l for l in text.split("\n") if not l.startswith("#"))


def split_message(text):
    """Subject and body paragraphs."""
    parts = text.strip().split("\n\n")
    subject = parts[0].strip()
    body = [p.strip() for p in parts[1:] if p.strip()]
    return subject, body


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


def repo_root(cdir=None):
    try:
        return subprocess.run(
            ["git"] + (["-C", cdir] if cdir else []) + ["rev-parse", "--show-toplevel"],
            capture_output=True, text=True, timeout=10, check=True).stdout.strip()
    except Exception:
        return ""


def agent_attribution_allowed(cdir=None):
    """Whether the target repository opts back into agent attribution - the
    session link, or attribution text of its own. Read from that repository's
    own untracked .claude/settings.local.json, the one file the instructions
    name for the opt-in, so a committed file cannot switch it on."""
    root = repo_root(cdir)
    if not root:
        return False
    tracked = subprocess.run(
        ["git", "-C", root, "ls-files", "--error-unmatch", "--", ".claude/settings.local.json"],
        capture_output=True, timeout=10)
    if tracked.returncode == 0:
        return False
    try:
        with open(os.path.join(root, ".claude", "settings.local.json")) as fh:
            attribution = json.load(fh).get("attribution", {})
    except Exception:
        return False
    if attribution.get("sessionUrl") is True:
        return True
    if isinstance(attribution.get("commit"), str) and attribution["commit"].strip():
        return True
    return False


def derived(subject, body, entries):
    """Rules read out of this repository's own history."""
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


def check(text, cdir=None):
    """Every rule, against one complete commit message. Empty list means clean."""
    subject, body = split_message(text)
    if not subject:
        return []
    generated = bool(GENERATED.match(subject))
    judged = "" if generated else text
    found = []

    if "\n" in subject:
        found.append(
            "the subject is broken across lines; git keeps the first line as "
            "the subject and turns the rest into a body. Write it as one line "
            "- a wrapped shell string leaks its newline and indent into the "
            "message")

    for ch, name in NON_ASCII_PUNCT.items():
        if ch in judged:
            found.append(
                f"the message uses a {name} ({ch!r}); write ASCII punctuation "
                f"instead ({ASCII_FOR[name]})")
            break

    over = OVERCLAIM.search(judged)
    if over:
        found.append(
            f"the message claims \"{over.group(0)}\" - state the concrete "
            f"behaviour that changed instead")

    hit = SESSION_LABEL.search(judged)
    if hit:
        found.append(
            f"the message carries the session-private label \"{hit.group(0)}\" "
            f"- meaningless in a public log. Describe what the commit does, "
            f"timelessly")

    allowed = agent_attribution_allowed(cdir)
    if AGENT_TRAILER.search(text):
        if not allowed:
            found.append(
                "a commit never carries an agent trailer (Claude-Session, "
                "Co-Authored-By: Claude, \"Generated with ... Claude Code\"); "
                "drop it. A repository that genuinely wants it opts in via its "
                "own .claude/settings.local.json: "
                "{\"attribution\": {\"sessionUrl\": true}}")
        else:
            body = [p for p in (
                "\n".join(l for l in part.splitlines()
                          if not AGENT_TRAILER.search(l)).strip()
                for part in body) if p]

    entries = history(cdir)
    if len(entries) >= MIN_SAMPLE and not generated:
        found += derived(subject, body, entries)
    return found
