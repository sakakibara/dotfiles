#!/usr/bin/env python3
"""PreToolUse(Bash) guard: block blanket git staging and .env staging.

A tripwire, not a boundary: it reads the command text the agent is about to
run and refuses the forms an agent reaches for by habit. It cannot see into
`sh -c` and `bash -c` payloads, xargs, git aliases, or a command assembled
from variables or substitutions, so a determined command can still stage
everything. It is also a Bash hook, so a script written with another tool and
then executed is invisible to it whatever it contains. The commit-msg hook
and review remain the layers that see the result.

Blocked, per command segment (split on ; && & || | and newline). The staging verb
is read as either of its two spellings, and a long option is matched by
prefix because git accepts any unambiguous abbreviation:
  a blanket staging flag, alone or inside a cluster
  a blanket pathspec: any run of dots and slashes (. ./ .. ../ ./. ../..),
    * ** *.* :/ : :(magic)<blanket body> :!... $PWD ~+, before or after --
  a pathspec taken from a file
  a commit that stages beyond the index on its way past (-a, -i, -p and
    their long spellings)
  a removal over a blanket pathspec (git rm, with or without --cached)
  a stash push or save with no pathspec (stashes every change)
  staging a .env* file, whatever its case; the conventional committed
    templates (.example, .sample, .template, .dist) are not blocked
Leading shell keywords (`if`, `then`, `elif`, `else`, `while`, `until`, `do`,
`case`, `in`, `!`, `{`), a subshell's own parentheses, wrappers with their
options (`env`, `command`, `exec`, `sudo`, `doas`, `eval`, `nice`, `time`,
`timeout`, `stdbuf`, `setsid`, `script`, `nohup`, `\\git`, an absolute git
path), `git -C dir`, `git -c key=val` and VAR=value
assignments are stripped before matching. Segments split on ; && & || | and
newline.
Exit 2 -> the model picks explicit paths."""
import json
import os
import re
import shlex
import sys

sys.dont_write_bytecode = True

SEGMENT = re.compile(r"\s*(?:;|&&|&|\|\||\||\n)\s*")
BLANKET_PATHSPECS = {".", "./", "..", "../", "*", "./*", ":/", ":/.", ":/*", ":", "**", "*.*",
                     "$PWD", "$PWD/", "${PWD}", "${PWD}/", "~+", "~+/"}
# Long options git resolves from any unambiguous prefix.
STAGE_LONG = ("--all", "--update", "--no-ignore-removal", "--pathspec-from-file")
COMMIT_LONG = ("--all", "--include", "--interactive", "--patch")
WRAPPERS = ("env", "command", "sudo", "doas", "exec", "eval", "nice", "time",
            "timeout", "stdbuf", "setsid", "script", "nohup",
            "if", "while", "until", "case", "in",
            "then", "do", "else", "elif", "!", "{")
# Wrapper options that consume the word after them, so the scan does not
# mistake a username or a duration for the command being wrapped.
WRAPPER_VALUE_OPTS = {
    "sudo": {"-u", "-g", "-h", "-p", "-r", "-t", "-U", "-C", "-D", "-T"},
    "doas": {"-u", "-C"},
    "env": {"-u", "-C", "-S"},
    "nice": {"-n"},
    "timeout": {"-s", "-k"},
    "stdbuf": {"-o", "-e", "-i"},
    "script": {"-c", "-t"},
}
STAGE_VERBS = ("add", "stage")


def words_of(segment):
    """The segment's words, with grouping parentheses and glued
    redirections split off as their own tokens: `(cd x && git add -A)`
    arrives as `git add -A)`, a case label as `all)`, and `-A>/dev/null` as
    `-A` then `>/dev/null`. A pathspec's own parentheses (`:(top)`) stay."""
    try:
        raw = shlex.split(segment.strip())
    except ValueError:
        raw = segment.split()
    words = []
    for w in raw:
        lead = len(w) - len(w.lstrip("("))
        words.extend("(" * lead)
        w = w[lead:]
        trail = 0
        if not w.startswith(":"):
            trail = len(w) - len(w.rstrip(")"))
            w = w[:len(w) - trail] if trail else w
        glued = re.match(r"(.*?)(\d*(?:>>|>\||&>|>|<).*)", w)
        if glued and glued.group(1):
            words.append(glued.group(1))
            w = glued.group(2)
        if w:
            words.append(w)
        words.extend(")" * trail)
    return words


def strip_prefix(words):
    """The words of a git invocation starting at `git`, with wrappers and
    git's own global options removed; None when this is not git."""
    i = 0
    saw_wrapper = False
    while i < len(words):
        w = words[i].lstrip("\\")
        if w == "case":
            i += 3
            saw_wrapper = True
            continue
        if w in ("(", ")") or (i + 1 < len(words) and words[i + 1] == ")"):
            i += 1
            saw_wrapper = True
            continue
        if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*=.*", w):
            i += 1
            continue
        redirect = re.fullmatch(r"\d*(?:>>|>\||&>|>|<)(.*)", w)
        if redirect:
            i += 1 if redirect.group(1) else 2
            continue
        if w in WRAPPERS:
            saw_wrapper = True
            i += 1
            while i < len(words) and (words[i].startswith("-") or re.fullmatch(r"\d+[smhd]?", words[i])):
                takes_value = words[i] in WRAPPER_VALUE_OPTS.get(w, set())
                i += 1
                if takes_value and i < len(words):
                    i += 1
            continue
        break
    if i >= len(words):
        return None
    if saw_wrapper and words[i].lstrip("\\") != "git":
        for j in range(i, min(i + 2, len(words))):
            if words[j].lstrip("\\") == "git":
                i = j
                break
    w = words[i].lstrip("\\")
    if w != "git" and not re.fullmatch(r".*/git", w):
        return None
    i += 1
    while i < len(words) and words[i].startswith("-"):
        if words[i] in ("-C", "-c", "--git-dir", "--work-tree", "--namespace") and i + 1 < len(words):
            i += 2
            continue
        i += 1
    return words[i:]


def is_long_prefix(arg, options):
    """True when arg names one of options, or an unambiguous abbreviation."""
    name = arg.split("=", 1)[0]
    if len(name) <= 2:
        return False
    return any(opt.startswith(name) for opt in options)


def has_stage_flag(args):
    for a in args:
        if a == "--":
            return False
        if re.fullmatch(r"-[a-zA-Z]+", a) and ("A" in a[1:] or "u" in a[1:]):
            return True
        if a.startswith("--") and is_long_prefix(a, STAGE_LONG):
            return True
    return False


def names_cwd_or_parent(arg):
    """An absolute or ~ path that is the working directory or contains it,
    which stages the whole tree as surely as `.` does."""
    if not arg.startswith(("/", "~")):
        return False
    target = os.path.realpath(os.path.expanduser(arg))
    here = os.path.realpath(os.getcwd())
    return here == target or here.startswith(target.rstrip("/") + "/")


def pathspec_body(arg):
    """(excludes, body) for a pathspec: `:!x`, `:^x` and `:(exclude)x` select
    everything else; other magic (`:(glob)`, `:(top)`, `:/`) wraps a body
    judged like a plain path."""
    if arg.startswith((":!", ":^")):
        return True, arg[2:]
    if arg.startswith(":("):
        magic, _, body = arg[2:].partition(")")
        return "exclude" in magic.split(","), body
    if arg.startswith(":/"):
        return False, arg[2:]
    if arg == ":":
        return False, ""
    return False, arg


def blanket_body(body):
    """Whether a pathspec body names the whole tree: the directory itself, a
    parent, a bare wildcard, or an absolute path at or above the cwd."""
    if body == "" or body in BLANKET_PATHSPECS or body.rstrip("/.") in BLANKET_PATHSPECS:
        return True
    if re.fullmatch(r"[./]+", body) or re.fullmatch(r"\*+|\*\*/\*|\*\.\*|\./\*", body.rstrip("/")):
        return True
    norm = os.path.normpath(body)
    if norm in (".", "..") or re.fullmatch(r"\.\.(/\.\.)*", norm):
        return True
    return names_cwd_or_parent(body)


def blanket_pathspec(args):
    """The first pathspec that names the whole tree, or None. An exclusion
    selects everything else only when it is the sole pathspec; next to
    explicit paths it merely narrows them."""
    specs = [a for a in args if a != "--" and not (a.startswith("-") and a != "-")]
    positives = [a for a in specs if not pathspec_body(a)[0]]
    for a in specs:
        excludes, body = pathspec_body(a)
        if excludes:
            if not positives:
                return a
            continue
        if blanket_body(body):
            return a
    return None


def env_target(args):
    for a in args:
        if a.startswith("-"):
            continue
        name = a.rstrip("/~").rsplit("/", 1)[-1]
        # A template beside the secret (`.env.example`, `.env.local.example`,
        # `.env.sample.md`) is not the secret.
        if re.fullmatch(r"\.env(\..+)?\.(example|sample|template|dist)(\..+)?", name, re.IGNORECASE):
            continue
        # `.env`, `.env.local`, `prod.env`, `database.env`, and direnv's `.envrc`;
        # not `my.env.md`, which only contains the word.
        if re.fullmatch(r"\.env(\..+)?|.+\.env|\.envrc", name, re.IGNORECASE):
            return a
    return None


def commit_cluster_stages(arg):
    """True when a bundled short-option word carries -a, -i or -p as an
    option. The letters after the first value-taking one (-m, -F, -C, -c,
    -t, -S, -u) are that option's value: `-unormal` is -u with `normal`."""
    if not re.fullmatch(r"-[a-zA-Z]{2,}", arg):
        return False
    for ch in arg[1:]:
        if ch in "mFCctSu":
            return False
        if ch in "aip":
            return True
    return False


def check_segment(segment):
    args = strip_prefix(words_of(segment))
    if not args:
        return None
    verb, rest = args[0], args[1:]
    if verb in STAGE_VERBS:
        if any(is_long_prefix(a, ("--dry-run",)) or re.fullmatch(r"-[a-zA-Z]*n[a-zA-Z]*", a) for a in rest):
            return None
        prompted = any(a in ("-i", "--interactive", "-p", "--patch", "-e", "--edit") for a in rest) or any(
            re.fullmatch(r"-[a-zA-Z]+", a) and ("i" in a[1:] or "p" in a[1:]) for a in rest)
        if prompted and not [a for a in rest if not a.startswith("-") or a == "-"]:
            return f"'git {verb}' by prompt over the whole tree stages whatever is answered. Stage explicit paths: git add path/to/file"
        if has_stage_flag(rest):
            return f"blanket 'git {verb}' stages unrelated files (stray specs, plans, secrets). Stage explicit paths: git add path/to/file"
        blanket = blanket_pathspec(rest)
        if blanket:
            return f"'git {verb} {blanket}' stages everything. Stage explicit paths: git add path/to/file"
        target = env_target(rest)
        if target:
            return f"never stage {target} (secrets). Add it to .gitignore instead."
    elif verb == "rm":
        if not any(is_long_prefix(a, ("--dry-run",)) or re.fullmatch(r"-[a-zA-Z]*n[a-zA-Z]*", a) for a in rest):
            blanket = blanket_pathspec(rest)
            if blanket:
                return f"'git rm {blanket}' stages the removal of everything. Name the paths: git rm path/to/file"
    elif verb == "commit":
        if "--dry-run" in rest:
            return None
        skip_value = False
        for a in rest:
            if skip_value:
                skip_value = False
                continue
            if a == "--":
                break
            if a in ("-m", "-F", "-C", "-c", "--message", "--file", "--reuse-message", "--reedit-message", "-t", "--template"):
                skip_value = True
                continue
            if a in ("-a", "-i", "-p") or is_long_prefix(a, COMMIT_LONG) or commit_cluster_stages(a):
                return f"'git commit {a}' stages every tracked change. Stage explicit paths first: git add path/to/file"
    elif verb == "stash":
        if any(a in ("-h", "--help") for a in rest):
            return None
        sub = rest[0] if rest and not rest[0].startswith("-") else "push"
        args = rest[1:] if rest and rest[0] == sub else rest
        if sub == "save":
            return "'git stash save' sweeps every change, including files you have not reviewed. Name paths: git stash push -- path/to/file"
        if sub == "push":
            paths, skip = [], False
            for a in args:
                if skip:
                    skip = False
                    continue
                if a in ("-m", "--message"):
                    skip = True
                elif not a.startswith("-"):
                    paths.append(a)
            prompted = any(a in ("-p", "--patch") or re.fullmatch(r"-[a-zA-Z]*p[a-zA-Z]*", a) for a in args)
            if not paths or blanket_pathspec(paths) or prompted:
                return "'git stash' over the whole tree sweeps every change, including files you have not reviewed. Name paths: git stash push -- path/to/file"
    return None


def main():
    try:
        cmd = json.load(sys.stdin).get("tool_input", {}).get("command", "")
    except Exception:
        return 0
    if not cmd or "git" not in cmd:
        return 0
    cmd = cmd.replace("\\\n", " ")
    for segment in SEGMENT.split(cmd):
        reason = check_segment(segment)
        if reason:
            print(f"BLOCKED: {reason}", file=sys.stderr)
            return 2
    return 0


if __name__ == "__main__":
    sys.exit(main())
