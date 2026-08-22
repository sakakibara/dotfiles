#!/usr/bin/env python3
"""commit-msg hook: hold the final message to the rules in commit_rules.

This runs whatever produced the commit - an agent, a bare terminal, an editor,
lazygit, a rebase - so it is the layer that does not depend on the harness.
Exit 1 -> git aborts the commit and keeps the message for another try."""
import os
import sys

sys.dont_write_bytecode = True   # the hook dir is read-only inside sandboxes
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import commit_rules


def main():
    if len(sys.argv) < 2:
        return 0
    try:
        with open(sys.argv[1]) as fh:
            raw = fh.read()
    except OSError:
        return 0

    text = commit_rules.strip_git_comments(raw)
    if not text.strip():
        return 0

    try:
        found = commit_rules.check(text)
    except Exception:
        # A rule that cannot run must not wedge every commit on the machine.
        return 0
    if not found:
        return 0

    print("commit rejected: the message does not match this repository's "
          "convention:", file=sys.stderr)
    for f in found:
        print(f"  - {f}", file=sys.stderr)
    print("  The message was kept; edit it and commit again.", file=sys.stderr)
    print("  Inspect the convention with: git log -10 --format='%B'",
          file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
