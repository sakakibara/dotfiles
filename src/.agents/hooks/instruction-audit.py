#!/usr/bin/env python3
import argparse
import json
import os
import re
import sys

sys.dont_write_bytecode = True
import unicodedata
from pathlib import Path


INSTRUCTION_NAMES = {
    "AGENT.md",
    "AGENTS.md",
    "CLAUDE.md",
    "GEMINI.md",
    "SKILL.md",
    ".cursorrules",
    ".windsurfrules",
}
OPAQUE_RE = re.compile(r"(?<![A-Za-z0-9+/=])[A-Za-z0-9+/]{96,}={0,2}(?![A-Za-z0-9+/=])|(?<![0-9A-Fa-f])[0-9A-Fa-f]{128,}(?![0-9A-Fa-f])")
HIDDEN_RE = re.compile(r"<!--|<details\b|display\s*:\s*none|visibility\s*:\s*hidden", re.IGNORECASE)
PRUNED_DIRS = {".git", ".cache", ".next", ".venv", "build", "dist", "node_modules", "target", "vendor"}
AGENT_CONFIG_DIRS = {".claude", ".codex", ".cursor", ".windsurf"}
INSTRUCTION_NAMES_UPPER = {n.upper() for n in INSTRUCTION_NAMES}


def under_agent_config(rel):
    """True for a path inside an agent config directory, the directory
    itself included: a symlink anywhere in there redirects what the agent
    loads."""
    return len(rel.parts) > 0 and rel.parts[0] in AGENT_CONFIG_DIRS


def inside(path, root):
    try:
        path.relative_to(root)
        return True
    except ValueError:
        return False


def display_path(path):
    return json.dumps(os.fspath(path), ensure_ascii=True)


def instruction_path(rel):
    parts = rel.parts
    if rel.name.upper() in INSTRUCTION_NAMES_UPPER:
        return True
    value = rel.as_posix()
    if value == ".github/copilot-instructions.md":
        return True
    if value.startswith(".github/instructions/") and value.endswith(".instructions.md"):
        return True
    if value in {".mcp.json", ".claude/settings.json", ".claude/settings.local.json", ".codex/config.toml", ".config/opencode/opencode.json", ".config/opencode/opencode.jsonc", ".cursor/mcp.json", ".vscode/mcp.json"}:
        return True
    for d in (".claude/agents/", ".claude/commands/"):
        if value.startswith(d) and rel.suffix.lower() == ".md":
            return True
    if ".cursor" in parts or ".windsurf" in parts:
        return rel.suffix.lower() in {".md", ".mdc"}
    return False


MAX_ENTRIES = 20000


LOCAL_SETTINGS = Path(".claude/settings.local.json")


def mirrored_agent_config(rel):
    """True for an agent's own config file kept under a home-mirroring
    `src/` tree: the same file the audit would discover at its live path."""
    parts = rel.parts
    return len(parts) > 1 and parts[0] == "src" and instruction_path(Path(*parts[1:]))


class ListingTruncated(Exception):
    """The working tree holds more entries than the audit will read. Refusing
    is the only honest outcome: a partial audit that passes certifies nothing."""


def tracked_paths(root):
    """Every path in the working tree under root, or None when root is not
    inside a repository. git lists the tree with the build directories
    excluded before a byte of them is read, which is what keeps a large
    checkout under the entry cap; untracked and ignored files are included
    on purpose, since a hostile instruction file an agent just wrote is
    untracked at that moment and one that is gitignored never becomes
    tracked at all."""
    import subprocess
    pathspec = ["."] + [f":(exclude,glob)**/{d}/**" for d in sorted(PRUNED_DIRS)]

    def listing(at):
        out = subprocess.run(
            ["git", "-C", os.fspath(at), "ls-files", "-z", "--cached", "--others", "--", *pathspec],
            capture_output=True, timeout=10, check=True).stdout
        return [p.decode("utf-8", "surrogateescape") for p in out.split(b"\0") if p]

    try:
        pending = [(root, "")]
        found = []
        while pending:
            at, prefix = pending.pop()
            for rel in listing(at):
                full = prefix + rel
                path = root / full
                if rel.endswith("/") or (not path.is_symlink() and path.joinpath(".git").exists()):
                    pending.append((path, full.rstrip("/") + "/"))
                    continue
                found.append(full)
                if len(found) > MAX_ENTRIES:
                    raise ListingTruncated(len(found))
    except ListingTruncated:
        raise
    except Exception:
        return None
    return [Path(p) for p in found]


def discover(root):
    found = []
    tracked = tracked_paths(root)
    if tracked is not None:
        for rel in tracked:
            path = root / rel
            if any(part in PRUNED_DIRS for part in rel.parts):
                continue
            if not path.exists() and not path.is_symlink():
                continue
            if instruction_path(rel) or mirrored_agent_config(rel):
                found.append(rel)
            elif path.is_symlink() and under_agent_config(rel):
                found.append(rel)
        return sorted(set(found), key=lambda item: item.as_posix())

    entries = 0
    for current, dirs, files in os.walk(root, followlinks=False):
        base = Path(current)
        dirs[:] = [name for name in dirs if name not in PRUNED_DIRS]
        entries += len(files) + len(dirs)
        if entries > MAX_ENTRIES:
            raise ListingTruncated(entries)
        for name in files:
            path = base / name
            rel = path.relative_to(root)
            if instruction_path(rel) or mirrored_agent_config(rel):
                found.append(rel)
        for name in dirs:
            path = base / name
            if path.is_symlink() and under_agent_config(path.relative_to(root)):
                found.append(path.relative_to(root))
            elif path.is_symlink():
                rel = path.relative_to(root)
                if instruction_path(rel) or mirrored_agent_config(rel):
                    found.append(rel)
    return sorted(set(found), key=lambda item: item.as_posix())


def inspect_file(path, rel, root, content_rules, errors, warnings):
    shown = display_path(rel.as_posix())
    if path.is_symlink():
        target = path.resolve(strict=False)
        if not inside(target, root):
            errors.append(f"{shown}: instruction symlink resolves outside repository")
            return
        path = target
    try:
        raw = path.read_bytes()
    except OSError as exc:
        errors.append(f"{shown}: cannot read: {exc}")
        return
    try:
        text = raw.decode("utf-8")
    except UnicodeDecodeError as exc:
        errors.append(f"{shown}: invalid UTF-8: {exc}")
        return
    for index, char in enumerate(text):
        category = unicodedata.category(char)
        if category == "Cf" or category == "Cc" and char not in "\n\r\t":
            errors.append(f"{shown}: dangerous Unicode control U+{ord(char):04X} at character {index + 1}")
    normalized = unicodedata.normalize("NFKC", text)
    if normalized != text:
        warnings.append(f"{shown}: Unicode compatibility normalization changes content")
    if content_rules and HIDDEN_RE.search(text):
        errors.append(f"{shown}: hidden or collapsed content marker found")
    if content_rules and OPAQUE_RE.search(text):
        warnings.append(f"{shown}: long opaque base64/hex-like payload found; inspect without executing")


def load_policy(root, path):
    policy_path = (root / path).resolve()
    if not inside(policy_path, root):
        raise ValueError("policy path resolves outside repository")
    with policy_path.open(encoding="utf-8") as handle:
        return json.load(handle)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".")
    parser.add_argument("--policy")
    parser.add_argument("--strict-locations", action="store_true")
    parser.add_argument("--quiet", action="store_true")
    args = parser.parse_args()

    root = Path(args.root).resolve()
    if not root.is_dir():
        print(f"ERROR: not a directory: {root}", file=sys.stderr)
        return 2

    errors = []
    warnings = []
    try:
        discovered = discover(root)
    except ListingTruncated as exc:
        print(f"ERROR: working tree has {exc.args[0]} entries, more than the {MAX_ENTRIES} this audit reads; add its build directories to the pruned set or audit a subdirectory", file=sys.stderr)
        return 1
    files = set(discovered)
    allowed = None
    for rel in discovered:
        if (root / rel).is_symlink() and (root / rel).is_dir() and under_agent_config(rel):
            errors.append(f"{display_path(rel)}: symlinked agent configuration directory")

    if args.policy:
        try:
            policy = load_policy(root, Path(args.policy))
        except (OSError, ValueError, json.JSONDecodeError) as exc:
            print(f"ERROR: policy: {exc}", file=sys.stderr)
            return 2
        allowed = {Path(value) for value in policy.get("allowed_instruction_files", [])}
        files.update(Path(value) for value in policy.get("audited_files", []))
        canonical = Path(policy["canonical_policy"])
        if canonical not in allowed:
            errors.append(f"{display_path(canonical)}: canonical policy is not allowlisted")
        attributes = (root / ".mox" / "attributes.toml")
        attributes_text = attributes.read_text(encoding="utf-8") if attributes.exists() else ""
        def planted_as_symlink(rel):
            key = rel[len("src/"):] if rel.startswith("src/") else rel
            section = re.search(r'^\["' + re.escape(key) + r'"\]\n((?:(?!\[).*\n?)*)', attributes_text, re.M)
            return bool(section and re.search(r'^symlink\s*=\s*true', section.group(1), re.M))
        for rel, target in list(policy.get("discovery_links", {}).items()) + list(policy.get("symlinks", {}).items()):
            if not planted_as_symlink(rel):
                errors.append(f"{display_path(rel)}: not planted as a symlink in .mox/attributes.toml")
        for rel, target in policy.get("discovery_links", {}).items():
            link = root / rel
            try:
                actual = link.read_text(encoding="utf-8")
            except OSError as exc:
                errors.append(f"{display_path(rel)}: cannot read discovery declaration: {exc}")
                continue
            if actual != target:
                errors.append(f"{display_path(rel)}: expected discovery target {target!r}, found {actual!r}")
            resolved = (link.parent / target).resolve()
            if resolved != (root / canonical).resolve():
                errors.append(f"{display_path(rel)}: discovery target does not resolve to canonical policy")
        for rel, target in policy.get("symlinks", {}).items():
            link = root / rel
            try:
                actual = link.read_text(encoding="utf-8").strip()
            except OSError as exc:
                errors.append(f"{display_path(rel)}: cannot read link declaration: {exc}")
                continue
            if actual != target:
                errors.append(f"{display_path(rel)}: expected link target {target!r}, found {actual!r}")
            elif not (link.parent / target).resolve().is_dir():
                errors.append(f"{display_path(rel)}: link target {target!r} is not a directory")
        unexpected = set(discovered) - allowed - {LOCAL_SETTINGS}
        missing = {rel for rel in allowed if not (root / rel).exists() and not (root / rel).is_symlink()}
        errors.extend(f"{display_path(rel)}: unexpected instruction-file location" for rel in sorted(unexpected, key=str))
        errors.extend(f"{display_path(rel)}: allowlisted instruction file not found" for rel in sorted(missing, key=str))
    elif args.strict_locations:
        for rel in discovered:
            value = rel.as_posix()
            if rel.parent != Path(".") and rel != LOCAL_SETTINGS and value != ".github/copilot-instructions.md" and not value.startswith((".cursor/", ".windsurf/")):
                errors.append(f"{display_path(rel)}: unexpected nested instruction-file location")
    else:
        for rel in discovered:
            if rel.name in {"AGENT.md", "AGENTS.md", "CLAUDE.md", "GEMINI.md"} and rel.parent != Path("."):
                warnings.append(f"{display_path(rel)}: nested instruction scope discovered; review precedence")

    for rel in sorted(files, key=lambda item: item.as_posix()):
        path = root / rel
        if not path.exists() and not path.is_symlink():
            errors.append(f"{display_path(rel)}: audited file not found")
            continue
        if path.is_dir():
            continue
        inspect_file(path, rel, root, rel in set(discovered) or allowed is not None and rel in allowed or mirrored_agent_config(rel), errors, warnings)

    for message in warnings:
        print(f"WARNING: {message}")
    for message in errors:
        print(f"ERROR: {message}", file=sys.stderr)
    if errors:
        return 1
    if not args.quiet:
        print(f"agent instruction audit passed ({len(files)} files, {len(warnings)} warning(s))")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
