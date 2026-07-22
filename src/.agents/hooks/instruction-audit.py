#!/usr/bin/env python3
import argparse
import json
import os
import re
import sys
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
TEXT_SUFFIXES = {".md", ".mdc", ".json", ".toml", ".yaml", ".yml", ".sh", ".py"}
OPAQUE_RE = re.compile(r"(?<![A-Za-z0-9+/=])[A-Za-z0-9+/]{96,}={0,2}(?![A-Za-z0-9+/=])|(?<![0-9A-Fa-f])[0-9A-Fa-f]{128,}(?![0-9A-Fa-f])")
HIDDEN_RE = re.compile(r"<!--|<details\b|display\s*:\s*none|visibility\s*:\s*hidden", re.IGNORECASE)


def inside(path, root):
    try:
        path.relative_to(root)
        return True
    except ValueError:
        return False


def instruction_path(rel):
    parts = rel.parts
    if rel.name in INSTRUCTION_NAMES:
        return True
    value = rel.as_posix()
    if value == ".github/copilot-instructions.md":
        return True
    if ".cursor" in parts or ".windsurf" in parts:
        return rel.suffix.lower() in {".md", ".mdc"}
    return False


def discover(root):
    found = []
    for current, dirs, files in os.walk(root, followlinks=False):
        dirs[:] = [name for name in dirs if name != ".git"]
        base = Path(current)
        for name in files:
            path = base / name
            rel = path.relative_to(root)
            if instruction_path(rel):
                found.append(rel)
        for name in dirs:
            path = base / name
            if path.is_symlink():
                rel = path.relative_to(root)
                if instruction_path(rel):
                    found.append(rel)
    return sorted(set(found), key=lambda item: item.as_posix())


def inspect_file(path, rel, root, content_rules, errors, warnings):
    if path.is_symlink():
        target = path.resolve(strict=False)
        if not inside(target, root):
            errors.append(f"{rel}: instruction symlink resolves outside repository: {target}")
            return
        path = target
    try:
        raw = path.read_bytes()
    except OSError as exc:
        errors.append(f"{rel}: cannot read: {exc}")
        return
    try:
        text = raw.decode("utf-8")
    except UnicodeDecodeError as exc:
        errors.append(f"{rel}: invalid UTF-8: {exc}")
        return
    for index, char in enumerate(text):
        category = unicodedata.category(char)
        if category == "Cf" or category == "Cc" and char not in "\n\r\t":
            errors.append(f"{rel}: dangerous Unicode control U+{ord(char):04X} at character {index + 1}")
    normalized = unicodedata.normalize("NFKC", text)
    if normalized != text:
        warnings.append(f"{rel}: Unicode compatibility normalization changes content")
    if content_rules and HIDDEN_RE.search(text):
        errors.append(f"{rel}: hidden or collapsed content marker found")
    if content_rules and OPAQUE_RE.search(text):
        warnings.append(f"{rel}: long opaque base64/hex-like payload found; inspect without executing")


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
    discovered = discover(root)
    files = set(discovered)
    allowed = None

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
            errors.append(f"{canonical}: canonical policy is not allowlisted")
        for rel, target in policy.get("discovery_links", {}).items():
            link = root / rel
            try:
                actual = link.read_text(encoding="utf-8")
            except OSError as exc:
                errors.append(f"{rel}: cannot read discovery declaration: {exc}")
                continue
            if actual != target:
                errors.append(f"{rel}: expected discovery target {target!r}, found {actual!r}")
            resolved = (link.parent / target).resolve()
            if resolved != (root / canonical).resolve():
                errors.append(f"{rel}: discovery target does not resolve to canonical policy")
        unexpected = set(discovered) - allowed
        missing = {rel for rel in allowed if not (root / rel).exists() and not (root / rel).is_symlink()}
        errors.extend(f"{rel}: unexpected instruction-file location" for rel in sorted(unexpected, key=str))
        errors.extend(f"{rel}: allowlisted instruction file not found" for rel in sorted(missing, key=str))
    elif args.strict_locations:
        for rel in discovered:
            value = rel.as_posix()
            if rel.parent != Path(".") and value != ".github/copilot-instructions.md" and not value.startswith((".cursor/", ".windsurf/")):
                errors.append(f"{rel}: unexpected nested instruction-file location")
    else:
        for rel in discovered:
            if rel.name in {"AGENT.md", "AGENTS.md", "CLAUDE.md", "GEMINI.md"} and rel.parent != Path("."):
                warnings.append(f"{rel}: nested instruction scope discovered; review precedence")

    for rel in sorted(files, key=lambda item: item.as_posix()):
        path = root / rel
        if not path.exists() and not path.is_symlink():
            errors.append(f"{rel}: audited file not found")
            continue
        if path.is_dir() or path.suffix.lower() not in TEXT_SUFFIXES and path.name not in INSTRUCTION_NAMES:
            continue
        inspect_file(path, rel, root, rel in set(discovered) or allowed is not None and rel in allowed, errors, warnings)

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
