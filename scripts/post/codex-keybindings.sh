#!/usr/bin/env bash
set -euo pipefail

command -v codex >/dev/null 2>&1 || exit 0

config="${CODEX_HOME:-$HOME/.codex}/config.toml"
mkdir -p "${config%/*}"
touch "$config"

python3 - "$config" <<'PY'
from pathlib import Path
import os
import re
import shutil
import stat
import subprocess
import sys
import tempfile

path = Path(sys.argv[1])
original_stat = path.stat()
text = path.read_text(encoding="utf-8")
if '"""' in text or "'''" in text:
    raise SystemExit("refusing to patch config.toml containing a multiline string")
managed = {
    "tui.keymap.global",
    "tui.keymap.composer",
    "tui.keymap.editor",
}
headers = list(re.finditer(r"(?m)^[ \t]*\[([^\]\r\n]+)\][ \t]*(?:\r?\n|$)", text))
spans = []
for index, header in enumerate(headers):
    if header.group(1) in managed:
        end = headers[index + 1].start() if index + 1 < len(headers) else len(text)
        spans.append((header.start(), end))

chunks = []
cursor = 0
for start, end in spans:
    chunks.append(text[cursor:start])
    cursor = end
chunks.append(text[cursor:])
body = "".join(chunks)

keymap = """[tui.keymap.global]
open_transcript = "ctrl-shift-t"
open_external_editor = "ctrl-shift-g"
copy = "ctrl-shift-o"
clear_terminal = "ctrl-l"

[tui.keymap.composer]
submit = ["enter", "ctrl-j", "ctrl-m"]
history_search_previous = "ctrl-r"
history_search_next = "ctrl-s"

[tui.keymap.editor]
insert_newline = ["shift-enter", "alt-enter", "ctrl-shift-j"]
move_left = "ctrl-b"
move_right = "ctrl-f"
move_up = "ctrl-p"
move_down = "ctrl-n"
move_word_left = "alt-b"
move_word_right = "alt-f"
move_line_start = "ctrl-a"
move_line_end = "ctrl-e"
delete_backward = ["backspace", "ctrl-h"]
delete_forward = "ctrl-d"
delete_backward_word = "ctrl-w"
delete_forward_word = "alt-d"
kill_line_start = "ctrl-u"
kill_line_end = "ctrl-k"
yank = "ctrl-y"
"""

if not body:
    separator = ""
elif body.endswith("\n\n"):
    separator = ""
elif body.endswith("\n"):
    separator = "\n"
else:
    separator = "\n\n"
candidate = body + separator + keymap
with tempfile.TemporaryDirectory(prefix="codex-keymap-check-") as check_dir:
    check_path = Path(check_dir) / "config.toml"
    check_path.write_text(candidate, encoding="utf-8")
    result = subprocess.run(
        [shutil.which("codex"), "app-server", "--strict-config", "--listen", "off"],
        env={**os.environ, "CODEX_HOME": check_dir},
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
    )
    if result.returncode and "no transport configured" not in result.stdout:
        raise SystemExit("Codex rejected patched config.toml:\n" + result.stdout.rstrip())

current_stat = path.stat()
if (current_stat.st_ino, current_stat.st_mtime_ns, current_stat.st_size) != (
    original_stat.st_ino,
    original_stat.st_mtime_ns,
    original_stat.st_size,
):
    raise SystemExit("config.toml changed while it was being patched")

fd, temporary = tempfile.mkstemp(prefix=".config.toml.", dir=path.parent)
try:
    with os.fdopen(fd, "w", encoding="utf-8") as handle:
        handle.write(candidate)
        handle.flush()
        os.fsync(handle.fileno())
    os.chmod(temporary, stat.S_IMODE(original_stat.st_mode))
    current_stat = path.stat()
    if (current_stat.st_ino, current_stat.st_mtime_ns, current_stat.st_size) != (
        original_stat.st_ino,
        original_stat.st_mtime_ns,
        original_stat.st_size,
    ):
        raise SystemExit("config.toml changed while it was being patched")
    os.replace(temporary, path)
finally:
    if os.path.exists(temporary):
        os.unlink(temporary)
PY
