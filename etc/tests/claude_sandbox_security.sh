#!/usr/bin/env bash
set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
script="$repo/src/.local/bin/claude-sandbox"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/home" "$work/cache" "$work/fixture"
printf '%s\n' test > "$work/fixture/input.txt"

help=$(bash "$script" help)
for value in run-untrusted --repo-setup --signing; do
  [[ "$help" == *"$value"* ]] || { echo "FAIL: help missing $value" >&2; exit 1; }
done

cat > "$work/bin/docker" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$CSB_TEST_LOG"
case "$1 $2" in
  "volume create") printf '%s\n' test-volume ;;
  "container inspect") exit 1 ;;
esac
exit 0
EOF
chmod +x "$work/bin/docker"

CSB_TEST_LOG="$work/docker.log" \
HOME="$work/home" \
XDG_CACHE_HOME="$work/cache" \
MOX_REPO="$repo" \
PATH="$work/bin:$PATH" \
  bash "$script" run-untrusted --image test-image "$work/fixture" -- sh -c 'test -f input.txt'

log=$(cat "$work/docker.log")
for value in '--network none' '--cap-drop ALL' 'no-new-privileges' '--read-only' '--memory 4g' '--cpus 4' '--pids-limit 512' '/workspace:rw,nosuid,nodev,size=4g'; do
  [[ "$log" == *"$value"* ]] || { echo "FAIL: untrusted runner missing $value" >&2; exit 1; }
done
for value in '.claude' 'SANDBOX_AGENT' 'host-1password' '.config/git'; do
  [[ "$log" != *"$value"* ]] || { echo "FAIL: untrusted runner exposed $value" >&2; exit 1; }
done
[[ "$log" != *'volume create'* ]] || { echo 'FAIL: untrusted runner persisted a workspace volume' >&2; exit 1; }

grep -q 'CSB_REPO_SETUP' "$repo/etc/sandbox/entrypoint.sh" || { echo 'FAIL: repository setup is not gated' >&2; exit 1; }

: > "$work/docker.log"
ln -s "$work/home" "$work/fixture/external-home"
CSB_TEST_LOG="$work/docker.log" \
HOME="$work/home" \
XDG_CACHE_HOME="$work/cache" \
MOX_REPO="$repo" \
PATH="$work/bin:$PATH" \
  bash "$script" "$work/fixture"

log=$(cat "$work/docker.log")
[[ "$log" == *'--permission-mode acceptEdits'* ]] || { echo 'FAIL: normal mode is not acceptEdits by default' >&2; exit 1; }
for value in 'CSB_REPO_SETUP' 'SANDBOX_AGENT' '.config/git' "$work/home:$work/home"; do
  [[ "$log" != *"$value"* ]] || { echo "FAIL: normal mode exposed optional capability $value" >&2; exit 1; }
done

echo "claude sandbox security tests passed"
