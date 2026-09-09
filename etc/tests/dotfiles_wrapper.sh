#!/usr/bin/env bash
# Run with: bash etc/tests/dotfiles_wrapper.sh   (from the repo root)
#
# Smoke tests for the `dotfiles` wrapper. Verifies that each subcommand
# parses arguments correctly and emits expected boilerplate. Doesn't run
# real mox -- that requires a full mox setup which is too heavy for
# unit-test scope. The deeper functionality is exercised by the
# pick/sync/packages tests.

set -uo pipefail

REPO_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
BIN="$REPO_DIR/src/.local/bin/dotfiles"

fails=0; passes=0

_match() {
  local desc="$1" pattern="$2" out="$3"
  if [[ "$out" == *"$pattern"* ]]; then
    printf '  ✓ %s\n' "$desc"; passes=$((passes+1))
  else
    printf '  ✗ %s\n      expected substring: %q\n      got: %q\n' "$desc" "$pattern" "$out"
    fails=$((fails+1))
  fi
}
_no_match() {
  local desc="$1" pattern="$2" out="$3"
  if [[ "$out" != *"$pattern"* ]]; then
    printf '  ✓ %s\n' "$desc"; passes=$((passes+1))
  else
    printf '  ✗ %s\n      did NOT expect: %q\n      got: %q\n' "$desc" "$pattern" "$out"
    fails=$((fails+1))
  fi
}
_section() { printf '\n%s\n' "$1"; }

_section "top-level help lists every custom subcommand"
out=$(bash "$BIN" --help 2>&1)
for cmd in info install sync edit profile doctor upgrade; do
  _match "help mentions $cmd" "dotfiles $cmd" "$out"
done

_section "each subcommand --help works and mentions the command name"
for cmd in install sync edit profile doctor upgrade; do
  out=$(bash "$BIN" "$cmd" --help 2>&1)
  _match "$cmd --help shows subject" "dotfiles $cmd" "$out"
  # Help text is non-empty (>50 bytes worth of useful prose).
  if (( ${#out} >= 50 )); then
    printf '  ✓ %s --help is non-trivial\n' "$cmd"; passes=$((passes+1))
  else
    printf '  ✗ %s --help too short (%d bytes)\n' "$cmd" "${#out}"
    fails=$((fails+1))
  fi
done

_section "edit with no pattern errors loudly"
out=$(bash "$BIN" edit 2>&1); rc=$?
_match "edit no-arg error message" "usage: dotfiles edit <pattern>" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ edit no-arg should exit non-zero (got %d)\n' "$rc"; fails=$((fails+1)); }

_section "edit with non-matching pattern errors"
out=$(bash "$BIN" edit definitely-not-a-real-managed-pattern-xxx 2>&1); rc=$?
_match "non-match error mentions pattern" "no managed file matches" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ edit non-match should exit non-zero (got %d)\n' "$rc"; fails=$((fails+1)); }

# A stub mox on PATH answers the queries the wrapper makes, so the profile
# and doctor paths run against fixed data instead of this machine's state.
STUB="$(mktemp -d)"
trap 'rm -rf "$STUB"' EXIT
cat > "$STUB/mox" <<'EOF'
#!/usr/bin/env bash
case "$1" in
  facts)  printf 'profile = "%s"\n' "${STUB_PROFILE-personal}" ;;
  doctor) printf '%s\n' "${STUB_DOCTOR_RAW:-mox doctor: ${STUB_ADVISORIES:-0} advisory item(s) need attention}" ;;
  status) [[ "${2:-}" == --porcelain ]] || printf '  clean    ~/.zshrc\n  clean    ~/.config/git/config\n  clean    ~/.codex/config.toml  (own 3)\n  ERROR    ~/.broken.toml (compose failed: TomlParseError)\n' ;;
  help)   [[ "$2" == apply || "$2" == status || "$2" == diff ]] ;;
  --help) printf 'Commands:\n  apply      Compose and write\n  status     Report drift\n  diff       Show differences\n' ;;
  edit)   printf 'EDIT %s\n' "$2" ;;
  *)      printf 'FORWARDED %s\n' "$*" ;;
esac
EOF
chmod +x "$STUB/mox"

_section "unknown subcommands are forwarded to mox or refused"
out=$(PATH="$STUB:$PATH" bash "$BIN" apply --dry-run 2>&1); rc=$?
_match "a mox subcommand is forwarded with its arguments" "FORWARDED apply --dry-run" "$out"
out=$(PATH="$STUB:$PATH" bash "$BIN" --version 2>&1)
_match "flags pass through" "FORWARDED --version" "$out"
out=$(PATH="$STUB:$PATH" bash "$BIN" statsu 2>&1); rc=$?
_match "a typo is refused" "unknown subcommand statsu" "$out"
_match "a typo gets the nearest subcommand" "did you mean: dotfiles status" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ typo should exit non-zero\n'; fails=$((fails+1)); }
out=$(PATH="$STUB:$PATH" bash "$BIN" docter 2>&1)
_match "a wrapper typo is suggested too" "did you mean: dotfiles doctor" "$out"
out=$(PATH="$STUB:$PATH" bash "$BIN" zzzzzzzz 2>&1)
_no_match "a distant word gets no suggestion" "did you mean" "$out"
out=$(PATH="/usr/bin:/bin" bash "$BIN" apply 2>&1); rc=$?
_match "missing mox is reported as such" "mox not on PATH" "$out"
_no_match "missing mox is not called a typo" "unknown subcommand" "$out"

_section "edit opens the single managed match"
out=$(PATH="$STUB:$PATH" bash "$BIN" edit config/git/config 2>&1); rc=$?
_match "edit hands mox the home path" "EDIT $HOME/.config/git/config" "$out"
[[ $rc -eq 0 ]] && passes=$((passes+1)) || { printf '  ✗ edit should exit zero (got %d)\n' "$rc"; fails=$((fails+1)); }

_section "profile with no arg prints the profile fact"
out=$(PATH="$STUB:$PATH" bash "$BIN" profile 2>&1); rc=$?
_match "profile prints the fact" "personal" "$out"
[[ $rc -eq 0 && "$out" == personal ]] && passes=$((passes+1)) || { printf '  ✗ profile should print exactly the fact (rc %d)\n      got: %q\n' "$rc" "$out"; fails=$((fails+1)); }
out=$(PATH="$STUB:$PATH" STUB_PROFILE="" bash "$BIN" profile 2>&1); rc=$?
_match "unset profile errors" "profile fact is unset" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ unset profile should exit non-zero\n'; fails=$((fails+1)); }

_section "profile with unknown name rejects"
out=$(bash "$BIN" profile some-bogus-name 2>&1); rc=$?
_match "rejects unknown profile" "unknown profile" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ unknown profile should exit non-zero\n'; fails=$((fails+1)); }

_section "upgrade --help documents --all and brew"
# Bare `dotfiles upgrade` runs a real upgrade, which we don't want to
# actually do in tests. Verify the --help path works as a smoke.
out=$(bash "$BIN" upgrade --help 2>&1)
_match "upgrade --help mentions --all" "--all" "$out"
_match "upgrade --help mentions brew"  "brew"  "$out"

_section "upgrade with unknown flag rejects"
out=$(bash "$BIN" upgrade --bogus 2>&1); rc=$?
_match "upgrade unknown flag error" "unknown flag" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ upgrade --bogus should exit non-zero\n'; fails=$((fails+1)); }

_section "doctor --help describes what's checked"
out=$(bash "$BIN" doctor --help 2>&1)
_match "doctor --help mentions profile"  "profile"  "$out"
_match "doctor --help mentions packages" "package"  "$out"

_section "doctor runs and emits a numbered summary"
# Doctor should always finish (it's tolerant of missing tools) and print
# "N passed, M failed" summary on a line by itself.
out=$(PATH="$STUB:$PATH" bash "$BIN" doctor 2>&1)
case "$out" in
  *"passed,"*"failed"*) printf '  ✓ doctor emits summary\n'; passes=$((passes+1)) ;;
  *) printf '  ✗ doctor missing summary\n      got: %q\n' "$out"; fails=$((fails+1)) ;;
esac
_match "doctor runs mox doctor" "mox doctor reports no advisory" "$out"

_section "doctor fails when mox doctor reports an advisory"
out=$(PATH="$STUB:$PATH" STUB_ADVISORIES=1 bash "$BIN" doctor 2>&1); rc=$?
_match "advisory count shown" "1 advisory" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ doctor should exit non-zero on an advisory\n'; fails=$((fails+1)); }

_section "doctor fails when mox doctor skipped a check"
out=$(PATH="$STUB:$PATH" STUB_DOCTOR_RAW='mox doctor: 1 check(s) skipped (coverage incomplete)' bash "$BIN" doctor 2>&1); rc=$?
_match "the skipped count is shown" "1 skipped" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ doctor should exit non-zero on a skipped check\n'; fails=$((fails+1)); }

_section "edit hands mox the path without its ownership annotation"
out=$(PATH="$STUB:$PATH" bash "$BIN" edit codex 2>&1)
_match "the annotated line is offered" "EDIT $HOME/.codex/config.toml" "$out"
_no_match "the annotation is stripped" "(own" "$out"
out=$(PATH="$STUB:$PATH" bash "$BIN" edit broken 2>&1)
_match "a status ERROR row is not offered as a path" "no managed file matches" "$out"

_section "upgrade forwards to mox"
out=$(PATH="$STUB:/usr/bin:/bin" bash "$BIN" upgrade 2>&1)
_match "bare upgrade runs mox upgrade" "FORWARDED upgrade" "$out"
out=$(PATH="$STUB:/usr/bin:/bin" bash "$BIN" upgrade --all 2>&1)
_match "upgrade --all runs mox upgrade --yes" "FORWARDED upgrade --yes" "$out"

_section "doctor fails when the mox doctor report cannot be parsed"
out=$(PATH="$STUB:$PATH" STUB_DOCTOR_RAW='mox doctor: a report shape the wrapper has never seen' bash "$BIN" doctor 2>&1); rc=$?
_match "the report is not read as clean" "unparsed" "$out"
[[ $rc -ne 0 ]] && passes=$((passes+1)) || { printf '  ✗ doctor should exit non-zero on an unparsed report\n'; fails=$((fails+1)); }

# A fixture repo whose libraries are stubs: `pick` prints the selection it
# was handed and the item list, and every setup entry is inert, so `install`
# can run end to end on either OS without touching the machine.
FIX="$(mktemp -d)"
trap 'rm -rf "$STUB" "$FIX"' EXIT
mkdir -p "$FIX/etc/bash/lib" "$FIX/etc/darwin" "$FIX/etc/linux" "$FIX/src/.config/mise" "$FIX/src/.config/holt" "$FIX/bin"
cp "$REPO_DIR/etc/bash/lib/init.bash" "$FIX/etc/bash/lib/"
printf 'x\n' > "$FIX/etc/darwin/packages.txt"
printf 'y\n' > "$FIX/etc/darwin/packages-blacklist.txt"
printf 'x\n' > "$FIX/etc/linux/packages-fedora.txt"
for lib in unix darwin brew mise holt linux tools; do
  printf '#!/usr/bin/env bash\n%s::setup() { :; }\n' "$lib" > "$FIX/etc/bash/lib/$lib.bash"
done
printf 'unix::keep_sudo() { :; }\n' >> "$FIX/etc/bash/lib/unix.bash"
printf 'darwin::require_clt() { :; }\n' >> "$FIX/etc/bash/lib/darwin.bash"
printf 'linux::detect_distro() { printf fedora; }\n' >> "$FIX/etc/bash/lib/linux.bash"
printf 'tools::setup() { printf RAN=tools::setup\\n; }\n' >> "$FIX/etc/bash/lib/tools.bash"
cat > "$FIX/etc/bash/lib/pick.bash" <<'EOF'
pick() {
  printf 'PICK=%s\n' "${DOTFILES_PICK:-}"
  printf 'ITEM=%s\n' "$@"
  local fn
  for fn in ${DOTFILES_PICK//,/ }; do
    [[ "$fn" == *::* ]] || continue
    if declare -F "$fn" >/dev/null; then "$fn"; else printf 'MISSING=%s\n' "$fn"; fi
  done
}
EOF
printf '#!/bin/sh\necho Linux\n' > "$FIX/bin/uname"
chmod +x "$FIX/bin/uname"

_section "install maps step names to their registered items"
out=$(MOX_REPO="$FIX" bash "$BIN" install brew mise 2>&1)
_match "brew becomes brew::setup" "PICK=brew::setup,mise::setup" "$out"
out=$(MOX_REPO="$FIX" bash "$BIN" install all 2>&1)
_match "all passes through" "PICK=all" "$out"
_match "darwin lists the brew step" "ITEM=brew::setup=" "$out"
h1=$(printf '%s\n' "$out" | sed -n 's/^ITEM=brew::setup=.*|//p' | head -n1)
printf 'blocked\n' >> "$FIX/etc/darwin/packages-blacklist.txt"
out=$(MOX_REPO="$FIX" bash "$BIN" install all 2>&1)
h2=$(printf '%s\n' "$out" | sed -n 's/^ITEM=brew::setup=.*|//p' | head -n1)
[[ -n "$h1" && -n "$h2" && "$h1" != "$h2" ]] && passes=$((passes+1)) || { printf '  ✗ the blacklist is part of the brew step hash (before=%s after=%s)\n' "$h1" "$h2"; fails=$((fails+1)); }

_section "install on Linux imports the tools library"
out=$(MOX_REPO="$FIX" PATH="$FIX/bin:$PATH" bash "$BIN" install tools 2>&1); rc=$?
_match "linux lists the distro packages" "ITEM=linux::setup=System packages (fedora)" "$out"
_match "linux lists the tools step" "ITEM=tools::setup=" "$out"
_match "the tools step runs from the imported library" "RAN=tools::setup" "$out"
_no_match "no picked step is missing" "MISSING=" "$out"
_no_match "linux reports no missing library" "No such file" "$out"
[[ $rc -eq 0 ]] && passes=$((passes+1)) || { printf '  ✗ linux install should exit zero (got %d)\n' "$rc"; fails=$((fails+1)); }

printf '\n%d passed, %d failed\n' "$passes" "$fails"
exit "$((fails > 0 ? 1 : 0))"
