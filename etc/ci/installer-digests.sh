#!/usr/bin/env bash
# Fetch every installer script the repo pins, by commit or by release tag,
# from the URL each declaration reads, and check it against the digest it
# records: a mismatch means the recorded digest and the pinned script no
# longer agree. Homebrew's pin is the `[[bootstrap]]` row mox itself
# installs from; the rest are the setup libraries' own.
set -uo pipefail
if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "installer-digests.sh: not inside a git work tree, so there are no setup libraries to read" >&2
  exit 2
fi
cd "$(git rev-parse --show-toplevel)" || exit 2

fails=0
_sha256() { if command -v sha256sum >/dev/null 2>&1; then sha256sum; else shasum -a 256; fi | cut -d' ' -f1; }
# check <label> <source file> <url> <digest the source records>
check() {
  local label="$1" source="$2" url="$3" want="$4" got
  got=$(curl -fsSL "$url" | _sha256) || { echo "FAIL: $label: could not fetch $url" >&2; fails=$((fails + 1)); return; }
  if [[ "$got" == "$want" ]]; then
    echo "$label: digest matches"
  else
    echo "FAIL: $label: $url has digest $got, $source records $want" >&2
    fails=$((fails + 1))
  fi
}

# The Homebrew pin is read from the one `[[bootstrap]]` row; with two rows a
# first-match read would check one and vouch for both.
brew_manifest=data/packages/darwin.toml
bootstrap_rows=$(grep -c '^[[:space:]]*\[\[bootstrap\]\]' "$brew_manifest")
if [[ "$bootstrap_rows" != 1 ]]; then
  echo "FAIL: $brew_manifest has $bootstrap_rows [[bootstrap]] rows; this check reads exactly one" >&2
  exit 1
fi
_bootstrap_field() {
  awk -v key="$1" '
    function value(s,   q) {
      sub(/^[[:space:]]*[A-Za-z0-9_-]+[[:space:]]*=[[:space:]]*/, "", s)
      q = substr(s, 1, 1)
      if (q != "\"" && q != "\047") return ""
      s = substr(s, 2)
      sub(q ".*$", "", s)
      return s
    }
    /^[[:space:]]*\[\[bootstrap\]\]/ { in_row = 1; next }
    /^[[:space:]]*\[/ { in_row = 0 }
    in_row && $0 ~ ("^[[:space:]]*" key "[[:space:]]*=") { print value($0) }
  ' "$brew_manifest"
}
check "Homebrew install.sh" "$brew_manifest" "$(_bootstrap_field url)" "$(_bootstrap_field sha256)"
check "holt install.sh" etc/bash/lib/holt.bash "$(sed -n 's/^HOLT_INSTALL_URL="\(.*\)"$/\1/p' etc/bash/lib/holt.bash | sed "s/\${HOLT_VERSION}/$(sed -n 's/^HOLT_VERSION=//p' etc/bash/lib/holt.bash)/")" "$(sed -n 's/^HOLT_INSTALL_SHA256=//p' etc/bash/lib/holt.bash)"
holt_ps_version=$(sed -n "s/^\$Script:HoltVersion = '\(.*\)'$/\1/p" etc/powershell/lib/Holt.psm1)
holt_ps_url=$(sed -n 's/^\$Script:HoltInstallUrl = "\(.*\)"$/\1/p' etc/powershell/lib/Holt.psm1 | sed "s/\$Script:HoltVersion/${holt_ps_version}/")
check "holt install.ps1" etc/powershell/lib/Holt.psm1 "$holt_ps_url" "$(sed -n "s/^\$Script:HoltInstallSha256 = '\(.*\)'$/\1/p" etc/powershell/lib/Holt.psm1)"
scoop_commit=$(sed -n "s/^\$Script:ScoopInstallCommit = '\(.*\)'$/\1/p" etc/powershell/lib/Scoop.psm1)
check "scoop install.ps1" etc/powershell/lib/Scoop.psm1 "https://raw.githubusercontent.com/ScoopInstaller/Install/${scoop_commit}/install.ps1" "$(sed -n "s/^\$Script:ScoopInstallSha256 = '\(.*\)'$/\1/p" etc/powershell/lib/Scoop.psm1)"

exit "$((fails > 0 ? 1 : 0))"
