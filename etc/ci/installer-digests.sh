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
check() {
  local label="$1" url="$2" want="$3" got
  got=$(curl -fsSL "$url" | _sha256) || { echo "FAIL: $label: could not fetch $url" >&2; fails=$((fails + 1)); return; }
  if [[ "$got" == "$want" ]]; then
    echo "$label: digest matches"
  else
    echo "FAIL: $label: $url has digest $got, the library records $want" >&2
    fails=$((fails + 1))
  fi
}

check "Homebrew install.sh" "$(sed -n 's/^url = "\(.*\)"$/\1/p' data/packages/darwin.toml)" "$(sed -n 's/^sha256 = "\(.*\)"$/\1/p' data/packages/darwin.toml)"
check "holt install.sh" "$(sed -n 's/^HOLT_INSTALL_URL="\(.*\)"$/\1/p' etc/bash/lib/holt.bash | sed "s/\${HOLT_VERSION}/$(sed -n 's/^HOLT_VERSION=//p' etc/bash/lib/holt.bash)/")" "$(sed -n 's/^HOLT_INSTALL_SHA256=//p' etc/bash/lib/holt.bash)"
holt_ps_version=$(sed -n "s/^\$Script:HoltVersion = '\(.*\)'$/\1/p" etc/powershell/lib/Holt.psm1)
holt_ps_url=$(sed -n 's/^\$Script:HoltInstallUrl = "\(.*\)"$/\1/p' etc/powershell/lib/Holt.psm1 | sed "s/\$Script:HoltVersion/${holt_ps_version}/")
check "holt install.ps1" "$holt_ps_url" "$(sed -n "s/^\$Script:HoltInstallSha256 = '\(.*\)'$/\1/p" etc/powershell/lib/Holt.psm1)"
scoop_commit=$(sed -n "s/^\$Script:ScoopInstallCommit = '\(.*\)'$/\1/p" etc/powershell/lib/Scoop.psm1)
check "scoop install.ps1" "https://raw.githubusercontent.com/ScoopInstaller/Install/${scoop_commit}/install.ps1" "$(sed -n "s/^\$Script:ScoopInstallSha256 = '\(.*\)'$/\1/p" etc/powershell/lib/Scoop.psm1)"

exit "$((fails > 0 ? 1 : 0))"
