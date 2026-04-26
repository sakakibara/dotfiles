#!/usr/bin/env bash
# sync — interactive review of installed-but-untracked packages.
#
# Diffs `<os pkg manager>` output against the tracked packages file
# (and the blacklist) and offers a per-row TUI to assign one of these
# actions to each untracked entry:
#
#   skip   — leave it alone
#   add    — append plain `kind:name` to packages.txt (applies everywhere)
#   @prof  — append `kind:name @profile` (profile-gated)
#   block  — append to packages-blacklist.txt (sync won't surface again)
#
# State is held in two parallel arrays (one row per item):
#   _sync_items[]    "kind<TAB>name"
#   _sync_actions[]  "skip" | "add" | "@<profile>" | "block"
#   _sync_desc[]     short description (or empty)
#
# Reuses pick's terminal primitives (alt-screen, key reader, width helpers).

import msg store packages pick linux

# Sets / map populated each `sync::_collect_*` pass, used to filter the
# installed-but-untracked list. Defined once at module load (eval-free
# at call time).
store::set _sync_tracked
store::set _sync_blacklisted
store::set _sync_installed
store::map _sync_desc_map

# ---------- query installed (OS-dispatched) ----------

sync::_query_installed() {
  local os="$1"
  case "$os" in
    darwin) sync::_query_installed_darwin ;;
    linux)  sync::_query_installed_linux ;;
    *) ;;
  esac
}

sync::_query_installed_linux() {
  # Yields user-installed packages (not transitive deps) as `pkg<TAB>name`.
  # Distro-specific because each package manager has its own "manually-
  # installed" query.
  local distro
  distro=$(linux::detect_distro 2>/dev/null || echo unknown)
  case "$distro" in
    fedora)
      # `dnf repoquery --userinstalled` emits `name-ver-arch` per line; strip
      # to bare names. This matches the request set, not transitive deps.
      dnf repoquery --userinstalled --qf '%{name}\n' 2>/dev/null \
        | sort -u | sed 's/^/pkg\t/'
      ;;
    debian)
      apt-mark showmanual 2>/dev/null | sort -u | sed 's/^/pkg\t/'
      ;;
    arch)
      pacman -Qe -q 2>/dev/null | sort -u | sed 's/^/pkg\t/'
      ;;
    suse)
      # `zypper se -i -t package` includes deps; filter to "i+" rows
      # (user-installed, not auto). zypper output has 7 columns —
      # 'S | Repo | Name | ...'; we want the Name column.
      zypper --non-interactive se -i -t package 2>/dev/null \
        | awk -F'|' '$1 ~ /i\+/ {gsub(/^[[:space:]]+|[[:space:]]+$/,"",$3); print $3}' \
        | sort -u | sed 's/^/pkg\t/'
      ;;
  esac
}

sync::_query_installed_darwin() {
  # Taps
  local t
  while IFS= read -r t; do
    [[ -z "$t" ]] && continue
    printf 'tap\t%s\n' "$t"
  done < <(brew tap 2>/dev/null)
  # Formulae installed on user request (skips deps pulled in transitively)
  local f
  while IFS= read -r f; do
    [[ -z "$f" ]] && continue
    printf 'brew\t%s\n' "$f"
  done < <(brew leaves --installed-on-request 2>/dev/null)
  # Casks
  local c
  while IFS= read -r c; do
    [[ -z "$c" ]] && continue
    printf 'cask\t%s\n' "$c"
  done < <(brew list --cask 2>/dev/null)
}

# Compute the diff between currently-installed and tracked. Outputs
# "kind<TAB>name" for each untracked, non-blacklisted entry.
# Args: PACKAGES_FILE BLACKLIST_FILE OS DEFAULT_KIND
sync::compute_untracked() {
  local pkg_file="$1" blacklist_file="$2" os="$3" default_kind="$4"

  _sync_tracked::clear
  _sync_blacklisted::clear

  if [[ -r "$pkg_file" ]]; then
    local kind name
    while IFS=$'\t' read -r kind name; do
      _sync_tracked::add "${kind}:${name}"
    done < <(packages::all "$pkg_file" "$default_kind")
  fi

  if [[ -r "$blacklist_file" ]]; then
    local kind name
    while IFS=$'\t' read -r kind name; do
      _sync_blacklisted::add "${kind}:${name}"
    done < <(packages::all "$blacklist_file" "$default_kind")
  fi

  local kind name
  while IFS=$'\t' read -r kind name; do
    _sync_tracked::has     "${kind}:${name}" && continue
    _sync_blacklisted::has "${kind}:${name}" && continue
    printf '%s\t%s\n' "$kind" "$name"
  done < <(sync::_query_installed "$os")
}

# Compute the inverse: tracked entries that aren't installed locally.
# Output: "kind<TAB>name<TAB>profiles" (profiles = comma-separated, may be empty).
# Lines that *should* be installed for the current profile but aren't are
# the interesting ones; cross-profile entries are silently filtered out.
# Args: PACKAGES_FILE OS DEFAULT_KIND CURRENT_PROFILE
sync::compute_missing() {
  local pkg_file="$1" os="$2" default_kind="$3" profile="$4"
  [[ -r "$pkg_file" ]] || return 0

  _sync_installed::clear
  local kind name
  while IFS=$'\t' read -r kind name; do
    _sync_installed::add "${kind}:${name}"
  done < <(sync::_query_installed "$os")

  local _pkg_kind _pkg_name _pkg_profiles
  local line
  while IFS= read -r line || [[ -n "$line" ]]; do
    packages::parse "$line" || continue
    packages::applies_to "$profile" || continue
    local k="${_pkg_kind:-$default_kind}"
    _sync_installed::has "${k}:${_pkg_name}" && continue
    local profile_str=""
    if (( ${#_pkg_profiles[@]} > 0 )); then
      local IFS=','
      profile_str="${_pkg_profiles[*]}"
    fi
    printf '%s\t%s\t%s\n' "$k" "$_pkg_name" "$profile_str"
  done < "$pkg_file"
}

# ---------- description fetch ----------

# Populate _sync_desc[] (parallel to _sync_items[]) by batching `brew desc`
# calls per-kind. Empty string for items with no description.
sync::_fetch_descriptions_darwin() {
  command -v brew >/dev/null 2>&1 || return 0
  local brews=() casks=()
  local i n=${#_sync_items[@]}
  for ((i=0; i<n; i++)); do
    local kind name
    IFS=$'\t' read -r kind name <<< "${_sync_items[i]}"
    case "$kind" in
      brew) brews+=("$name") ;;
      cask) casks+=("$name") ;;
    esac
  done

  if (( ${#brews[@]} > 0 )); then
    local line nm desc
    while IFS= read -r line; do
      [[ "$line" == *": "* ]] || continue
      nm="${line%%: *}"; desc="${line#*: }"
      _sync_desc_map::put "brew:$nm" "$desc"
    done < <(brew desc "${brews[@]}" 2>/dev/null)
  fi
  if (( ${#casks[@]} > 0 )); then
    local line nm desc
    while IFS= read -r line; do
      [[ "$line" == *": "* ]] || continue
      nm="${line%%: *}"; desc="${line#*: }"
      _sync_desc_map::put "cask:$nm" "$desc"
    done < <(brew desc --cask "${casks[@]}" 2>/dev/null)
  fi
}

# Fetch a one-line description for each pkg-kind item via the distro's
# package manager. Single-package query per item — slower than darwin's
# batched `brew desc`, but the untracked subset is usually small.
sync::_fetch_descriptions_linux() {
  local distro
  distro=$(linux::detect_distro 2>/dev/null || echo unknown)
  local i n=${#_sync_items[@]}
  for ((i=0; i<n; i++)); do
    local kind name
    IFS=$'\t' read -r kind name <<< "${_sync_items[i]}"
    [[ "$kind" == "pkg" ]] || continue
    local d=""
    case "$distro" in
      fedora) d=$(dnf info "$name" 2>/dev/null | awk -F': *' '/^Summary/ {print $2; exit}') ;;
      debian) d=$(apt-cache show "$name" 2>/dev/null | awk -F': *' '/^Description-en|^Description:/ {print $2; exit}') ;;
      arch)   d=$(pacman -Si "$name" 2>/dev/null | awk -F': *' '/^Description/ {print $2; exit}') ;;
      suse)   d=$(zypper --non-interactive info "$name" 2>/dev/null | awk -F': *' '/^Summary/ {print $2; exit}') ;;
    esac
    [[ -n "$d" ]] && _sync_desc_map::put "pkg:$name" "$d"
  done
}

sync::_fetch_descriptions() {
  _sync_desc_map::clear
  case "$(uname -s)" in
    Darwin) sync::_fetch_descriptions_darwin ;;
    Linux)  sync::_fetch_descriptions_linux  ;;
  esac

  _sync_desc=()
  local i n=${#_sync_items[@]}
  for ((i=0; i<n; i++)); do
    local kind name d
    IFS=$'\t' read -r kind name <<< "${_sync_items[i]}"
    d=$(_sync_desc_map::get "${kind}:${name}" 2>/dev/null || echo "")
    _sync_desc[i]="$d"
  done
}

# ---------- review TUI ----------

# Pad an action label to ACTION_LABEL_WIDTH columns, accounting for visible
# width (for CJK if anyone ever puts a multi-byte profile name).
sync::_action_label() {
  local _sync_action_str
  sync::_action_label_v "$1"
  printf '%s' "$_sync_action_str"
}

# Subshell-free: writes the styled `[ … action … ]` block (centered) to
# the dynamic-scoped `_sync_action_str`.
sync::_action_label_v() {
  local action="$1"
  local label color
  case "$action" in
    skip)  label='skip';   color='\033[2m'    ;;
    add)   label='add';    color='\033[1;32m' ;;
    @*)    label="$action"; color='\033[1;36m' ;;
    block) label='block';  color='\033[1;31m' ;;
    *)     label="$action"; color=''           ;;
  esac
  local total=10 _pick_width
  pick::_str_width_v "$label"
  local pad=$((total - _pick_width))
  (( pad < 0 )) && pad=0
  local pl=$((pad / 2)) pr=$((pad - pad / 2))
  printf -v _sync_action_str '%b[%*s%s%*s]\033[0m' "$color" "$pl" "" "$label" "$pr" ""
}

sync::_render() {
  local cursor="$1" current="$2"
  local cols rows
  cols=$(pick::_cols)
  rows=$(pick::_rows)

  local n=${#_sync_items[@]}
  # 4 chrome lines (title + 2 keymap lines + blank), 2 footer lines (blank + status).
  local body_rows=$((rows - 6))
  (( body_rows < 3 )) && body_rows=3

  # Slide the viewport so the cursor stays visible with one-line padding
  # at top/bottom when room allows. _sync_offset is dynamic-scoped from
  # sync::review and persists across redraws.
  if (( n <= body_rows )); then
    _sync_offset=0
  else
    if (( cursor < _sync_offset + 1 )); then
      _sync_offset=$((cursor - 1))
    elif (( cursor >= _sync_offset + body_rows - 1 )); then
      _sync_offset=$((cursor - body_rows + 2))
    fi
    (( _sync_offset < 0 )) && _sync_offset=0
    (( _sync_offset > n - body_rows )) && _sync_offset=$((n - body_rows))
  fi

  # DEC sync mode 2026 — commit the redraw atomically (no flash between
  # clear and re-draw). Ignored by terminals that don't support it.
  printf '\033[?2026h'
  printf '\033[H\033[2J'
  printf '\033[1mReview untracked packages\033[0m\n'
  printf '\033[2m↑/↓ move · space cycle · a add · p add @personal · w add @work\n'
  printf '   b blacklist · s skip · enter apply · q cancel · ? help\033[0m\n\n'

  local end=$((_sync_offset + body_rows))
  (( end > n )) && end=$n

  local pending=0 i
  for ((i=0; i<n; i++)); do
    [[ "${_sync_actions[i]}" != "skip" ]] && pending=$((pending + 1))
  done

  local _sync_action_str
  for ((i=_sync_offset; i<end; i++)); do
    local action="${_sync_actions[i]}"

    local kind name
    IFS=$'\t' read -r kind name <<< "${_sync_items[i]}"
    local display="${kind}:${name}"

    local marker
    if (( i == cursor )); then
      marker=$'\033[7m›\033[0m '
    else
      marker='  '
    fi

    sync::_action_label_v "$action"

    local desc="${_sync_desc[i]:-}"
    if [[ -n "$desc" ]]; then
      printf '%s%b %s \033[2m— %s\033[0m\n' "$marker" "$_sync_action_str" "$display" "$desc"
    else
      printf '%s%b %s\n' "$marker" "$_sync_action_str" "$display"
    fi
  done

  printf '\n\033[2m%d pending · profile: %s' "$pending" "$current"
  if (( n > body_rows )); then
    printf ' · %d–%d/%d' "$((_sync_offset + 1))" "$end" "$n"
  fi
  printf '\033[0m\n'
  printf '\033[?2026l'  # commit atomic redraw
}

sync::_render_help() {
  printf '\033[H\033[2J'
  printf '\033[1mSync keybindings\033[0m\n\n'
  cat <<'EOF'
  ↑ / k          move up
  ↓ / j          move down
  space          cycle: skip → add → @<current> → @<other> → block → skip
  a              add (no profile annotation — applies everywhere)
  p              add @personal
  w              add @work
  b              blacklist (write to packages-blacklist.txt)
  s              skip (default — no action)
  enter          apply pending actions
  q / esc        cancel without writing
  ?              this help

EOF
  printf '\033[2mPress any key to return.\033[0m\n'
  pick::_read_key >/dev/null
}

# Cycle through actions: skip → add → @<current> → @<other> → block → skip.
sync::_cycle_action() {
  local current="$1" other="$2" action="$3"
  case "$action" in
    skip) printf 'add' ;;
    add)  printf '@%s' "$current" ;;
    "@$current")
      if [[ -n "$other" ]]; then printf '@%s' "$other"
      else printf 'block'; fi
      ;;
    "@$other") printf 'block' ;;
    block) printf 'skip' ;;
    *)     printf 'skip' ;;
  esac
}

# Run the review TUI. Reads _sync_items[] and writes _sync_actions[].
# Returns 0 if user pressed enter, 130 if they cancelled.
sync::review() {
  local current other
  current=$(packages::current_profile)
  case "$current" in
    personal) other=work ;;
    work)     other=personal ;;
    *)        other="" ;;
  esac

  local n=${#_sync_items[@]}
  if (( n == 0 )); then
    msg::heading "Everything installed is already tracked."
    return 0
  fi

  _sync_actions=()
  local i
  for ((i=0; i<n; i++)); do _sync_actions[i]=skip; done

  msg::heading "Fetching package descriptions…"
  sync::_fetch_descriptions

  pick::_tui_open
  trap 'pick::_tui_close' EXIT INT TERM

  local cursor=0 result=apply
  local _sync_offset=0
  while true; do
    sync::_render "$cursor" "$current"
    local key
    key=$(pick::_read_key)
    case "$key" in
      up)    (( cursor > 0 ))     && cursor=$((cursor - 1)) ;;
      down)  (( cursor < n - 1 )) && cursor=$((cursor + 1)) ;;
      space) _sync_actions[cursor]=$(sync::_cycle_action "$current" "$other" "${_sync_actions[cursor]}") ;;
      a)     _sync_actions[cursor]=add ;;
      s)     _sync_actions[cursor]=skip ;;
      b)     _sync_actions[cursor]=block ;;
      p)     _sync_actions[cursor]="@personal" ;;
      w)     _sync_actions[cursor]="@work" ;;
      help)  sync::_render_help ;;
      enter) result=apply; break ;;
      q|esc) result=cancel; break ;;
    esac
  done

  pick::_tui_close
  trap - EXIT INT TERM

  [[ "$result" == "cancel" ]] && return 130
  return 0
}

# ---------- apply ----------

# Format a parsed entry back into the file's flat-text syntax. Default
# kind (e.g. "brew" on darwin) is dropped — bare names are formula entries.
# Args: kind name default_kind [profile_annotation]
sync::_format_entry() {
  local kind="$1" name="$2" default_kind="$3" annotation="${4:-}"
  local prefix=""
  [[ "$kind" != "$default_kind" ]] && prefix="${kind}:"
  if [[ -n "$annotation" ]]; then
    printf '%s%s %s' "$prefix" "$name" "$annotation"
  else
    printf '%s%s' "$prefix" "$name"
  fi
}

# Atomically append lines to FILE. Lines are passed on stdin.
sync::_append() {
  local file="$1"
  [[ -z "$file" ]] && return 1
  # tmpfile in same dir for atomic rename.
  local dir tmp
  dir="$(dirname "$file")"
  mkdir -p "$dir" 2>/dev/null
  tmp=$(mktemp "$dir/.packages-sync.XXXXXX") || return 1
  if [[ -f "$file" ]]; then
    cat "$file" > "$tmp"
    # Ensure trailing newline before appending.
    [[ -s "$tmp" ]] && [[ "$(tail -c1 "$tmp" | od -An -c | tr -d ' ')" != $'\\n' ]] && printf '\n' >> "$tmp"
  fi
  cat >> "$tmp"
  mv "$tmp" "$file"
}

# Apply the pending actions to packages.txt and packages-blacklist.txt.
# Args: PACKAGES_FILE BLACKLIST_FILE DEFAULT_KIND
sync::apply() {
  local pkg_file="$1" blacklist_file="$2" default_kind="$3"

  local n=${#_sync_items[@]}
  local i
  local pkg_lines=()
  local blacklist_lines=()

  for ((i=0; i<n; i++)); do
    local action="${_sync_actions[i]}"
    [[ "$action" == "skip" ]] && continue
    local kind name
    IFS=$'\t' read -r kind name <<< "${_sync_items[i]}"

    case "$action" in
      add)
        pkg_lines+=("$(sync::_format_entry "$kind" "$name" "$default_kind")")
        ;;
      @*)
        pkg_lines+=("$(sync::_format_entry "$kind" "$name" "$default_kind" "$action")")
        ;;
      block)
        blacklist_lines+=("$(sync::_format_entry "$kind" "$name" "$default_kind")")
        ;;
    esac
  done

  if (( ${#pkg_lines[@]} > 0 )); then
    {
      printf '\n# Added by `dotfiles sync` on %s\n' "$(date -u +%FT%TZ)"
      local l
      for l in "${pkg_lines[@]}"; do printf '%s\n' "$l"; done
    } | sync::_append "$pkg_file"
    msg::success "added ${#pkg_lines[@]} entr$([[ ${#pkg_lines[@]} -eq 1 ]] && echo y || echo ies) to packages.txt"
  fi
  if (( ${#blacklist_lines[@]} > 0 )); then
    {
      printf '\n# Blacklisted by `dotfiles sync` on %s\n' "$(date -u +%FT%TZ)"
      local l
      for l in "${blacklist_lines[@]}"; do printf '%s\n' "$l"; done
    } | sync::_append "$blacklist_file"
    msg::success "blacklisted ${#blacklist_lines[@]} entr$([[ ${#blacklist_lines[@]} -eq 1 ]] && echo y || echo ies)"
  fi
  return 0
}

# ---------- main entry ----------

# sync::run [os] — the orchestration function the wrapper calls.
sync::run() {
  local os
  case "$(uname -s)" in
    Darwin) os=darwin ;;
    Linux)  os=linux  ;;
    *) msg::error "sync: unsupported OS $(uname -s) (Windows path is PowerShell-only — see windows/scoop.ps1)"; return 1 ;;
  esac

  local source_dir pkg_file blacklist_file default_kind profile
  source_dir="${CHEZMOI_SOURCE_DIR:-$(chezmoi source-path 2>/dev/null)}"
  if [[ -z "$source_dir" ]]; then
    msg::error "sync: chezmoi source dir not found"
    return 1
  fi

  case "$os" in
    darwin)
      default_kind=brew
      pkg_file="$source_dir/etc/darwin/packages.txt"
      blacklist_file="$source_dir/etc/darwin/packages-blacklist.txt"
      ;;
    linux)
      default_kind=pkg
      local distro
      distro=$(linux::detect_distro 2>/dev/null || echo unknown)
      if [[ "$distro" == "unknown" ]]; then
        msg::error "sync: could not detect linux distro"
        return 1
      fi
      pkg_file="$source_dir/etc/linux/packages-${distro}.txt"
      blacklist_file="$source_dir/etc/linux/packages-blacklist.txt"
      ;;
  esac
  profile=$(packages::current_profile)

  msg::heading "Computing untracked packages…"
  _sync_items=()
  while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    _sync_items+=("$line")
  done < <(sync::compute_untracked "$pkg_file" "$blacklist_file" "$os" "$default_kind")

  if (( ${#_sync_items[@]} == 0 )); then
    msg::success "Everything installed is already tracked. Nothing to sync."
  else
    # First-run bootstrap: packages.txt is essentially empty AND many items
    # are installed. Per-item review on a 200-package list is hostile —
    # offer a single-key bulk import instead.
    local tracked_count
    tracked_count=$(packages::all "$pkg_file" "$default_kind" 2>/dev/null | wc -l | tr -d ' ')
    if (( ${#_sync_items[@]} >= 20 && tracked_count == 0 )) && [[ -t 0 && -t 1 ]]; then
      msg::heading "Bootstrap mode"
      msg::arrow "${#_sync_items[@]} packages installed, packages.txt is empty."
      printf '  Bulk-import all as global entries (no profile annotation)? [y/N] '
      local ans=''
      IFS= read -rn1 ans </dev/tty 2>/dev/null || ans=n
      printf '\n'
      if [[ "$ans" == "y" || "$ans" == "Y" ]]; then
        _sync_actions=()
        local i
        for ((i=0; i<${#_sync_items[@]}; i++)); do _sync_actions[i]="add"; done
        sync::apply "$pkg_file" "$blacklist_file" "$default_kind"
        msg::arrow "Re-run \`dotfiles sync\` later to refine (profile-gate or blacklist individual entries)."
      else
        sync::review || return $?
        sync::apply "$pkg_file" "$blacklist_file" "$default_kind"
      fi
    else
      sync::review || return $?
      sync::apply "$pkg_file" "$blacklist_file" "$default_kind"
    fi
  fi

  # Stale-entry honesty: list tracked-but-not-installed for the current profile.
  local missing
  missing=$(sync::compute_missing "$pkg_file" "$os" "$default_kind" "$profile")
  if [[ -n "$missing" ]]; then
    msg::heading "Tracked but not installed locally (run \`dotfiles install\` to fix):"
    while IFS=$'\t' read -r kind name profiles; do
      [[ -z "$kind" ]] && continue
      local suffix=""
      [[ -n "$profiles" ]] && suffix=" \033[2m@$profiles\033[0m"
      printf '  • %s:%s%b\n' "$kind" "$name" "$suffix"
    done <<<"$missing"
  fi
}
