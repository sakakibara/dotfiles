#!/usr/bin/env bash
# pick — interactive multi-select runner. Replaces sequential `ask_to_run`
# prompts with a single TUI menu where each named function is a togglable
# row. Selected entries run in order with per-step output captured to log
# files and a final summary.
#
# Item syntax
#   [+|~]name[=label][~reason]
#     +     required (always selected, can't toggle)
#     ~     disabled (greyed, can't toggle, optionally with ~reason text)
#     name  bash function or command to run when selected
#     label human-readable text shown in the menu (default: name)
#
# Env
#   DOTFILES_PICK_SCOPE   scope key for last-selection memory (default: "default")
#   DOTFILES_PICK         non-interactive selection: "all" | "none" | "a,b,c"
#                         when set OR no controlling tty, the menu is skipped
#                         "all"   = every non-disabled item (incl required)
#                         "none"  = required only
#                         "a,b,c" = listed names (incl required, skip disabled)
#
# State (under XDG_STATE_HOME, default ~/.local/state)
#   dotfiles/pick/<scope>.tsv      last selection (one chosen name per line)
#   dotfiles/pick/run-log.tsv      run history (ISO ts \t step \t exit \t dur_ms)
#   dotfiles/pick/logs/<step>.log  stdout+stderr from the last run of <step>
#
# Exit code: number of failed steps (0 on full success, 130 on user cancel).

import msg dict

# ---------- state paths ----------

pick::_state_dir() {
  printf '%s' "${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles/pick"
}

pick::_log_dir() {
  printf '%s' "$(pick::_state_dir)/logs"
}

pick::_state_file() {
  printf '%s/%s.tsv' "$(pick::_state_dir)" "${DOTFILES_PICK_SCOPE:-default}"
}

pick::_runlog_file() {
  printf '%s/run-log.tsv' "$(pick::_state_dir)"
}

# Replace `::` and `/` so the step name becomes a safe filename component.
pick::_safe_name() {
  local s="${1//:/-}"
  printf '%s' "${s//\//-}"
}

# ---------- item parsing ----------
#
# Parse one item spec, populating these globals (function-scoped because
# bash is dynamically scoped):
#   _pick_state   "required" | "disabled" | "normal"
#   _pick_name    function/command name
#   _pick_label   display label
#   _pick_reason  reason text (disabled items only)

pick::_parse_item() {
  local spec="$1"
  _pick_state="normal"
  _pick_reason=""

  case "$spec" in
    +*) _pick_state="required"; spec="${spec#+}" ;;
    \~*) _pick_state="disabled"; spec="${spec#\~}" ;;
  esac

  # split off ~reason if present
  if [[ "$spec" == *"~"* ]]; then
    _pick_reason="${spec#*\~}"
    spec="${spec%%\~*}"
  fi

  if [[ "$spec" == *"="* ]]; then
    _pick_name="${spec%%=*}"
    _pick_label="${spec#*=}"
  else
    _pick_name="$spec"
    _pick_label="$spec"
  fi
}

# ---------- terminal helpers ----------

pick::_cols() {
  local c
  c=$(tput cols 2>/dev/null) || c=80
  (( c < 20 )) && c=20
  printf '%d' "$c"
}

# Visible-length-aware truncation. Counts bytes (good enough for ASCII
# labels — UTF-8 wide chars are an edge we don't optimize for here).
pick::_trunc() {
  local s="$1" max="$2"
  if (( ${#s} <= max )); then
    printf '%s' "$s"
  else
    printf '%s…' "${s:0:max-1}"
  fi
}

pick::_tui_open() {
  printf '\033[?1049h'  # alt screen
  printf '\033[?25l'    # hide cursor
  stty -echo -icanon time 0 min 1 2>/dev/null || true
}

pick::_tui_close() {
  stty echo icanon 2>/dev/null || true
  printf '\033[?25h'    # show cursor
  printf '\033[?1049l'  # leave alt screen
}

# ---------- key reader ----------
#
# Returns a normalized key name on stdout. Recognized:
#   up down left right space enter q esc help search a n
#   any printable character (returned as-is, single byte)

pick::_read_key() {
  local k k2 k3
  IFS= read -rsn 1 k || { printf 'esc'; return; }
  case "$k" in
    $'\x1b')
      # ESC — could be alone, or the start of a CSI sequence (arrow keys
      # send \e[A etc). 50ms wait is well under perceptible latency but
      # plenty of time for the rest of the sequence to arrive.
      if IFS= read -rsn 1 -t 0.05 k2; then
        if [[ "$k2" == '[' ]]; then
          IFS= read -rsn 1 -t 0.05 k3 || k3=''
          case "$k3" in
            A) printf 'up' ;;
            B) printf 'down' ;;
            C) printf 'right' ;;
            D) printf 'left' ;;
            *) printf 'unknown' ;;
          esac
        else
          printf 'unknown'
        fi
      else
        printf 'esc'
      fi
      ;;
    ' ')         printf 'space' ;;
    $'\n'|$'\r') printf 'enter' ;;
    j)           printf 'down' ;;
    k)           printf 'up' ;;
    h)           printf 'left' ;;
    l)           printf 'right' ;;
    a)           printf 'a' ;;
    n)           printf 'n' ;;
    q)           printf 'q' ;;
    '?')         printf 'help' ;;
    *)           printf '%s' "$k" ;;
  esac
}

# ---------- rendering ----------
#
# Reads from these caller-locals (dynamically scoped):
#   _pick_n           number of items
#   _pick_names[]     parallel arrays
#   _pick_labels[]
#   _pick_states[]
#   _pick_reasons[]
#   pick_selected     dict name holding the current selection set

pick::_render() {
  local cursor="$1" cols selected_count=0
  cols=$(pick::_cols)

  printf '\033[H\033[2J'  # cursor home + clear screen
  printf '\033[1mSelect steps to run\033[0m\n'
  printf '\033[2m↑/↓ move · space toggle · a all · n none · enter run · q quit · ? help\033[0m\n\n'

  local i
  for ((i=0; i<_pick_n; i++)); do
    local name="${_pick_names[i]}"
    local label="${_pick_labels[i]}"
    local state="${_pick_states[i]}"
    local reason="${_pick_reasons[i]}"
    local mark prefix
    local is_selected=0

    case "$state" in
      required)
        mark=$'\033[1;32m🔒\033[0m '
        is_selected=1
        ;;
      disabled)
        mark=$'\033[2m·\033[0m  '
        ;;
      *)
        if dict::has pick_selected "$name"; then
          mark=$'\033[1;32m✔\033[0m  '
          is_selected=1
        else
          mark='   '
        fi
        ;;
    esac
    (( is_selected )) && selected_count=$((selected_count + 1))

    if (( i == cursor )); then
      prefix=$'\033[7m›\033[0m '
    else
      prefix='  '
    fi

    # Style the label by state (dim disabled, bold current)
    local styled_label="$label"
    case "$state" in
      disabled) styled_label=$'\033[2m'"$label"$'\033[0m' ;;
      *) (( i == cursor )) && styled_label=$'\033[1m'"$label"$'\033[0m' ;;
    esac

    # Reason suffix (disabled only)
    local reason_suffix=''
    if [[ "$state" == "disabled" && -n "$reason" ]]; then
      reason_suffix=$' \033[2m('"$reason"$')\033[0m'
    fi

    # Compose; truncation is approximate (treats ANSI as zero width by
    # operating on the visible text only, then re-applying styles wraps
    # the truncated text).
    local visible="${label}"
    [[ "$state" == "disabled" && -n "$reason" ]] && visible="${visible} (${reason})"
    local budget=$((cols - 6))   # 2 prefix + 3 mark + 1 spacer
    if (( ${#visible} > budget )); then
      visible=$(pick::_trunc "$visible" "$budget")
      printf '%s%s%s\n' "$prefix" "$mark" "$visible"
    else
      printf '%s%s%s%s\n' "$prefix" "$mark" "$styled_label" "$reason_suffix"
    fi
  done

  printf '\n\033[2m%d selected · %d total\033[0m\n' "$selected_count" "$_pick_n"
}

pick::_render_help() {
  printf '\033[H\033[2J'
  printf '\033[1mKeybindings\033[0m\n\n'
  cat <<'EOF'
  ↑ / k          move up
  ↓ / j          move down
  space          toggle the highlighted item
  a              select all eligible items
  n              clear all eligible items (required stay selected)
  enter          run the selection
  q / esc        cancel without running
  ?              this help

EOF
  printf '\033[2mPress any key to return.\033[0m\n'
  pick::_read_key >/dev/null
}

# ---------- non-interactive resolution ----------

pick::_resolve_noninteractive() {
  local mode="${DOTFILES_PICK:-}"

  # No env, no tty → loud failure.
  if [[ -z "$mode" ]] && ! { [[ -t 0 ]] && [[ -t 1 ]]; }; then
    msg::error "pick: non-interactive shell requires DOTFILES_PICK env var (set to 'all', 'none', or a comma-separated list of item names)"
    return 2
  fi

  # Required items: always selected.
  local i
  for ((i=0; i<_pick_n; i++)); do
    if [[ "${_pick_states[i]}" == "required" ]]; then
      dict::set pick_selected "${_pick_names[i]}" 1
    fi
  done

  case "$mode" in
    all)
      for ((i=0; i<_pick_n; i++)); do
        if [[ "${_pick_states[i]}" == "normal" ]]; then
          dict::set pick_selected "${_pick_names[i]}" 1
        fi
      done
      ;;
    none|"")
      for ((i=0; i<_pick_n; i++)); do
        if [[ "${_pick_states[i]}" == "normal" ]]; then
          dict::del pick_selected "${_pick_names[i]}"
        fi
      done
      ;;
    *)
      # Comma-separated whitelist. Clear normals first, then add the named.
      for ((i=0; i<_pick_n; i++)); do
        if [[ "${_pick_states[i]}" == "normal" ]]; then
          dict::del pick_selected "${_pick_names[i]}"
        fi
      done
      local IFS=','
      local picks
      read -ra picks <<<"$mode"
      unset IFS
      local p found j
      for p in "${picks[@]}"; do
        # strip whitespace
        p="${p# }"; p="${p% }"
        [[ -z "$p" ]] && continue
        found=0
        for ((j=0; j<_pick_n; j++)); do
          if [[ "${_pick_names[j]}" == "$p" ]]; then
            found=1
            if [[ "${_pick_states[j]}" == "disabled" ]]; then
              msg::error "pick: '$p' is disabled, skipping"
            else
              dict::set pick_selected "$p" 1
            fi
            break
          fi
        done
        (( found )) || msg::error "pick: warning: unknown item '$p' in DOTFILES_PICK"
      done
      ;;
  esac
  return 0
}

# ---------- state load / save ----------

pick::_load_last_selection() {
  # Reads the last-selection file (if any) into a fresh dict
  # `pick_last_selection`.
  dict::clear pick_last_selection
  local file
  file="$(pick::_state_file)"
  [[ -r "$file" ]] || return 0
  local line
  while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    dict::set pick_last_selection "$line" 1
  done < "$file"
  return 0
}

pick::_save_selection() {
  local dir file
  dir="$(pick::_state_dir)"
  file="$(pick::_state_file)"
  mkdir -p "$dir" 2>/dev/null || return 0  # state save is best-effort
  : > "$file"
  local i
  for ((i=0; i<_pick_n; i++)); do
    local nm="${_pick_names[i]}"
    [[ "${_pick_states[i]}" == "disabled" ]] && continue
    if dict::has pick_selected "$nm"; then
      printf '%s\n' "$nm" >> "$file"
    fi
  done
  return 0
}

pick::_log_run() {
  # Args: name exit duration_ms
  local dir file
  dir="$(pick::_state_dir)"
  file="$(pick::_runlog_file)"
  mkdir -p "$dir" 2>/dev/null || return 0
  local ts
  ts=$(date -u +%FT%TZ 2>/dev/null || date)
  printf '%s\t%s\t%d\t%d\n' "$ts" "$1" "$2" "$3" >> "$file"
  return 0
}

# ---------- step execution ----------

pick::_run_step() {
  # Args: name label
  local name="$1" label="$2"
  local log_dir log_file safe
  log_dir="$(pick::_log_dir)"
  mkdir -p "$log_dir" 2>/dev/null || true
  safe="$(pick::_safe_name "$name")"
  log_file="$log_dir/$safe.log"

  msg::heading "→ $label"

  local start end dur_ms exit_code

  # bash 3.2 has neither EPOCHREALTIME nor `date +%s%N` on macOS BSD date.
  # Fall back to whole-second granularity if nanosecond date is missing.
  start=$(date +%s%N 2>/dev/null)
  if [[ -z "$start" || "$start" == *N ]]; then
    start=$(($(date +%s) * 1000000000))
  fi

  if [[ "${DOTFILES_VERBOSE:-0}" == "1" ]]; then
    "$name" 2>&1 | tee "$log_file"
    exit_code=${PIPESTATUS[0]}
  else
    "$name" >"$log_file" 2>&1
    exit_code=$?
  fi

  end=$(date +%s%N 2>/dev/null)
  if [[ -z "$end" || "$end" == *N ]]; then
    end=$(($(date +%s) * 1000000000))
  fi
  dur_ms=$(( (end - start) / 1000000 ))

  pick::_log_run "$name" "$exit_code" "$dur_ms"

  if (( exit_code == 0 )); then
    msg::success "$label  (${dur_ms}ms)"
  else
    msg::error "$label  (exit $exit_code · log: $log_file)"
  fi
  return "$exit_code"
}

pick::_run_selected() {
  local i ran=0 failed=0 skipped=0 selected=0

  for ((i=0; i<_pick_n; i++)); do
    dict::has pick_selected "${_pick_names[i]}" && selected=$((selected + 1))
  done

  if (( selected == 0 )); then
    msg::heading "Nothing to run."
    return 0
  fi

  for ((i=0; i<_pick_n; i++)); do
    local nm="${_pick_names[i]}"
    local lbl="${_pick_labels[i]}"
    if ! dict::has pick_selected "$nm"; then
      skipped=$((skipped + 1))
      continue
    fi
    if pick::_run_step "$nm" "$lbl"; then
      ran=$((ran + 1))
    else
      failed=$((failed + 1))
      ran=$((ran + 1))
    fi
  done

  msg::heading "Summary"
  printf '  ✔ %d ran   ✖ %d failed   ↪ %d skipped\n' "$ran" "$failed" "$skipped"
  if (( failed > 0 )); then
    printf '  logs: %s\n' "$(pick::_log_dir)"
  fi

  return "$failed"
}

# ---------- main entry ----------

pick() {
  local _pick_n=0
  local _pick_names=() _pick_labels=() _pick_states=() _pick_reasons=()
  local _pick_state _pick_name _pick_label _pick_reason

  local item
  for item in "$@"; do
    pick::_parse_item "$item"
    _pick_names+=("$_pick_name")
    _pick_labels+=("$_pick_label")
    _pick_states+=("$_pick_state")
    _pick_reasons+=("$_pick_reason")
    _pick_n=$((_pick_n + 1))
  done

  (( _pick_n > 0 )) || return 0

  # Build initial selection: required + last-run picks.
  dict::clear pick_selected
  pick::_load_last_selection

  local i
  for ((i=0; i<_pick_n; i++)); do
    local s="${_pick_states[i]}"
    local nm="${_pick_names[i]}"
    if [[ "$s" == "required" ]]; then
      dict::set pick_selected "$nm" 1
    elif [[ "$s" == "normal" ]] && dict::has pick_last_selection "$nm"; then
      dict::set pick_selected "$nm" 1
    fi
  done

  # Non-interactive (env-driven or no tty)
  if [[ -n "${DOTFILES_PICK:-}" ]] || ! { [[ -t 0 ]] && [[ -t 1 ]]; }; then
    pick::_resolve_noninteractive || return $?
    pick::_save_selection
    pick::_run_selected
    return $?
  fi

  # Interactive TUI loop
  pick::_tui_open
  trap 'pick::_tui_close' EXIT INT TERM

  local cursor=0 result=run
  while true; do
    pick::_render "$cursor"
    local key
    key=$(pick::_read_key)
    case "$key" in
      up)
        (( cursor > 0 )) && cursor=$((cursor - 1))
        ;;
      down)
        (( cursor < _pick_n - 1 )) && cursor=$((cursor + 1))
        ;;
      space)
        local s="${_pick_states[cursor]}"
        local nm="${_pick_names[cursor]}"
        if [[ "$s" == "normal" ]]; then
          if dict::has pick_selected "$nm"; then
            dict::del pick_selected "$nm"
          else
            dict::set pick_selected "$nm" 1
          fi
        fi
        ;;
      a)
        for ((i=0; i<_pick_n; i++)); do
          [[ "${_pick_states[i]}" == "normal" ]] && \
            dict::set pick_selected "${_pick_names[i]}" 1
        done
        ;;
      n)
        for ((i=0; i<_pick_n; i++)); do
          [[ "${_pick_states[i]}" == "normal" ]] && \
            dict::del pick_selected "${_pick_names[i]}"
        done
        ;;
      help)
        pick::_render_help
        ;;
      enter)
        result=run
        break
        ;;
      q|esc)
        result=cancel
        break
        ;;
    esac
  done

  pick::_tui_close
  trap - EXIT INT TERM

  if [[ "$result" == "cancel" ]]; then
    msg::heading "Cancelled."
    return 130
  fi

  pick::_save_selection
  pick::_run_selected
}
