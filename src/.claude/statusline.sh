#!/usr/bin/env bash

command -v jq >/dev/null 2>&1 || exit 0

input=$(cat)

IFS=$'\t' read -r model dir ctx five five_at week week_at added removed <<<"$(
  jq -r '[
    (.model.display_name // "?"),
    (.workspace.current_dir // .cwd // "?"),
    ((.context_window.used_percentage // 0) | floor),
    ((.rate_limits.five_hour.used_percentage // -1) | floor),
    (.rate_limits.five_hour.resets_at // 0),
    ((.rate_limits.seven_day.used_percentage // -1) | floor),
    (.rate_limits.seven_day.resets_at // 0),
    (.cost.total_lines_added // 0),
    (.cost.total_lines_removed // 0)
  ] | @tsv' <<<"$input" 2>/dev/null
)"
[ -n "$model" ] || exit 0

epoch_fmt() {
  date -r "$1" "+$2" 2>/dev/null || date -d "@$1" "+$2" 2>/dev/null
}

meter() {
  local out
  out=$(printf '%s %s%%%s' "$1" "$2" "$3")
  if [ "$2" -ge 80 ]; then
    printf '\033[31m%s\033[0m' "$out"
  elif [ "$2" -ge 60 ]; then
    printf '\033[33m%s\033[0m' "$out"
  else
    printf '\033[2m%s\033[0m' "$out"
  fi
}

join() {
  printf '%s' "$1"
  shift
  local s
  for s in "$@"; do
    printf ' \033[2m|\033[0m %s' "$s"
  done
}

vis_len() {
  local plain
  plain=$(printf '%s' "$1" | sed $'s/\x1b\\[[0-9;]*m//g')
  printf '%s' "${#plain}"
}

top=$(git -C "$dir" rev-parse --show-toplevel 2>/dev/null)
if [ -n "$top" ]; then
  loc=${top##*/}
  sub=${dir#"$top"}
  if [ ${#sub} -gt 24 ]; then
    sub="/.../${sub##*/}"
  fi
  loc_seg=$(printf '\033[34m%s\033[0m\033[2m%s\033[0m' "$loc" "$sub")
else
  p=$dir
  case $dir in
    "$HOME") p='~' ;;
    "$HOME"/*) p="~${dir#"$HOME"}" ;;
  esac
  if [ ${#p} -gt 40 ]; then
    tail=${p##*/}
    rest=${p%/*}
    p="~/.../${rest##*/}/$tail"
  fi
  loc_seg=$(printf '\033[34m%s\033[0m' "$p")
fi

left_segs=()
[ "${AGENT_SANDBOX:-}" = 1 ] && left_segs+=("$(printf '\033[33m[sbx]\033[0m')")
left_segs+=("$(printf '\033[36m%s\033[0m' "$model")")
left_segs+=("$loc_seg")

branch=$(git -C "$dir" symbolic-ref --short HEAD 2>/dev/null \
  || git -C "$dir" rev-parse --short HEAD 2>/dev/null)
if [ -n "$branch" ]; then
  dirty=''
  [ -n "$(git -C "$dir" status --porcelain 2>/dev/null | head -1)" ] && dirty='*'
  track=''
  read -r behind ahead <<<"$(git -C "$dir" rev-list --left-right --count '@{u}...HEAD' 2>/dev/null)"
  [ "${ahead:-0}" -gt 0 ] && track="$track ^$ahead"
  [ "${behind:-0}" -gt 0 ] && track="$track v$behind"
  left_segs+=("$(printf '\033[35m%s%s\033[0m\033[2m%s\033[0m' "$branch" "$dirty" "$track")")
fi

right_segs=("$(meter ctx "$ctx" '')")
if [ "$five" -ge 0 ]; then
  at=''
  [ "$five_at" -gt 0 ] && at=" @$(epoch_fmt "$five_at" %H:%M)"
  right_segs+=("$(meter 5h "$five" "$at")")
fi
if [ "$week" -ge 0 ]; then
  at=''
  if [ "$week_at" -gt 0 ]; then
    if [ $((week_at - $(date +%s))) -lt 86400 ]; then
      at=" @$(epoch_fmt "$week_at" %H:%M)"
    else
      at=" @$(epoch_fmt "$week_at" %a)"
    fi
  fi
  right_segs+=("$(meter wk "$week" "$at")")
fi
if [ "$((added + removed))" -gt 0 ]; then
  right_segs+=("$(printf '\033[2m+%s/-%s\033[0m' "$added" "$removed")")
fi

left=$(join "${left_segs[@]}")
right=$(join "${right_segs[@]}")
len_l=$(vis_len "$left")
len_r=$(vis_len "$right")

margin=4
cols=${COLUMNS:-0}
if [ "$cols" -le 0 ]; then
  printf '%s \033[2m|\033[0m %s' "$left" "$right"
elif [ $((len_l + len_r + margin + 2)) -le "$cols" ]; then
  printf '%s%*s%s' "$left" $((cols - margin - len_l - len_r)) '' "$right"
else
  printf '%s\n' "$left"
  if [ $((len_r + margin)) -le "$cols" ]; then
    printf '%*s%s' $((cols - margin - len_r)) '' "$right"
  else
    printf '%s' "$right"
  fi
fi
