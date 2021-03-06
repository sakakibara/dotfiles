function get_os -d "Get the current OS name"
  set -l os
  switch (uname)
  case Darwin
    set os macos
  case CYGWIN'*'
    set os cygwin
  case Linux
    if test -f /etc/os-release
      set os (awk -F= '$1=="ID" { print tolower($2) ;}' "/etc/os-release")
    else if command -v lsb_release &>/dev/null
      set os (lsb_release -si)
    else if test -f /etc/debian_version
      set os "debian"
    end

    if test -n "$os" && test -f /proc/version \
        && grep -q "Microsoft" "/proc/version"
      set os (string join "" $os _wsl)
    end
  end

  if test -n "$os"
    echo $os
    return 0
  end
  return 1
end
