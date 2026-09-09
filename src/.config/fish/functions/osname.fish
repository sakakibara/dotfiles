function osname -d "Get the current OS name"
    set -l osname
    switch (uname)
        case Darwin
            set osname macos
        case CYGWIN'*'
            set osname cygwin
        case Linux
            if test -f /etc/os-release
                set osname (awk -F= '$1=="ID" { print tolower($2) ;}' "/etc/os-release" | string trim -c '"')
            else if command -v lsb_release &>/dev/null
                set osname (lsb_release -si | string lower)
            else if test -f /etc/lsb-release
                set osname (awk -F= '$1=="DISTRIB_ID" { print tolower($2) ;}' "/etc/lsb-release" | string trim -c '"')
            else if test -f /etc/fedora-release
                set osname fedora
            else if test -f /etc/debian_version
                set osname debian
            end
    end
    if test "$argv[1]" = init
        echo "set -gx OSNAME $osname"
    else
        echo $osname
    end
end
