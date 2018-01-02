#!/usr/bin/env bash
GROUPS=(
'@Development tools'
)
PKGS=(
    fish
    tmux
    redhat-lsb
    fontconfig-devel
    global
    mingw-w64-tools
    perf
    python2-pip
    python3-pip
    xclip
    ack
    the_silver_searcher
    jq
    rawtherapee
    thunderbird
    gimp
    inkscape
    krita
    freemind
    gnome-tweak-tool
    libselinux-python
    curlpp
)

main()
{
    for pk in ${PKGS[*]}; do
        printf -- "- { name: \"%s\", desc: \"%s\" }\n" "$pk" "$(dnf info $pk | grep -m1 Summary | sed -e 's/.*Summary *\://'g)"
    done
}

main "$@"

# End of fedora_pkgs.sh
