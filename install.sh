#! /bin/sh
# File          :  install.sh
# Created       :  Fri 23 Oct 2015 23:15:47
# Last Modified :  Mon 23 Nov 2015 22:33:40
# Maintainer    :  sharlatan, <sharlatanus@gmail.com>
# License       :  Same as Bash (GPL)
# Credits:
#
#..:: Description ::..
# Depandancies: GNU core utils
#
ABS_PATH=$(realpath $0 | grep -Po "(?:(\S)*)(dotfiles)")
LS_HF=$(ls -a $ABS_PATH | grep "^\.")
INSTALL_PKG="vim tmux guake fish"

if [[ $UID -ne 0 ]]; then
    echo Run as root.
    exit 0
fi



yum_install() {
    yum update
    yum install $1
}

dnf_install() {
    dnf update
    dnf install $1
}

apt_install() {
    apt-get update
    apt-get install $1
}
# ---[ main ]---

case what_pkg in
    yum) yum_install
        ;;
    dnf) dnf_install
        ;;
    apt) apt_install
        ;;
        ecas

for dotfile in $LS_HF; do
    ln -sf $ABS_PATH/$dotfile $HOME
done
echo ${ABS_PATH}
