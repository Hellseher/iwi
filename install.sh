#! /bin/sh
# File          :  install.sh
# Created       :  Fri 23 Oct 2015 23:15:47
# Last Modified :  Fri 23 Oct 2015 23:35:19
# Maintainer    :  sharlatan, <sharlatanus@gmail.com>
# License       :  Same as Bash (GPL)
# Credits:
#
#..:: Description ::..
#
#
ABS_PATH=$(realpath $0 | grep -Po "(?:(\S)*)(dotfiles)")
LS_HF=$(ls -a $ABS_PATH | grep "^\.")



for dotfile in $LS_HF; do
    ln -s $ABS_PATH/$dotfile $HOME
done
