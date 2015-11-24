#! /bin/sh
# File          :  test.sh
# Created       :  Mon 23 Nov 2015 22:33:46
# Last Modified :  Mon 23 Nov 2015 22:37:48
# Maintainer    :  sharlatan, <sharlatanus@gmail.com>
# License       :  Same as Bash (GPL)
# Credits:
#
#..:: Description ::..
#
# 

for pkg in $(yum dnf apt-get); do
    command -v $pkg
done
