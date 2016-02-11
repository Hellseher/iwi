#! /bin/sh
# File          :  comparator.sh
# Created       :  Mon 11 Jan 2016 00:22:25
# Last Modified :  Mon 11 Jan 2016 00:28:16
# Maintainer    :  sharlatan, <sharlatanus@gmail.com>
# License       :  Same as Bash (GPL)
# Credits:
#
#..:: Description ::..
#
# 

for string in $(cat ./parsed); do
    if grep -Fxq $string "./built-in"; then
        echo ++ $string
    else
        echo $string
    fi

done
