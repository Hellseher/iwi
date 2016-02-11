#! /bin/sh
# File          :  iwi_parser.sh
# Created       :  Sun 10 Jan 2016 23:32:18
# Last Modified :  Wed 27 Jan 2016 00:11:19
# Maintainer    :  sharlatan, <sharlatanus@gmail.com>
# License       :  Same as Bash (GPL)
# Credits:
#
#..:: Description ::..
# Parse the file for non build in functions

SCRIPT="$1"
BUILT_IN="./bash-built-in"

PARSED="$(cat "$SCRIPT" \
    | grep -v "#"  \
    | grep -vP "^$" \
    | grep -oP "(\b\w+)" \
    | grep -v "[A-Z]" \
    | grep -v "_" \
    | grep -vP "\d" \
    | grep -v "^.$")"

if grep -Fxq "$FILENAME" my_list.txt
then
    # code if found
else
    # code if not found
fi

