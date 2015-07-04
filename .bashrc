#! /bin/sh
# File          :  .bashrc
#
# Created       :  Mon 08 Dec 2014 19:31:26
# Last Modified :  Fri 19 Jun 2015 22:46:01
# Maintainer    :  sharlatan, <sharlatanus@gmail.com>
# License       :  Same as Bash (GPL)
# Credits       :  See CREDITS section
#
# -=:[ DESCRIPTION ]:=-
#
# All setting for .bashrc file.
# <END OF # DESCRIPTION>-------------------------------------------------------

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi


# -=[ EXPORTS ]=-                                                            {{{
#export GOROOT=$HOME/go
export EDITOR=vim
#export PATH=$PATH:$GOROOT/bin
#< END Of EXPORTS >-                                                         }}}

# -=:[ SETTINGS ]:=-                                                         {{{
# set vi like keyboard movement
set -o vi           # se

PROMPT_COMMAND='pwd'
PS1=' \[\e[1;33m\](\#:\!)\$:\[\e[0m\] '

# History hacks
export HISTCONTROL=ignoredups: erasedups
shopt -s histappend
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
# <END OF # SETTINGS>--------------------------------------------------------}}}

#-=:[ FUNCTIONS ]:=-                                                          {{{
# Add all extra functionality here.

#--==[ pht ]==------------------------------------------------------------------
# Bunch of function to work with photos.
#


function picsort(){
    # Sort pictures in local directory by created date
    read -p "Sort images? (Yy|Nn): " answer
    if [[ $answer = y  ||  $answer = Y ]]; then
        exiftool -r '-Directory<DateTimeOriginal' \
            -d %d_%B_%Y . && find ./ -type d -empty \
                -exec rm -rf {}\;
    fi
}


function extract () {
    # ease way to extract archives
    if [ -f $1 ]; then
        case $1 in
            *.tat.bz2   ) tar xvjf $1
                ;;
            *.tat.gz    ) tar xvzf $1
                ;;
            *.bz2       ) bunzip2 $1
                ;;
            *.rar       ) unrar x $1
                ;;
            *.gz        ) gunzip $1
                ;;
            *.tar       ) tar xvf $1
                ;;
            *.tbz2      ) tar xvjf $1
                ;;
            *.tgz       ) tar xvzf $1
                ;;
            *.zip       ) unzip $1
                ;;
            *.Z         ) uncompress $1
                ;;
            *.7z        ) 7z x  $1
                ;;
            *           ) echo "Cant extract '$1'..."
                ;;
        esac
    else
        echo "'$1' is not a valid file..."
    fi
}
#-=[ fzf setup ]=---------------------------------------------------------------
# TODO add case condition > fd (-a -f)

function fe() {
    # fe [fuzzy pattern] open the selected file with the default editor
    # -Bypass fuzzy finder if ther's only one match (--selected-1)
    # - Exit if ther's no match (--exit-0)

    local file
    file=$(fzf-tmux -d 20% --query="$1" --select-1 --exit-0)
    [ -n "$file" ] && ${EDITOR: -vim} "$file"
}

function fd() {
    # fd - cd to selected directory (excluding hiden)

    local dir
    dir=$(find ${1:-*} -path '*/\.' -prune \
        -o -type d -print 2> /dev/null | fzf-tmux -d 20% +m) &&
    cd "$dir"
}

function fda() {
    # fda - cd to any (including hiden) directory

    local dir
    dir=$(find ${1:-.} -type d 2> /dev/null | fzf-tmux -d 20% +m) &&
        cd "$dir"
}

function fdf() {
    # fda cd into directory of the selected file

    local file
    local dir
    file=$(fzf-tmux -d 20% +m -q "$1") && dir=$(dirname "$file") &&
        cd "$dir"
}
#
#function lsd() {
#    # list of recently visited dirs
#
#    if [ -e /tmp/lsd ]; then
#        rm /tmp/lsd
#    else touch /tmp/lsd
#    fi
#
#    history|awk '$0 !~ /history/' | awk '$0 !~ /\"/'|awk '/cd \// || /cd \~/ ||
#        /pushd \// || /pushd \~/' | awk '{print $3}' |
#        sort -u | nl | tee /tmp/lsd
#
#    read -p "> " where
#    TOGO=$(sed -n "${where}p" /tmp/lsd | awk -F" " '{print $2}')
#    eval cd "$TOGO"
#}
#
#<END OF FUNCIONS>-----------------------------------------------------------}}}

#-=:[ ALIAS ]:=-                                                             {{{
alias c='clear'
alias em='emacs -nw'
#<END OF ALIAS>--------------------------------------------------------------}}}


if [[ ! $TERM =~ screen ]]; then
        exec tmux
fi
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

#-=:[ CREDITS ]:=-{{{
# --[ GitHub
# https://github.com/junegunn/fzf
#<END OF CREDITS> }}}
