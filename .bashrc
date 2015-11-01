#! /bin/sh
# File          :  .bashrc
#
# Created       :  Mon 08 Dec 2014 19:31:26
# Last Modified :  Wed 28 Oct 2015 21:35:10
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
export HISTCONTROL=ignoredups: erasedups
export EDITOR=vim
#export PATH:$PATH:~/.lein/lein
#export PATH=$PATH:$GOROOT/bin
# export 256 colors to terminal
if [[ "$TERM" == "xterm" ]]; then
    export TERM=xterm-256color
fi
#<END Of EXPORTS >-----------------------------------------------------------}}}

# -=:[ SETTINGS ]:=-                                                         {{{
# set vi like keyboard movement
set -o vi           # se

PROMPT_COMMAND='pwd'
PS1='[ \[\e[33m\]\u@\H\[\e[m\] ]\$: ' 

# History hacks
shopt -s histappend
export PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"
# <END OF # SETTINGS>--------------------------------------------------------}}}

#-=:[ FUNCTIONS ]:=-                                                          {{{
#-=[ fzf setup ]=---------------------------------------------------------------
# TODO add case condition > fd (-a -f)

# ---[ fzf 
function fe() {
    # fe [fuzzy pattern] open the selected file with the default editor
    # -Bypass fuzzy finder if ther's only one match (--selected-1)
    # - Exit if ther's no match (--exit-0)

    cd
    local file
    file=$(fzf-tmux -d 20% --query="$1" --select-1 --exit-0)
    [ -n "$file" ] && ${EDITOR: -vim} "$file"
}

function fd() {
    # fd - cd to selected directory (excluding hiden)

    cd
    local dir
    dir=$(find ${1:-*} -path '*/\.' -prune \
        -o -type d -print 2> /dev/null | fzf-tmux -d 20% +m) &&
    cd "$dir"
}

function fda() {
    # fda - cd to any (including hiden) directory

    cd
    local dir
    dir=$(find ${1:-.} -type d 2> /dev/null | fzf-tmux -d 20% +m) &&
        cd "$dir"
}

function fdf() {
    # fda cd into directory of the selected file

    cd
    local file
    local dir
    file=$(fzf-tmux -d 20% +m -q "$1") && dir=$(dirname "$file") &&
        cd "$dir"
}
# --[ end of fzf ]--

# --[ percol ]--
function mp () {
    # Read man with percol
     tmux split-window 'man $(ls /usr/bin | percol)'
}
function m() {
    #function_body
    man $1 | percol
}
function gpom () {
    git push origin master
}

function show() {
    # Copy password from the list
    if [[ $# -ne 1 ]]; then
        echo Enter at least one argument
        exit 1
    fi
    grep $1 ~/Documents/txt/overrall | awk -F: '{print $3}' | tr -d '\n' | xclip
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
alias aplay='ansible-playbook'
#<END OF ALIAS>--------------------------------------------------------------}}}


if [[ ! $TERM =~ screen ]]; then
        exec tmux
fi
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

#-=:[ CREDITS ]:=-{{{
# --[ GitHub
# https://github.com/junegunn/fzf
#<END OF CREDITS> }}}
