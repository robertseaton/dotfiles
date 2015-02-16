#! /usr/bin/env zsh

SCRIPT_HOME=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

function add {
    if [[ ! -e ~/$1 ]]
    then
       ln -sn $SCRIPT_HOME/$1 ~/$1
    fi
}

add .zshrc
add .emacs.d
add .rtorrent.rc
add .xmodmap
add .xinitrc
add .xmonad
add .Xresources
add .Xdefaults
add .fonts.conf
add .i3
add .i3status.conf
add .config
add .ncmpcpp
add .conkerorrc
add .tmux.conf
