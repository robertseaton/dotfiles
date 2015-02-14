#! /usr/bin/env bash

SCRIPT_HOME=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

function add {
	ln -s $SCRIPT_HOME/$1 ~/$1
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
