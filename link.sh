#! /usr/bin/env bash

SCRIPT_HOME=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

function add {
	ln -sf $SCRIPT_HOME/$1 ~/$1
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
