# all this stuff was autogenerated and i have no idea what it all means
# just like life
zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' '' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}'
zstyle ':completion:*' menu select=5
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/robb/.zshrc'

autoload -Uz compinit
compinit

autoload -U colors
colors

case "$TERM" in
	"dumb")
    PS1="> "
    ;;
	xterm*|rxvt*|eterm*|screen*)
    export PROMPT="%{$fg[blue]%}[@%m %c]$ %{$reset_color%}"
    ;;
*)
    export PROMPT="%{$fg[blue]%}[@%m %c]$ %{$reset_color%}"
    ;;
esac

# the best prompt
# example: [@robb downloads]$



HOSTNAME=$( hostname )

if [[ $HOSTNAME == 'jupiter' ]]; then
    export PROMPT="%{$fg_bold[yellow]%}[@%m %c]$ %{$reset_color%}"
elif [[ $HOSTNAME == 'neptune' ]]; then
    export PROMPT="%{$fg_bold[blue]%}[@%m %c]$ %{$reset_color%}"
else
    export PROMPT="%{$fg_bold[red]%}[@%m %c]$ %{$reset_color%}"
fi

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export TERM=xterm-256color

# play 1080p porn smoothly
export LIBVA_DRIVER_NAME=vdpau

# I LOVE HISTORY
# some people might say i love history too much
# i have never met these people
HISTFILE=~/.zsh_history
HISTSIZE=100000000
SAVEHIST=100000000

# the less i have to hit tab
# the better
setopt menu_complete

# because i love history, appendixes, and appending things
setopt appendhistory

# synchronize history across shells
# sweet relief from the voices in my head
setopt inc_append_history
setopt share_history
 
# because someday i will learn advanced pattern matching
# it sounds very useful, like regexps
setopt extendedglob

# don't assume i want to cd, that's fucking rude
unsetopt autocd

# seriously why does this shit even exist
unsetopt beep

# DON'T YOU YELL AT ME WHEN YOU CAN'T FIND A MATCH
# THAT'S YOUR PROBLEM, _NOT_ MINE
unsetopt nomatch

# when i put a process in the background,
# it's code for "fuck off and leave me alone"
unsetopt notify

# i feel bad for whoever implemented vi mode
# surely an alcoholic by now
bindkey -e

# matches current input to history items
# bind to up arrow and down arrow
# this will change your life and make traffic bearable

bindkey '^[OA' history-beginning-search-backward
bindkey '^[OB' history-beginning-search-forward

# same but for term running in emacs
bindkey '^[[A' history-beginning-search-backward
bindkey '^[[B' history-beginning search-forward


# colored man pages
man() {
    env LESS_TERMCAP_mb=$'\E[01;31m' \
        LESS_TERMCAP_md=$'\E[01;38;5;74m' \
        LESS_TERMCAP_me=$'\E[0m' \
        LESS_TERMCAP_se=$'\E[0m' \
        LESS_TERMCAP_so=$'\E[38;5;246m' \
        LESS_TERMCAP_ue=$'\E[0m' \
        LESS_TERMCAP_us=$'\E[04;38;5;146m' \
        man "$@"
}

# aliases
alias ls='ls --color=auto'
alias add="yaourt -S "
# alias emacs="TERM=xterm emacs -nw"
alias conkeror="GTK2_RC_FILES=~/.gtkrc-2.0.conkeror conkeror"
alias dmesg="dmesg --color=always"
alias muhsnapshots="sudo zfs list -t snapshot | grep"
ezcp() {
    sudo cp -r $1 $2 & disown
}

export 
PATH=/home/rps/sh:/home/rps/bin:/home/rps/.gem/ruby/2.0.0/bin:/home/rps/go/bin:/usr/local/bin:/usr/local/sbin:$HOME/Library/Haskell/bin:~/.cabal/bin:~/.scripts:/usr/lib32/smlnj/bin:/home/rps/.gem/ruby/2.1.0/bin:$PATH

export SMLNJ_HOME=/usr/lib32/smlnj
export GOROOT=$HOME/go
export CLASSPATH=/opt/jars/*
export VDPAU_DRIVER="r600"
export EDITOR="emacsclient"
export LANG="en_US.UTF-8"

