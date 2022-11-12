export BROWSER=firefox
export AUR=paru
export EDITOR=nvim
export TERMINAL=alacritty
export HISTCONTROL=ignoredups:erasedups
export TERM="xterm-256color"
[[ $- != *i* ]] && return

export PATH=$HOME/.local/bin:$PATH
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CACHE_HOME=$HOME/.cache
export XINITRC=${XDG_CONFIG_HOME:-$HOME/.config}/x11/xinitrc

[ "$(tty)" = "/dev/tty1" ] && ! pidof -s Xorg >/dev/null 2>&1 && exec startx "$XINITRC"
