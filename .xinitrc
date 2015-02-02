sudo systemctl start network.service
sudo iptables --flush
start-pulseaudio-x11 > pulse.log
xrandr --output DVI-1 --right-of DVI-0
sshfs adbge@adbge.webfactional.com: ~/mnt/rs.io
xmodmap ~/.xmodmap &
xrdb ~/.Xresources &
urxvt -e zsh -c "tmux attach -d" & disown

pidof mpdas >& /dev/null
if [ $? -ne 0 ]; then
 mpdas &
fi

xsetroot -cursor_name left_ptr
compton -cCfb
feh --bg-scale Desktop/simpledesktops.com.png
exec xmonad
