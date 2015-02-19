sudo systemctl start network.service
sudo iptables --flush
start-pulseaudio-x11
xrandr --output DVI-1 --right-of DVI-0

xmodmap ~/.xmodmap &
xrdb ~/.Xresources &
xfce4-terminal --window -e "tmux attach -d" & disown
lxsession &

pidof mpdas >& /dev/null
if [ $? -ne 0 ]; then
 mpdas &
fi

xsetroot -cursor_name left_ptr
compton -cCfb # --vsync opengl
feh --bg-scale Desktop/simpledesktops.com.png
exec xmonad
