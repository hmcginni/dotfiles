#!/usr/bin/env bash


#############################################
#                                           #
#               INITIALIZE DWM              #
#                                           #
#############################################


#------- Set the time in the dwm bar -------#

killall setdwmtitle.sh
.scripts/setdwmtitle.sh &

#-------------- Set wallpaper --------------#

while true; do
    feh --bg-fill ~/Dropbox/Walls/002.png
    sleep 1m
done &

#------------ Enable compositing -----------#

compton -bcCf -o 0.8 -r 16 -D 5 --inactive-dim 0.25 #--focus-exclude "xo = 0 && y = 0 && override_redirect = true"

#------------------ Other ------------------#

nm-applet &
wmname LG3D

#---------------- Start DWM ----------------#

# exec dwm

# TEST: relaunch DWM if the binary changes, otherwise bail
csum=$(sha1sum $(which dwm))
new_csum=""
while true
do
    if [ "$csum" != "$new_csum" ]
    then
        csum=$new_csum
        dwm
    else

        exit 0
    fi
    new_csum=$(sha1sum $(which dwm))
    sleep 0.5
done
