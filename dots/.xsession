#!/usr/bin/env bash


#############################################
#                                           #
#               INITIALIZE DWM              #
#                                           #
#############################################


#------------- Load X settings -------------#

xrdb -merge ~/.Xresources

#------- Set the time in the dwm bar -------#

while true; do
    xsetroot -name "$( date +"%A  ·  %F  · %r" )"
    sleep 1m
done &

#-------------- Set wallpaper --------------#

feh --bg-fill ~/Pictures/Wallpapers/gensingen-plant.jpg

#------------ Enable compositing -----------#

compton -bcC --focus-exclude "x = 0 && y = 0 && override_redirect = true"

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
