#!/bin/bash

dock=org.gnome.shell.extensions.dash-to-dock

gsettings set "$dock" show-apps-at-top true
gsettings set "$dock" click-action 'minimize'
