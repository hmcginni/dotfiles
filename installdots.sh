#!/bin/bash

# Install packages #
####################

sudo apt update && sudo apt upgrade && sudo apt autoremove && sudo apt autoclean
sudo apt install tmux rxvt-unicode build-essential emacs emacs-goodies-el


# Backup existing dotfiles #
############################

mkdir -p ~/.dotbackup

for file in dots/{.[^.],}*; do
    shortfile=${file#dots\/}
    mv ~/$shortfile ~/.dotbackup/ -v
    #echo $shortfile
done

printf '\n\nExisting dotfiles backed up to ~/.dotbackup folder.\n\n'

for file in dots/{.[^.],}*; do
    shortfile=${file#dots\/}
    ln -sv "`pwd`/$file" ~/$shortfile
done

printf '\n\nSymlinks created for dotfiles from git repo.\n\n'


# Manually copy files to the correct location #
###############################################


