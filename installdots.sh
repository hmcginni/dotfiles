#!/bin/bash

# Install packages #
####################

sudo apt update && sudo apt upgrade && sudo apt autoremove && sudo apt autoclean
sudo apt install tmux stterm rxvt-unicode build-essential emacs emacs-goodies-el


# Copy dots to ~ #
##################

mkdir -p ~/.dotbackup

for file in files/.[^.]*; do
    shortfile=${file/files\/}
    mv ~/$shortfile ~/.dotbackup/ -v
    #echo $shortfile
done

printf '\n\nExisting dotfiles backed up to ~/.dotbackup folder.\n\n'

for file in files/.[^.]*; do
    shortfile=${file/files\/}
    ln -sv "`pwd`/$file" ~/$shortfile
done

printf '\n\nSymlinks created for dotfiles from git repo.\n\n'

