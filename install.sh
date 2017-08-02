#!/bin/bash

mkdir -p ~/.dotbackup

for file in files/.[^.]*; do
    shortfile=${file/files\/}
    mv ~/$shortfile ~/.dotbackup/ -v
    #echo $shortfile
done

printf '\n\nExisting dotfiles backed up to ~/.dotbackup folder.\n\n'

for file in files/.[^.]*; do
    shortfile=${file/files\/}
    ln -s "`pwd`/$file" ~/$shortfile
    echo "`pwd`/$file"
done

printf '\n\nSymlinks created for dotfiles from git repo.\n\n'
