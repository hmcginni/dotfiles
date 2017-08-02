#!/bin/bash

mkdir -p ~/.cfgbackup

for file in files/.[^.]*; do
    shortfile=${file/files\/}
    cp ~/$shortfile ~/.cfgbackup/ -v
    #echo $shortfile
done

printf '\n\nExisting dotfiles backed up to ~/.cfgbackup folder.\n\n'

for file in files/.[^.]*; do
    shortfile=${file/files\/}
    ln -s $file ~/$shortfile
done

printf '\n\nSymlinks created for dotfiles from git repo.\n\n'
