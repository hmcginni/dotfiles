#!/bin/bash


if [ ! -f $1 ]; then
    echo "$1 does not exist or is not a regular file."
    exit 1
fi

file=$1
fullfile=$(readlink -f $file)
reluserpath=${fullfile#/home/$USER}
gitdotspath=$GITDOTS/dots$reluserpath

mkdir -p $gitdotspath
mv -v $fullfile $gitdotspath
ln -sv $gitdotspath $fullfile
