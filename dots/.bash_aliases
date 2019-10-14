#!/bin/bash
#
# .BASH_ALIASES
#        Custom stuff for ~/.bashrc including (but not limited to) aliases
#

# Enable delete functionality in st

tput smkx

# Functions

if [[ -f ~/.bash_functions ]]
then
    . ~/.bash_functions
fi

# Variables

blue="\[\e[0;94m\]"
gray="\[\e[1;90m\]"
green="\[\e[0;32m\]"
red="\[\e[0;91m\]"
reset="\[\e[00m\]"

case "$TERM" in
    xterm*|rxvt*|eterm*|screen*|st*)
		# PS1="\n${blue}\u${gray} @ ${green}\h${gray} → ${blue}\w ${red} "
		PS1="\n${green}\u${gray} in ${blue}\w ${red} "
		PS1+="\$(_parse_git_branch)\n${gray} $ ${reset}"
		;;
    *)
		PS1="> "
		;;
esac

export PS1
export GPG_TTY
GPG_TTY=$(tty)

export hrmpc="BC:5F:F4:5A:77:41"
export sofl1="28:f1:0e:52:44:88"
export bamboo="ec:b1:d7:94:55:9a"

export PATH="$HOME/bin:$PATH"
export GIT_EDITOR="emacsclient -a \"emacs\" "
export GTAGSCONF="/usr/local/share/gtags/gtags.conf"
export GTAGSLABEL="pygments"

#--------------------
# Aliases

# general
alias open='xdg-open'

# function shortcuts
alias copy='_copy'
alias ediff='_ediff'
alias testdir='_test_dir'
alias newdir='_new_dir_today'
alias n='newdir'
alias q='_quiet'
alias qfind='_qfind'
alias update='pass hrm | sudo -S apt autoclean; sudo apt update; sudo apt upgrade -y; sudo apt autoremove; sudo -k'
alias vpn='_vpn'

# tmux
alias t='_tmux_go'
alias tl='tmux list-sessions'
alias tk='tmux kill-session -t'
alias tt='_tmux_run "source ~/.bashrc"'

# git
alias gc='git checkout'
alias gp='_git_push_wrapper'
alias gg='_git_push_wrapper now'
alias gitupdate='git pull --recurse-submodules=yes; git submodule init; git submodule sync; git submodule update --recursive'
alias gitclean='git checkout -- . && git clean -fd'

# emacs
alias e='_emacsclient'

# MATLAB
alias ml='_ml_wrapper gui'
alias mlc='_ml_wrapper cmd'

# Python
alias py='ipython3 qtconsole &'

# SSH
alias slb='ssh -Y ubuntu@sl-bamboo'
