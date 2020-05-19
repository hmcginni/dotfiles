#!/bin/bash
#
# .BASH_ALIASES
#        Custom stuff for ~/.bashrc including (but not limited to) aliases
#


# ──────────────────────────────────────────────────────────
# Enable delete functionality in st

tput smkx


# ──────────────────────────────────────────────────────────
# Functions

if [[ -f ~/.bash_functions ]]
then
    . ~/.bash_functions
fi


# ──────────────────────────────────────────────────────────
# Variables

blue="\[\e[0;94m\]"
gray="\[\e[1;90m\]"
green="\[\e[0;32m\]"
red="\[\e[0;91m\]"
reset="\[\e[00m\]"

case "$TERM" in
    xterm*|rxvt*|eterm*|screen*|st*)
		# PS1="\n${blue}\u${gray} @ ${green}\h${gray} → ${blue}\w ${red} "
		PS1="\n${green}\u${gray} → ${blue}\w ${red} "
		PS1+="\$(_parse_git_branch)\n${gray} $ ${reset}"
		;;
    *)
		PS1="> "
		;;
esac

export PS1

# ──────────────────────────────────────────────────────────
# Magic for fixing gpg-agent issues (??)

GPG_TTY=$(tty)
export GPG_TTY


# ──────────────────────────────────────────────────────────
# Exports

export hrmpc="BC:5F:F4:5A:77:41"
export sofl1="28:f1:0e:52:44:88"

export PATH="$HOME/bin:$PATH"
export EDITOR="emacsclient -a emacs "
export GIT_EDITOR=$EDITOR
export GTAGSCONF="/usr/local/share/gtags/gtags.conf"
export GTAGSLABEL="pygments"
export XDG_CONFIG_HOME="$HOME/.config"
export RIPGREP_CONFIG_PATH="$XDG_CONFIG_HOME/ripgrep/config"


# ──────────────────────────────────────────────────────────
# Aliases

# general
alias open='xdg-open'
alias update='pass hrm | sudo -S apt autoclean; sudo apt update; sudo apt upgrade -y; sudo apt autoremove; sudo snap refresh; sudo -k'

alias copy='_copy'
alias ediff='_ediff'
alias testdir='_test_dir'
alias new='_new_dir_today'
alias n='_new_dir_today'
alias q='_quiet'
alias qfind='_qfind'
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
alias gitupdate='git pull --recurse-submodules; git submodule sync; git submodule update --init --recursive'
alias gitclean='git checkout -- .; git clean -fd'

# emacs
alias e='_emacsclient'

# MATLAB
alias ml='_matlab_wrapper gui'
alias mlc='_matlab_wrapper cmd'

# Python
alias activate='_activate_venv'
alias venv='_create_venv'
