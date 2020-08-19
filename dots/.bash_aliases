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
# Prompt

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


# ──────────────────────────────────────────────────────────
# Variables

export PS1
export GPG_TTY=$(tty)
export hrmpc="BC:5F:F4:5A:77:41"
export hrmlaptop="F8:75:A4:45:4B:3A"
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
alias bc='bc -l'
alias rg='rg -L'
alias open='xdg-open'
alias update='pass hrm | sudo -S apt autoclean; sudo apt update; sudo apt upgrade -y; sudo apt autoremove; sudo snap refresh;'
alias copy='_copy'
alias ediff='_ediff'
alias new='_new_timestamped_directory'
alias q='_quiet'
alias qfind='_qfind'
alias rmtemp='_rmtemp'
alias vpn='_vpn'

# tmux
alias t='_tmux_go'
alias tl='tmux list-sessions'
alias tk='tmux kill-session -t'
alias tt='_tmux_run "source ~/.bashrc"'

# git
alias gp='_git_push_wrapper'
alias gg='_git_push_wrapper now'
alias gitupdate='git pull --recurse-submodules; git submodule sync; git submodule update --init --recursive'
alias gitclean='git checkout -- .; git clean -fd'
alias gitfwupdate='_git_update_eintestframework'

# emacs
alias e='_emacsclient'

# MATLAB
alias ml='_matlab_wrapper gui'
alias mlc='_matlab_wrapper cmd'

# Python
alias activate='_activate_venv'
alias venv='_create_venv'

# MDT
alias run='_run_simulink_test'
