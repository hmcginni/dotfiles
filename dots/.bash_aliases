#!/bin/bash
#
# .BASH_ALIASES
#        Custom stuff for ~/.bashrc including (but not limited to) aliases
#

# Enable delete functionality in st --------------------------------------------


tput smkx


# Functions --------------------------------------------------------------------


if [[ -f ~/.bash_functions ]]; then
    . ~/.bash_functions
fi


# Variables --------------------------------------------------------------------


blue="\[\e[1;34m\]"
gray="\[\e[1;90m\]"
green="\[\e[0;32m\]"
red="\[\e[1;31m\]"
reset="\[\e[00m\]"

case "$TERM" in
    xterm*|rxvt*|eterm*|screen*|st*)
	PS1="\n${blue}\u${gray} @ ${green}\h${gray} →source ~/.bashrc
clear
 ${blue}\w ${red} \$(_parse_git_branch) \n${gray} $ ${reset}"
	;;
    *)
	PS1="> "
	;;
esac

export hrmpc="BC:5F:F4:5A:77:41"
export sofl1="28:f1:0e:52:44:88"
GPG_TTY=$(tty)
export GPG_TTY
export PS1
export PATH="$HOME/bin:$PATH"


# Aliases ----------------------------------------------------------------------

# tmux
alias t='_tmux_go'
alias tl='tmux list-sessions'
alias tk='tmux kill-session -t'
alias tt='_tmux_run "source ~/.bashrc"'

# git
alias gp='_git_push_wrapper'
alias gg='_git_push_wrapper now'
alias gitupdate='git pull; git submodule sync; git submodule update --recursive'
alias gitclean='git checkout -- . && git clean -fd'

# emacs
alias e='emacs -nw'

# MATLAB
alias ml='_ml_wrapper gui'
alias mlc='_ml_wrapper cmd'

# others
alias copy='_copy'
alias ediff='emacs diff'
alias open='_xdg_open'
alias q='_quiet'
alias qfind='_qfind'
alias update='sudo apt autoclean; sudo apt update; sudo apt upgrade -y; sudo apt autoremove'
alias vpn='_vpn'
# alias vpn='pass vpn | sudo openconnect -umcginh2 --passwd-on-stdin --protocol=nc remote.covidien.com/linux'

