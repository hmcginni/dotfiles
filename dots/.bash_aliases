#!/bin/bash
#
# .BASH_ALIASES
#        Custom stuff for ~/.bashrc including (but not limited to) aliases
#

# Enable delete functionality in st terminal -----------------------------------


tput smkx


# Variables --------------------------------------------------------------------


blue="\[\033[1;34m\]"
gray="\[\033[1;37m\]"
green="\[\033[0;32m\]"
red="\[\033[1;31m\]"
black="\[\033[00m\]"

export PS1="\n${blue}\u${gray} @ ${green}\h${gray} ∈ ${blue}\w ${red} \$(_parse_git_branch) \n${gray} $ ${black}"
export hrmpc="BC:5F:F4:5A:77:41"
GPG_TTY=$(tty)
export GPG_TTY


# Aliases ----------------------------------------------------------------------

# tmux
alias t='_tmux_go'
alias tl='tmux list-sessions'
alias tk='tmux kill-session -t'

# git
alias gpush='_git_push'
alias g='git commit -a -m \"[m] $(date "+%Y%m%d %I:%M%p") update\"'
alias gitupdate='git pull; git submodule sync; git submodule update --recursive'
alias gitclean='git checkout -- . && git clean -fd'

# emacs
alias emacs='q \emacs -f gui'
alias e='\emacs -nw -f command-line'

# MATLAB
alias ml='_ml'

# Screen Unlock Daemon (slockd)
alias s='_slockd_handler'

# others
alias update='pass hrm | sudo -S apt autoclean && pass hrm | sudo -S apt update && pass hrm | sudo -S apt upgrade -y && pass hrm | sudo -S apt autoremove && pass hrm | sudo -S snap refresh'
alias copy='_copy'
alias ediff='emacs diff'
alias err='_err'
alias q='_quiet'
alias qfind='_qfind'
alias socksvpn='pass vpn | openconnect -umcginh2 --passwd-on-stdin --protocol=nc --script-tun --script "ocproxy -D 11080" remote.covidien.com/linux'
alias vpn='pass vpn | sudo openconnect -umcginh2 --passwd-on-stdin --protocol=nc remote.covidien.com/linux'

# Functions --------------------------------------------------------------------

_copy(){
    tr -d '\n' <<< "$1" | xclip -selection clipboard
}

# ---------------------------- #

_err() {
    "$@" 2>&1 1>/dev/null
}

# ---------------------------- #

_git_push() {
    commitMsg=$(printf "%s" "$@")
    echo $commitMsg
    git commit -a -m "$commitMsg"
    git push
}

# ---------------------------- #

_ml() {
    export MATLAB_JAVA=/usr/lib/jvm/java-8-openjdk-amd64/jre
    nohup matlab -desktop &>/dev/null &
}

# ---------------------------- #

_parse_git_branch() {
    branch=$(git branch 2>/dev/null | grep \* | cut -d"*" -f2)
    if [[ ! -z $branch ]]; then
	printf "\n ⌥ ⎇ : %s" "$branch"
    fi
}

# ---------------------------- #

_quiet() {
    ("$@") &>/dev/null &
}

# ---------------------------- #

_qfind() {
    find "${@}" 2>&1 | grep -v "Permission denied"
}

# ---------------------------- #

_slockd_handler() {
    systemctl --user "$1" slockd.service
}

complete -F _systemctl s
complete -F _systemctl _slockd_handler

# ---------------------------- #

_tmux_go() {
    if [[ -z "$1" ]]; then
	operation="list-sessions"	
    elif [[ ! -z $(grep "$1:" <<< "$(tmux ls)") ]]; then #session exists
	if [[ ! -z "$TMUX" ]]; then #session exists and currently in tmux
	    operation="switch -t"
	else #session exists and not currently in tmux
	    operation="attach -t"
	fi
    elif [[ ! -z "$TMUX" ]]; then #session does not exist and currently in tmux
	operation="switch -t"
    else #session does not exist and not currently in tmux
	operation="new -s"
    fi
    
    tmux $operation $1 || tmux rename-session $1
}

# ---------------------------- #
