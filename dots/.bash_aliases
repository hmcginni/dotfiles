#!/bin/bash
#
# .BASH_ALIASES - Custom stuff for ~/.bashrc including (but not limited to) aliases


# Enable delete functionality in st terminal ===================================


tput smkx


# Exports ======================================================================


export DWMDIR="/usr/local/src/dwm-6.1"
export STDIR="${HOME}/repos/st"
export DMENUDIR="/usr/local/src/dmenu-4.7"
export PS1="\n\[\033[1;37m\]\[\033[1;34m\]\u\[\033[1;37m\] @ \[\033[0;32m\]\h\[\033[1;37m\] in [\[\033[1;34m\]\w\[\033[1;37m\]]\[\033[1;31m\]\$(_parse_git_branch)\n\[\033[1;37m\] $ \[\033[00m\]"
export hrmpc="BC:5F:F4:5A:77:41"
GPG_TTY=$(tty)
export GPG_TTY


# Aliases ======================================================================


# general
alias update='sudo apt autoclean && sudo apt update && sudo apt upgrade && sudo apt autoremove -y && sudo snap refresh'

# suckless tools
alias bdwm='cd $DWMDIR && sudo make -B clean install && cd -'
alias bst='cd $STDIR && sudo make install && cd -'
alias bdmenu='cd $DMENUDIR && sudo make -B clean install && cd -'
alias bs='buildmenu && buildst && buildwm'

# tmux
alias t='_tmux_go'
alias tl='tmux list-sessions'
alias tk='tmux kill-session -t'

# emacs
alias emacs='q \emacs -f color-theme-atom-one-light'
alias e='\emacs -nw -f command-line'

# MATLAB
alias ml='_ml'

# others
alias copy='_copy'
alias ediff='emacs diff'
alias err='_err'
alias gitupdate='git pull; git submodule sync; git submodule update --recursive'
alias gitclean='git checkout -- . && git clean -fd'
alias q='_quiet'
alias qfind='_qfind'
alias vpn='pass medtronic/vpn | sudo openconnect -umcginh2 --passwd-on-stdin --protocol=nc remote.covidien.com/linux'
alias socksvpn='pass medtronic/vpn | openconnect -umcginh2 --passwd-on-stdin --protocol=nc --script-tun --script "ocproxy -D 11080" remote.covidien.com/linux'

# Functions ====================================================================


_copy(){
    tr -d '\n' <<< "$1" | xclip -selection clipboard
}

_err() {
    "$@" 2>&1 1>/dev/null
}

_ml() {
    export MATLAB_JAVA=/usr/lib/jvm/java-8-openjdk-amd64/jre
    nohup matlab -desktop &>/dev/null &
}

_parse_git_branch() {
    branch=$(git branch 2>/dev/null | grep \* | cut -d"*" -f2)
    if [[ ! -z $branch ]]; then
	printf "\n ⌥ ⎇ : %s" "$branch"
    fi
}

_quiet() {
    ("$@") &>/dev/null &
}

_qfind() {
    find "${@}" 2>&1 | grep -v "Permission denied"
}

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
