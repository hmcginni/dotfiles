#!/bin/bash
#
# .BASH_ALIASES - Custom stuff for ~/.bashrc including (but not limited to) aliases


# Enable delete functionality in st terminal ===================================


tput smkx


# Exports ======================================================================


export DWMDIR="/usr/local/src/dwm-6.1"
export STDIR="${HOME}/repos/st"
export DMENUDIR="/usr/local/src/dmenu-4.7"
export GOPATH="/home/hrm/Documents/go"
export PATH=$PATH:$GOPATH/bin
export PS1="\n\[\033[1;37m\]\[\033[1;34m\]\u\[\033[1;37m\] @ \[\033[0;32m\]\h\[\033[1;37m\] in [\[\033[1;34m\]\w\[\033[1;37m\]]\[\033[1;31m\]\$(parse_git_branch)\n\[\033[1;37m\] $ \[\033[00m\]"


# Aliases ======================================================================


# update software
alias update='sudo apt autoclean && sudo apt update && sudo apt upgrade && sudo apt autoremove -y && sudo snap refresh'

# suckless tools
alias buildwm='cd $DWMDIR && sudo make -B clean install && cd -'
alias buildst='cd $STDIR && sudo make install && cd -'
alias buildmenu='cd $DMENUDIR && sudo make -B clean install && cd -'
alias bs='buildmenu && buildst && buildwm'

# tmux
alias tl='tmux ls'
alias ta='tmux attach -t'
alias tn='tmux new -s'
alias trn='tmux rename-session'
alias tk='tmux kill-session -t'
alias ts='tmux switch -t'
alias tt='tmux switch -l'

# MATLAB
alias ml='nohup custom-matlab-launcher &>/dev/null &'

# others
alias wakepc='wakeonlan BC:5F:F4:5A:77:41'
alias ediff='emacs diff'
alias re='gvfs-trash -f'
alias gitupdate='git pull; git submodule sync; git submodule update --recursive'
alias gitclean='git checkout -- . && git clean -fd'
alias q='_quiet'
alias quiet='_quiet'
alias qfind='_qfind'
alias mvpn='sudo openconnect -umcginh2 --protocol=nc remote.covidien.com/linux'
alias fontstyle='sudo /etc/fonts/infinality/infctl.sh setstyle'
alias copy='_copy'


# Functions ====================================================================


parse_git_branch() {
    branch=$(git branch 2>/dev/null | grep \* | cut -d"*" -f2)
    if [ ! -z $branch ]; then
	printf "\n[⎇ %s]" $branch
    fi
}

_copy(){
    tr -d '\n' <<< "$1" | xclip -selection clipboard
}

_quiet() {
    ("$@") &>/dev/null &
}

_qfind() {
    find "${@}" 2>&1 | grep -v "Permission denied"
}
