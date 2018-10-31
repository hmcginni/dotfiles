#!/bin/bash

# Aliases ======================================================================


alias update='sudo apt autoclean && sudo apt update && sudo apt upgrade && sudo apt autoremove -y && sudo updatedb && sudo snap refresh'

# suckless tools
alias editdwm='sudo emacs -nw ${DWMDIR}/config.def.h'
alias editst='sudo emacs -nw ${STDIR}/config.def.h'
alias editdmenu='sudo emacs -nw ${DMENUDIR}/config.def.h'
alias cdwm='cd $DWMDIR'
alias cdst='cd $STDIR'
alias cdmenu='cd $DMENUDIR'
alias buildwm='a=$(pwd) && cd $DWMDIR && sudo make -B clean install && cd $a'
alias buildst='cd $STDIR && sudo make install && cd -'
alias buildmenu='a=$(pwd) && cd $DMENUDIR && sudo make -B clean install && cd $a'
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
alias ml='custom-matlab-launcher &>/dev/null &'

# others
alias wakePC='wakeonlan BC:5F:F4:5A:77:41'
alias ediff='emacs diff'
alias re='gvfs-trash -f'
alias gitupdate='git pull && git submodule sync && git submodule update --recursive'
alias quiet='_quiet'
alias qfind='_qfind'
alias mvpn='sudo openconnect -umcginh2 --protocol=nc remote.covidien.com/linux'
alias fontstyle='sudo /etc/fonts/infinality/infctl.sh setstyle'
alias copy='_copy'


# Enable delete functionality in st terminal ===================================


tput smkx


# Functions ====================================================================


parse_git_branch() {
    git branch 2>/dev/null | grep \* | cut -d"*" -f2
    #git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

_copy(){
    tr -d '\n' <<< "$1" | xclip -selection clipboard
}

_licfree() {
    lic $1 | grep "Users of $1" | sed -e '/Total of /!d;s//&\n/;s/.*\n//;:a;/license/bb;$!{n;ba};:b;s//\n&/;P;D' 
}

_licrm() {
    /opt/matlab/2017a/etc/glnxa64/lmutil lmremove -c /opt/matlab/2017a/licenses/network.lic $1 hrm hmcginnis-dell5520 $2
}

_quiet() {
    ("$@") &>/dev/null &
}

_qfind() {
    find "${@}" 2>&1 | grep -v "Permission denied"
}
