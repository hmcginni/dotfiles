#!/bin/bash
#
# .BASH_ALIASES
#        Custom stuff for ~/.bashrc including (but not limited to) aliases
#

# Enable delete functionality in st --------------------------------------------


tput smkx


# Variables --------------------------------------------------------------------


blue="\[\e[1;34m\]"
gray="\[\e[1;90m\]"
green="\[\e[0;32m\]"
red="\[\e[1;31m\]"
reset="\[\e[00m\]"

case "$TERM" in
    xterm*|rxvt*|eterm*|screen*|st*)
	PS1="\n${blue}\u${gray} @ ${green}\h${gray} ∈ ${blue}\w ${red} \$(_parse_git_branch) \n${gray} $ ${reset}"
	;;
    *)
	PS1="> "
	;;
esac

export hrmpc="BC:5F:F4:5A:77:41"
GPG_TTY=$(tty)
export GPG_TTY
export PS1


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
alias e='emacs -nw -f cmd'

# MATLAB
alias ml='_ml gui -r ''check Simulink Simulink_Test Stateflow'''
alias mlc='_ml cmd'
alias matlab='_ml'

# Systemd
alias s='_systemd_handler'

# others
alias update='sudo apt autoclean && sudo apt update && sudo apt upgrade -y && sudo apt autoremove && sudo snap refresh'
alias copy='_copy'
alias ediff='emacs diff'
alias q='_quiet'
alias qfind='_qfind'
alias socksvpn='pass vpn | openconnect -umcginh2 --passwd-on-stdin --protocol=nc --script-tun --script "ocproxy -D 11080" remote.covidien.com/linux'
alias vpn='pass vpn | sudo openconnect -umcginh2 --passwd-on-stdin --protocol=nc remote.covidien.com/linux'

# Functions --------------------------------------------------------------------

_copy(){
    tr -d '\n' <<< "$1" | xclip -selection clipboard
}

# ---------------------------- #

_git_push_wrapper() {
    if [[ $PWD =~ hrmutils ]]; then
	if [[ $1 == "now" ]]; then
	    commitMsg="[m] $(date '+%Y%m%d %I:%M%p') update"
	else
	    commitMsg="$*"
	fi
	
    echo "$commitMsg"
    git commit -a -m "$commitMsg"
    git push
    
    else
	echo "git push not possible here."
    fi
}

# ---------------------------- #

_ml() {
    export MATLAB_JAVA=/usr/lib/jvm/java-8-openjdk-amd64/jre

    if [[ $1 == "gui" ]]; then
	shift
	notify-send "Starting MATLAB..." \
		    "nohup matlab -desktop -nosplash $* &>/dev/null &"
	nohup \matlab -desktop -nosplash $* &>/dev/null &
    elif [[ $1 == "cmd" ]]; then
	shift
	notify-send "Starting MATLAB..." "matlab -nosplash -nodesktop $*"
	\matlab -nosplash -nodesktop "$*"
    else
	notify-send "Starting MATLAB..." "matlab $*"
	\matlab "$*"
    fi
}

# ---------------------------- #

_parse_git_branch() {
    branch=$(git branch 2>/dev/null | grep "\*" | cut -d"*" -f2)
    if [[ -n $branch ]]; then
	printf "\n ⌥ ⎇ : %s" "$branch"
    fi
}

# ---------------------------- #

_quiet() {
    ("$*") &>/dev/null &
}

# ---------------------------- #

_qfind() {
    find "$*" 2>&1 | grep -v "Permission denied"
}

# ---------------------------- #

_systemd_handler() {
    sudo systemctl "$*"
}

# ---------------------------- #

_tmux_go() {
    if [[ $# == 0 ]]; then
	operation="list-sessions"
	tmux_args="$operation"
    else
	if grep -q "$1:" <<< "$(tmux ls)" ; then #session exists
	    if [[ -n "$TMUX" ]]; then #session exists and currently in tmux
		operation="switch -t"
	    else #session exists and not currently in tmux
		operation="attach -t"
	    fi
	elif [[ -n "$TMUX" ]]; then #session does not exist and currently in tmux
	    operation="switch -t"
	else #session does not exist and not currently in tmux
	    operation="new -s"
	fi
	tmux_args="$operation"" ""$1"
    fi
    
    tmux $tmux_args || tmux rename-session "$1"
}

# ---------------------------- #

_tmux_run() {
    tmux list-panes -a -F '#{session_name}:#{window_index}.#{pane_index}' | xargs -I PANE tmux send-keys -t PANE "$*" Enter clear Enter
    if [[ -z "$TMUX" ]]; then
	source ~/.bashrc
    fi
}
