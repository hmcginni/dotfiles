#!/bin/bash
#
# .BASH_FUNCTIONS
#        Custom bash functions
#


#
# COPY - add line to clipboard
#
_copy(){
    tr -d '\n' <<< "$1" | xclip -selection clipboard
}


#
# FZF_COMPLETE_GIT_CHECKOUT - fuzzy branch completion in Git
#
_fzf_branch() {
    
    FZF_COMPLETION_TRIGGER=''
    _fzf_complete "--height=10 --reverse" "$@" < <(git branch -a)
}
complete -F _fzf_branch -o default -o bashdefault "gc"


#
# GIT_PUSH_WRAPPER - simplify git pushes
#
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


#
# ML_WRAPPER - launch MATLAB with preferred Java version
#
_ml_wrapper() {
    export MATLAB_JAVA=/usr/lib/jvm/java-8-openjdk-amd64/jre

    if [[ $1 == "gui" ]]; then
	shift
	notify-send "Starting MATLAB..." "nohup matlab -desktop -nosplash $* &>/dev/null &"
	nohup matlab -desktop -nosplash "$@" &>/dev/null &
    elif [[ $1 == "cmd" ]]; then
	shift
	notify-send "Starting MATLAB..." "matlab -nosplash -nodesktop $*"
	matlab -nosplash -nodesktop "$*"
    else
	notify-send "Starting MATLAB..." "matlab $*"
	matlab "$*"
    fi
}


#
# PARSE_GIT_BRANCH - add current Git branch to bash prompt
#
_parse_git_branch() {
    branch=$(git branch 2>/dev/null | grep "\*" | cut -d"*" -f2)
    if [[ -n $branch ]]; then
	printf "\n ⌥ ⎇ : %s" "$branch"
    fi
}


#
# QFIND - find without errors
#
_qfind() {
    find "$@" 2>/dev/null
}


#
# QUIET - run command quietly in the background
#
_quiet() {
    ("$*") &>/dev/null & disown
}


#
# TMUX_GO - simplify tmux actions
#
_tmux_go() {
    if [[ $# == 0 ]]; then
	operation="list-sessions"
	tmux_args="$operation"
    else

	if [[ -n $TMUX ]]; then
	    operation="switch -t" # If we're in TMUX, switch to specified session
	else
	    operation="new -A -s" # If we're not in TMUX, either attach or create
	fi	
	tmux_args="$operation"" ""$1"
    fi
    
    tmux $tmux_args || tmux rename-session "$1"
}


#
# TMUX_RUN - run command in all tmux panes
#
_tmux_run() {
    tmux list-panes -a -F '#{session_name}:#{window_index}.#{pane_index}' | xargs -I PANE tmux send-keys -t PANE "$*" Enter clear Enter
    if [[ -z "$TMUX" ]]; then
	source ~/.bashrc
    fi
}


#
# XDG_OPEN - quietly open file with the default application
#
_xdg_open() {
    nohup xdg-open "$1" &>/dev/null &
}


#
# VPN - wrapper function to control the MDTVPN systemd service
#
_vpn() {
    option="$1"
    if [[ -z $option ]]; then
	option="status"
    fi
    
    sudo systemctl "$option" mdtvpn.service
}
