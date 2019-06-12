#!/bin/bash
#
# .BASH_FUNCTIONS - Custom bash functions
#


_copy(){
    #
    # COPY - add line to clipboard
    #

    tr -d '\n' <<< "$1" | xclip -selection clipboard
}


_ediff() {
    #
    # EDIFF - Launch Emacs with ediff-files
    #

    file1=$1
    file2=$2

    emacs --eval "(ediff-files \"$file1\" \"$file2\")"
}


_emacsclient() {
    #
    # EMACSCLIENT - Open file with emacsclient
    #

    file=$1
    emacsclient -a "emacs" "$file" &>/dev/null &
}


_fzf_branch() {
    #
    # FZF_COMPLETE_GIT_CHECKOUT - fuzzy branch completion in Git
    #

    FZF_COMPLETION_TRIGGER='' _fzf_complete \
			              "--height=10 --reverse" "$@" < <(git branch -a)
}
complete -F _fzf_branch -o default -o bashdefault "gc"


_git_push_wrapper() {
    #
    # GIT_PUSH_WRAPPER - simplify git pushes
    #

    if [[ $PWD =~ hrmutils ]]
    then
	    if [[ $1 == "now" ]]
	    then
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


_ml_wrapper() {
    #
    # ML_WRAPPER - launch MATLAB with preferred Java version
    #

    export MATLAB_JAVA=/usr/lib/jvm/java-8-openjdk-amd64/jre

    if [[ $1 == "gui" ]]
    then
	    shift
	    notify-send "Starting MATLAB..." "nohup matlab -desktop -nosplash $* &>/dev/null &"
	    nohup matlab -desktop -nosplash "$@" &>/dev/null &
    elif [[ $1 == "cmd" ]]
    then
	    shift
	    notify-send "Starting MATLAB..." "matlab -nosplash -nodesktop $*"
	    matlab -nosplash -nodesktop "$*"
    else
	    notify-send "Starting MATLAB..." "matlab $*"
	    matlab "$*"
    fi
}


_parse_git_branch() {
    #
    # PARSE_GIT_BRANCH - add current Git branch to bash prompt
    #

    branch=$(git branch 2>/dev/null | grep "\*" | cut -d"*" -f2)
    if [[ -n $branch ]]
    then
	    printf "\n ⌥ ⎇ : %s" "$branch"
    fi
}


_qfind() {
    #
    # QFIND - find without errors
    #

    find "$@" 2>/dev/null
}


_quiet() {
    #
    # QUIET - run command quietly in the background
    #

    ("$*") &>/dev/null & disown
}


_tmux_go() {
    #
    # TMUX_GO - simplify tmux actions
    #

    if [[ $# == 0 ]]
    then
	    operation="list-sessions"
	    tmux_args="$operation"
    else

	    if [[ -n $TMUX ]]
	    then
	        operation="switch -t" # If we're in TMUX, switch to specified session
	    else
	        operation="new -A -s" # If we're not in TMUX, either attach or create
	    fi	
	    tmux_args="$operation"" ""$1"
    fi
    
    tmux $tmux_args || tmux rename-session "$1"
}


_tmux_run() {
    tmux list-panes -a -F '#{session_name}:#{window_index}.#{pane_index}' | xargs -I PANE tmux send-keys -t PANE "$*" Enter clear Enter
    if [[ -z "$TMUX" ]]
    then
	    source ~/.bashrc
    fi
}


_xdg_open() {
    #
    # XDG_OPEN - quietly open file with the default application
    #

    nohup xdg-open "$1" &>/dev/null &
}


_vpn() {
    #
    # VPN - wrapper function to control the MDTVPN systemd service
    #

    option="$1"
    if [[ -z $option ]]
    then
	    option="status"
    fi
    
    sudo systemctl "$option" mdtvpn.service
}
