#!/bin/bash
#
# .BASH_FUNCTIONS - Custom bash functions
#


_copy(){
    # COPY - add line to clipboard

    tr -d '\n' <<< "$1" | xclip -selection clipboard
}


_create_venv() {
	python3 -m venv "$1" --system-site-packages
}


_ediff() {
    # EDIFF - Launch Emacs with ediff-files

    file1=$1
    file2=$2

    emacs --eval "(ediff-files \"$file1\" \"$file2\")"
}


_emacsclient() {
    # EMACSCLIENT - Open file with emacsclient

	if ! file=$(which "$1")
	then
		file=$1
	fi
	
    emacsclient -a "emacs" "$file" &>/dev/null &
}


_git_diff_wrapper() {

	branch=$(git rev-parse --abbrev-ref HEAD)

	git diff --name-status "$branch".."$branch""@{u}"
	
}


_git_push_wrapper() {
    # GIT_PUSH_WRAPPER - simplify git pushes

    if [[ $PWD =~ hrmutils || $PWD =~ hrmcginnis ]]
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


_load_venv() {
	# LOAD_VENV - activate Python virtual environment

	local venv
	
	venv=$1
	source "$venv"/bin/activate
}


_ml_wrapper() {
    # ML_WRAPPER - launch MATLAB with preferred Java version

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


_new_dir_today() {
	# NEW_DIR_TODAY - create a new timestamped directory

	local today
	local name
	local args
	
	args=$*
	today="$(date +%Y%m%d)"
	name="${today}-${args// /-}"
	mkdir -p "$name"
	echo "Created \"$name\""
}


_parse_git_branch() {
    # PARSE_GIT_BRANCH - add current Git branch to bash prompt

	local branch
	local repo
	
	terminal_width=$(tput cols)
	ellipsis=" [...]"
	max_line_length=$(( terminal_width - ${#ellipsis} - 2 ))
	
	branch=$(git branch 2>/dev/null | grep "\*" | cut -d"*" -f2)

    if [[ -n $branch ]]
    then
		stash=""

		if [[ -n "$(git stash list)" ]]
		then
			stash="*"
		fi
		
		repo="$stash"$(basename "$(git rev-parse --show-toplevel)")"$stash"
		git_line=$(printf " %s |%s" "$repo" "$branch")
		
		if [[ ${#git_line} -gt $(( terminal_width - 2 )) ]]
		then
			disp_line="${git_line:0:$max_line_length}$ellipsis"
		else
			disp_line="$git_line"
		fi
	fi

	printf "\n%s" "$disp_line"
}


_qfind() {
    # QFIND - find without errors

    find "$@" 2>/dev/null
}


_quiet() {
    # QUIET - run command quietly in the background

	if which chronic >/dev/null 2>&1
	then
		chronic "$@" & disown
		# ("$*") &>/dev/null & disown
	else
		printf "CHRONIC utility not installed (moreutils)." >&2
	fi
	
}


_test_dir() {
	# TEST_DIR - change to the test folder containing the provided id

	tid=$1
	dir=$(find "$SW_TEST_DIR" -name "$tid" -type d)

	if [[ -n $dir ]]
	then
		pushd "$dir"
	else
		echo "No test folder found containing that ID."
	fi
}


_tmux_go() {
    # TMUX_GO - simplify tmux actions

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
	# TMUX_RUN - run command on all TMUX panes
	
    tmux list-panes -a -F '#{session_name}:#{window_index}.#{pane_index}' \
		| xargs -I PANE tmux send-keys -t PANE "$*" Enter clear Enter
    if [[ -z "$TMUX" ]]
    then
	    source ~/.bashrc
    fi
}


_vpn() {
    # VPN - wrapper function to control the MDTVPN systemd service

    option="$1"
    if [[ -z $option ]]
    then
	    option="status"
    fi
    
    sudo systemctl "$option" mdtvpn.service
}
