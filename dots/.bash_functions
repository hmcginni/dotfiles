#!/bin/bash
#
# Custom bash functions
#


# ──────────────────────────────────────────────────────────
# Activate Python virtual environment

_activate_venv () {

	local venv
	
	venv=$1
	source "$venv"/bin/activate

}


# ──────────────────────────────────────────────────────────
# Simplify `bc` usage

_bc () {

	bc -l <<< "$*"

}


# ──────────────────────────────────────────────────────────
# Add line to clipboard

_copy () {

    tr -d '\n' <<< "$1" | xclip -selection clipboard

}


# ──────────────────────────────────────────────────────────
# Create a Python virtual environment

_create_venv () {
	
	python3 -m venv "$1" --system-site-packages
	
}


# ──────────────────────────────────────────────────────────
# Launch Emacs with ediff-files

_ediff () {

    file1=$1
    file2=$2

	if [ -f "$file1" ] && [ -f "$file2" ]
	then
		emacs --eval "(ediff-files \"$file1\" \"$file2\")"
	fi

}


# ──────────────────────────────────────────────────────────
# Open file with emacsclient

_emacsclient () {

	if ! file=$(which "$1")
	then
		file=$1
	fi
	
    emacsclient -c -a "" "$file" >/dev/null 2>&1 &

}


# ──────────────────────────────────────────────────────────
# Simplify git pushes

_git_push_wrapper () {

    if [[ $PWD =~ hrmutils || $PWD =~ hrmcginnis ]]
    then
	    if [[ $1 == "now" ]]
	    then
	        commitMsg="[m] $(date '+%Y%m%d %I:%M%p') quick update"
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


# ──────────────────────────────────────────────────────────
# MDT - Update EinTestFramework submodule

_git_update_eintestframework () {

	local eintests=""
	local framework=""
	local branch=""
	local id=""
	local commit_msg=""
	local current=""
	local updated=""

	eintests="$SW_TEST_DIR"
	framework="$eintests"/eintestframework
	branch="$(git -C "$eintests" rev-parse --abbrev-ref HEAD)"
	id="$(cut -d- -f1-2 <<< $branch)"
	commit_msg=$(printf "%s update eintestframework" "$id")

	current=$(git -C "$framework" rev-parse HEAD)
	updated=$(git -C "$framework" rev-parse origin/master)

	if [[ $current != $updated ]]
	then
		git -C "$eintests" submodule update --remote --recursive
		git -C "$eintests" add "$framework"
		git -C "$eintests" commit -m "$commit_msg"
		notify-send "EinTests" "EinTestFramework submodule has been updated to latest version"
	else
		notify-send "EinTests" "EinTestFramework is already the latest version"
	fi
	
}


# ──────────────────────────────────────────────────────────
# MDT - Go to the specified EinTests folder

_goto_test_folder () {

	local id
	local test_root
	local test_folder
	
	id=$1
	test_root="$SW_TEST_DIR"

	if [[ -n $id ]]
	then
		test_folder=$(dirname $(fd Automate.*"$id" "$SW_TEST_DIR"))
	else
		test_folder="$test_root"
	fi

	xclip -selection clipboard <<< "pushd $test_folder"
	pushd "$test_folder"
	
}


# ──────────────────────────────────────────────────────────
# Launch MATLAB with custom options

_matlab_wrapper () {
	
	mode=$1
	release=$2
	
	if [[ -z $release ]]
	then
		release="19b"
	fi

	ml_install=$(printf "/opt/matlab/20%s/bin/matlab" "$release")
	export MATLAB_JAVA="/usr/lib/jvm/java-8-openjdk-amd64/jre"

	if [[ $mode == "gui" || -z $mode ]]
    then
	    notify-send "Starting MATLAB R20$release" "matlab -nosplash -r run hrm_startup.m"
	    "$ml_install" -nosplash -r "run hrm_startup.m" >/dev/null 2>&1 & disown

    elif [[ $mode == "cmd" ]]
    then
		mljob=$(tr -cd "[:alnum:]" <<< "\
				   $(awk 'tolower($0) ~ /matlab/ {print $1}' <<< "$(jobs)")")

		if [[ -n $mljob ]]
		then
			notify-send "MATLAB" "Moving to foreground process"
			fg "$mljob"
		else
			notify-send "Starting MATLAB R20$release" "matlab -nosplash -nodesktop -r run hrm_startup.m $*"
			"$ml_install" -nosplash -nodesktop -r "run hrm_startup.m"
		fi
		
    fi

	unset MATLAB_JAVA

}


# ──────────────────────────────────────────────────────────
# Create a new timestamped directory

_new_timestamped_directory () {

	local today
	local name
	local args
	
	args=$*
	today="$(date +%Y%m%d)"
	name="$today-${args// /-}"
	name=${name%%-}
	mkdir -p "$name"
	echo "Created \"$name\""

}


# ──────────────────────────────────────────────────────────
# Add current Git branch to bash prompt

_git_branch_prompt () {
	
	terminal_width=$(tput cols)
	ellipsis=" [...]"
	max_line_length=$(( terminal_width - ${#ellipsis} ))
	branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)

    if [[ -n $branch ]]
    then
		if [[ $branch == "HEAD" ]]
		then
			git_sha=$(git rev-parse --short HEAD)
			branch+=" ($git_sha)"
		fi
		
		if [[ -n "$(git stash list)" ]]
		then
			stash="*"
		fi
		
		repo="${stash}$(basename "$(git rev-parse --show-toplevel)")${stash}"
		git_line=$(printf " %s | %s" "$repo" "$branch")
		
		if [[ ${#git_line} -gt $(( terminal_width )) ]]
		then
			disp_line="${git_line:0:$max_line_length}$ellipsis"
		else
			disp_line="$git_line"
		fi
	fi

	printf "\n%s" "$disp_line"

}

_docker_prompt () {

	if [ -n "$DOCKER" ]
	then
		prompt="image: $DOCKER"
	fi

	printf "\n%s" "$DOCKER"
	
}


# ──────────────────────────────────────────────────────────
# Run command quietly in the background

_quiet () {

	if which chronic >/dev/null 2>&1
	then
		chronic "$@" & disown
	else
		printf "\"chronic\" utility not installed (moreutils)." >&2
	fi
	
}


# ──────────────────────────────────────────────────────────
# MDT - Run the specified Simulink test

_run_simulink_test () {

	local test_id=""
	local build_id=""
	local sltools=""
	local log=""

	test_id_number=$1
	build_id=$2

	if [[ $test_id_number =~ ^[0-9]{4,5}$ ]]
	then
		
		test_id="ESWT-$test_id_number"
		test -z "$build_id" && build_id=${test_id//-/_}
		sltools="$SW_TEST_DIR"/eintestframework/Tools/Bamboo/Simulink
		log="${build_id}-batch-execution-stdout-log.txt"

		python3.7 "$sltools"/run_simulink_tests.py -vv -f <(echo -e "$test_id") -b "$build_id" | tee "$log"

	else
		
		printf "\nInvalid ID.\n" >&2
		return 1
		
	fi

}


# ──────────────────────────────────────────────────────────
# Spell checker

_spell_checker () {

	aspell -a <<< "$@"

}


# ──────────────────────────────────────────────────────────
# Simplify tmux actions

_tmux_go () {

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


# ──────────────────────────────────────────────────────────
# Run command on all TMUX panes

_tmux_run () {
	
	if [[ -n "$TMUX" ]]
	then
		tmux list-panes -a -F '#{session_name}:#{window_index}.#{pane_index}' \
			| xargs -I PANE tmux send-keys -t PANE "$*" Enter clear Enter
	else
		eval "$*"
    fi

}


# ──────────────────────────────────────────────────────────
# MDT - Wrapper function to control the MDTVPN systemd service

_vpn () {

    option="$1"
    if [[ -z $option ]]
    then
	    option="status"
    fi
    
    sudo systemctl "$option" mdtvpn.service

}


# ──────────────────────────────────────────────────────────
# Print current weather conditions

_weather () {

	fmt="\n%C+|+%t+(feels+like+%f)+|+Humidity:+%h+|+Wind:+%w\n"
	curl wttr.in/Natick?format=$fmt

}
