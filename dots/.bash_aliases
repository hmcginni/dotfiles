#!/bin/bash
#
# .BASH_ALIASES
#        Custom stuff for ~/.bashrc including (but not limited to) aliases
#


# ──────────────────────────────────────────────────────────
# Enable delete functionality in st

tput smkx


# ──────────────────────────────────────────────────────────
# Functions

if [[ -f ~/.bash_functions ]]
then
    . ~/.bash_functions
fi


# ──────────────────────────────────────────────────────────
# Prompt

blue="\[\e[0;94m\]"
gray="\[\e[1;90m\]"
green="\[\e[0;32m\]"
red="\[\e[0;91m\]"
reset="\[\e[00m\]"

if [ -n "$DOCKER" ]
then
	uname_color="${blue/0/1}"
else
	uname_color="$green"
fi

case "$TERM" in
    xterm*|rxvt*|eterm*|screen*|st*)
		# PS1="\n${blue}\u${gray} @ ${green}\h${gray} → ${blue}\w ${red} "
		PS1="\n${uname_color}\u${gray} → ${blue}\w ${red} "
		PS1+="\$(_git_branch_prompt)\n${gray} $ ${reset}"
		;;
    *)
		PS1="> "
		;;
esac

export PS1


# ──────────────────────────────────────────────────────────
# gpg-agent

export GPG_TTY=$(tty)


# ──────────────────────────────────────────────────────────
# Variables

export hrmpc="BC:5F:F4:5A:77:41"
export hrmlaptop="F8:75:A4:45:4B:3A"
export sofl1="28:f1:0e:52:44:88"
export PATH="$HOME/bin:$PATH"
export VISUAL='emacsclient -a "emacs"'
export EDITOR="$VISUAL"
export GIT_EDITOR=$EDITOR
export GTAGSCONF="/usr/local/share/gtags/gtags.conf"
export GTAGSLABEL="pygments"
export XDG_CONFIG_HOME="$HOME/.config"
export LESS="-RiFM --mouse --wheel-lines=3"


# ──────────────────────────────────────────────────────────
# Aliases

# general
alias c='_bc'
alias gpg='gpg2'
alias rg='rg -L'
alias files='_quiet nautilus -w .'
alias open='xdg-open'
alias update='pass mcginh2 | sudo -S apt autoclean; sudo apt update; sudo apt -y upgrade; sudo apt -y autoremove; sudo snap refresh;'
alias copy='_copy'
alias ediff='_ediff'
alias new='_new_timestamped_directory'
alias q='_quiet'
alias bash_refresh='source "$HOME"/.bashrc'
alias sp='_spell_checker'
alias vpn='_vpn'
alias weather='_weather'

# tmux
alias t='_tmux_go'
alias tl='tmux list-sessions'
alias tk='tmux kill-session -t'
alias tt='_tmux_run "source ~/.bashrc"'

# git
alias gp='_git_push_wrapper'
alias gg='_git_push_wrapper now'
alias gitupdate='git pull --recurse-submodules; git submodule sync; git submodule update --init --recursive'
alias gitclean='git checkout -- .; git clean -fd'
alias gitfwupdate='_git_update_eintestframework'
alias gco="git for-each-ref --format='%(refname:short)' refs/heads | fzf --exact | xargs git checkout"

# emacs
alias e='_emacsclient'
alias edit='_emacsclient'
alias tasks='_export_org_todos'

# MATLAB
alias ml='_matlab_wrapper gui'
alias mlc='_matlab_wrapper cmd'

# Python
alias activate='_activate_venv'
alias venv='_create_venv'

# Docker
alias iosevka_build="_iosevka_build"

_iosevka_build () {

	local hrm_mono_dir
	local iosevka_dir

	hrm_mono_dir="$HOME/.local/share/fonts/mono/Hrm_Mono"
	iosevka_dir="$HOME/repos/Iosevka"

	pushd "$iosevka_dir"
	docker run -it --rm --cpus=2 --memory=2g --memory-swap=2g -v $(pwd):/build avivace/iosevka-build
	popd

	rm -rfv "$hrm_mono_dir"
	cp "$iosevka_dir/dist/hrm-mono/ttf" "$hrm_mono_dir"

}
