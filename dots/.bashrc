# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Start TMUX
# if command -v tmux>/dev/null; then
#     [ -z $TMUX ] && { tmux attach >> /dev/null 2>&1 || tmux new-session >> /dev/null; }
# fi

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'


# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi


# Custom Environment Variables

# arrow character: \342\206\222

export DISPLAY=:0.0
#export PS1="\[\033[1;37m\]┌─[\[\033[1;34m\]\u\[\033[1;37m\]@\[\033[0;32m\]\h\[\033[1;37m\]]─────[\[\033[1;34m\]\w\[\033[1;31m\]\$(parse_git_branch)\[\033[1;37m\]]\n\[\033[1;37m\]└───[ \[\033[00m\]"
export PS1="\n\[\033[1;37m\]\[\033[1;34m\]\u\[\033[1;37m\] @ \[\033[0;32m\]\h\[\033[1;37m\] in [\[\033[1;34m\]\w\[\033[1;31m\]\$(parse_git_branch)\[\033[1;37m\]]\n\[\033[1;37m\] $ \[\033[00m\]"
export DWMDIR="/usr/local/src/dwm-6.1"
export STDIR="/usr/local/src/st-0.7"
export DMENUDIR="/usr/local/src/dmenu-4.7"
export GITDOTS="/home/$USER/.cfg/dotfiles"
export FT2_SUBPIXEL_HINTING=0  # Classic mode
export LIC="/opt/matlab/2017a/etc/license.dat"
export LMUTIL="/opt/matlab/2017a/etc/glnxa64/lmutil"
# export MATLAB_JAVA="/usr/lib/jvm/java-7-openjdk-amd64/jre"

# New Aliases

alias update='yes | sudo apt-get autoclean && yes | sudo apt update && yes | sudo apt upgrade && yes | sudo apt-get autoremove && sudo updatedb'
alias editdwm='sudo emacs -nw ${DWMDIR}/config.def.h'
alias editst='sudo emacs -nw ${STDIR}/config.def.h'
alias editdmenu='sudo emacs -nw ${DMENUDIR}/config.def.h'

alias cdwm='cd $DWMDIR'
alias cdst='cd $STDIR'
alias cdmenu='cd $DMENUDIR'

alias gitdots='cd $GITDOTS'
alias buildwm='a=$(pwd) && cd $DWMDIR && sudo make -B clean install && cd $a'
alias buildst='a=$(pwd) && cd $STDIR && sudo make -B clean install && cd $a'
alias buildmenu='a=$(pwd) && cd $DMENUDIR && sudo make -B clean install && cd $a'
alias bs='buildmenu && buildst && buildwm'
alias cdgit='cd ~/.cfg/dotfiles'
alias notes='cd ~/Dropbox/Notes'
alias addtogit='~/.scripts/addtogitdots.sh'
alias tl='tmux ls'
alias ta='tmux attach -t'
alias tn='tmux new -s'
alias tr='tmux rename-session'
alias tk='tmux kill-session -t'
alias ml='MATLAB_JAVA=/usr/lib/jvm/java-1.8.0-openjdk-amd64/jre matlab -useStartupFolderPref'


# M*IC Stuff

export BPQNXIP=192.168.0.208
export DDQNXIP=192.168.0.94
alias sshbp='ssh root@${BPQNXIP}'
alias sshdd='ssh root@${DDQNXIP}'
alias qnxenv='source /opt/qnx/6.6.0/qnx660-env.sh'
alias pspdir='cd /opt/matlab/2017a/toolbox/psp/QNX_Target_PSP/'
alias licenses='/opt/matlab/2017a/etc/glnxa64/lmutil lmstat -a -c /opt/matlab/2017a/licenses/license.lic'
alias lic='/opt/matlab/2017a/etc/glnxa64/lmutil lmstat -f ${1} -c /opt/matlab/2017a/licenses/license.lic'
alias lictest='/opt/matlab/2017a/etc/glnxa64/lmutil lmstat -f Simulink_Test -c /opt/matlab/2017a/licenses/license.lic'
alias licsf='/opt/matlab/2017a/etc/glnxa64/lmutil lmstat -f Stateflow -c /opt/matlab/2017a/licenses/license.lic'
alias licdv='/opt/matlab/2017a/etc/glnxa64/lmutil lmstat -f Simulink_Design_Verifier -c /opt/matlab/2017a/licenses/license.lic'
alias licvnv='/opt/matlab/2017a/etc/glnxa64/lmutil lmstat -f SL_Verification_Validation -c /opt/matlab/2017a/licenses/license.lic'
alias licsl='licdv && licvnv && lictest && licsf'
alias licrm='_licrm'
alias mylic="licenses | grep 'hrm\|Users' | sed '$!N;/\n.*hrm/! d;P;D'"
alias qnxtargetdir='cd /opt/matlab/2017a/toolbox/psp/QNX_Target_PSP/'
alias gitit='git pull && git submodule init && git submodule sync && git submodule update --recursive'
alias copy='function _copy(){ "$1" | \tr -d '"'\n'"' | xclip -selection clipboard; };_copy'
alias licfree='_licfree'
alias einfs='sshfs publicfiles@ein-fs.thcg.net:pub/software/mcginh2 /mnt/ein-fs -C -o nonempty'
alias gitupdate='git pull && git submodule sync && git submodule update --recursive'
alias quiet='_quiet'

# Functions

parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

_licfree() {
    lic $1 | grep "Users of $1" | sed -e '/Total of /!d;s//&\n/;s/.*\n//;:a;/license/bb;$!{n;ba};:b;s//\n&/;P;D' 
}

_licrm() {
    /opt/matlab/2017a/etc/glnxa64/lmutil lmremove -c /opt/matlab/2017a/licenses/license.lic $1 hrm hmcginnis-dell5520 $2
}

_quiet() {
    ("$@") &>/dev/null
}

# === Begin Einstein Provisioned Setting. === #
# export PATH=/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
# export NDDSHOME=/opt/RTI/ndds.5.2.3
# export PATH=/opt/qt/5.8.0/bin:$PATH
# export PATH=/opt/gcc-arm-none-eabi-5_4-2016q3/arm-none-eabi/bin:$PATH
# export PATH=/opt/synopsys/8.7.1/bin:$PATH
# export PATH=/opt/microchip/xc16/1.26/bin:$PATH
# export JAVA_HOME=/opt/jdk/jdk1.8.0_102
# export PATH=${JAVA_HOME}/bin:$PATH
# export PATH=/opt/Xilinx/Vivado/2016.1/bin:$PATH
# export PATH=/opt/Xilinx/SDK/2016.1/bin:$PATH

# === End Einstein Provisioned Setting. Place personal settings below this line. === #
