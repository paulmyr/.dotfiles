# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

alias zshconfig="nvim ~/.zshrc"
alias zshreload="source ~/.zshrc"

alias vi="nvim"
# alias vim="nvim"

# only for openSuse systems
#alias zypup="sudo zypper up"
#alias zypupd="sudo zypper up -D"
#alias zypdup="sudo zypper dup"
#alias zypdupd="sudo zypper dup -D"
#alias zypin="sudo zypper in"
#alias zypse="zypper se"

# TODO: replace with git aliases
#

plugins=(
    git
    colored-man-pages
)

alias gst="git status"
alias gnuke="git restore ."
alias gaddp="git add -p"
alias gcm="git commit"
alias gcmm="git commit -m"

alias python="python3"

alias gnuke="git restore ."

alias UNI="cd ~/Documents/university/"

alias ytd="youtube-dl -x"

alias denv="conda deactivate"

function aenv() {
    if [ -n "$1" ]
    then
        conda activate "$1"
    else
        echo "please specify environment"
    fi
}

# Uncomment when discord is installed
# 2>&1 redirects stderr to stdout
# alias discord="~/bin/Discord/Discord > /tmp/discord.log 2>&1 &"

function gcm() {
    if [ -n "$1" ]
    then
        git commit -m "$1"
    else
        echo "Please use a commit message"
    fi
}

function commitTex() {
    git add *.tex
    if [ -n "$1" ]
    then
        git commit -m "$1"
    else
        git commit
    fi
    git push
}

function lr() {
    clear
    l
}

function rswu() {
    for f in "$@"
    do 
        mv "$f" "${f// /_}"
    done
}

alias stem="tmux new-session -d -s emacs 'aenv autotune && emacs &'"
alias tat="tmux attach -t"
alias vim="nvim"

##############################################################################
# MACOS
#
# export PATH="$PATH:/Users/paulmayer/Library/Python/3.8/bin"
# export PATH="/opt/homebrew/opt/python@3.10/bin:$PATH"
#
# alias emacs='$(/Applications/Emacs.app/Contents/MacOS/Emacs "$@")'
#
##############################################################################

bindkey -v

# display neofetch the fast way
# cat $HOME/.nf 2> /dev/null
setsid neofetch >| $HOME/.nf

export PATH="$PATH:$HOME/bin"
setxkbmap -option ctrl:nocaps

fnlock(){
    echo 0 | sudo tee /sys/module/hid_apple/parameters/fnmode
}

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/home/palu/.mujoco/mujoco210/bin
export PATH=$PATH:/home/palu/.spicetify:/home/palu/.local/bin
export EDITOR=/usr/bin/nvim
