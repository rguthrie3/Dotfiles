# Path to oh-my-zsh installation.
ZSH=/usr/share/oh-my-zsh/

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="gallois"

# Case sensitive completion
CASE_SENSITIVE="true"

# Disable auto update
DISABLE_AUTO_UPDATE="true"

# Time stamp for commands
HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

ZSH_CACHE_DIR=$HOME/.oh-my-zsh-cache
if [[ ! -d $ZSH_CACHE_DIR ]]; then
  mkdir $ZSH_CACHE_DIR
fi


# ===== End oh-my-zsh conf =====

# Env variables
export LANG=en_US.UTF-8
export EDITOR='vim'
export TERM="xterm-256color"

# Aliases
alias ll="ls -lah"
alias qemu="qemu-system-x86_64"

# Paths
#,export PATH="${PATH}:/home/rguthrie/Programming/Repos/llvm/build/bin"
export PATH="/home/rguthrie/.local/bin:${PATH}"

source $ZSH/oh-my-zsh.sh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Emacs
alias em="emacsclient -t"
function emg() {
    emacsclient -c $@ &
}

#autoload -U select-word-style
#select-word-style normal 
export WORDCHARS="-_"

function guitar_pro() {
    wine ~/.wine/drive_c/Program\ Files\ \(x86\)/Arobas\ Music/Guitar\ Pro\ 7/GuitarPro7.exe
}

function pacman_no_dependents() {
    awk 'NR == FNR { deps[$0] } NR != FNR { if (!($1 in deps)) { print $1 } }' \
        <(expac -s '%n %D' | awk '{for (i = 2; i <= NF; ++i) { print $(i) } }' | sort | uniq) \
        <(expac -s '%n %D')
}
