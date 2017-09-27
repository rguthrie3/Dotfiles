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
plugins=(git dirhistory mercurial pip)

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
alias gas="as"
alias cdp="cd \${OLDPWD}"
alias make="make -j 8"

# Paths
export PATH=/usr/local/bin:/opt/cuda/bin:$PATH # TODO remove
export LD_LIBRARY_PATH=/opt/cuda/lib64:$LD_LIBRARY_PATH # TODO remove
export LD_LIBRARY_PATH=/home/rguthrie/Packages/boost_1_60_0/stage/lib:$LD_LIBRARY_PATH # for Dynet (doesnt like boost 1.62)
export LD_LIBRARY_PATH=/home/rguthrie/Packages/dynet/build/dynet:$LD_LIBRARY_PATH
export CLASSPATH="/home/rguthrie/Packages/stanford-corenlp-full-2016-10-31/*:${CLASSPATH}"

source $ZSH/oh-my-zsh.sh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOME/Packages/z/z.sh
source /usr/share/zsh/scripts/zplug/init.zsh
