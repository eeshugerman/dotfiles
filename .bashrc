set -o vi

alias vim="nvim"
alias vi="nvim"
alias xclip="xclip -selection c"
alias open="xdg-open"

export PS1="\W $ "

export PATH="$PATH:$HOME/opt/REAPER"

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/programming
source /usr/bin/virtualenvwrapper.sh
