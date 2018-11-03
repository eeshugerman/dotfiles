source /usr/share/defaults/etc/profile

export PS1="\W $ "
set -o vi

alias vim="nvim"
alias vi="nvim"

alias xclip="xclip -selection c"

csvim () { gzip -d -c $1 | csvtool readable - | vim - ; }


# export PATH="/anaconda3/bin:$PATH"

# for guake tab names
PROMPT_COMMAND='echo -ne "\033]0;${PWD}\007"'

alias open="xdg-open"

export PATH="$PATH:~/opt/REAPER"

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/programming
source /usr/bin/virtualenvwrapper.sh
