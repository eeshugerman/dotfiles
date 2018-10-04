source /usr/share/defaults/etc/profile

export PS1="\W $ "
set -o vi

alias vim="nvim"
alias vi="nvim"

alias xclip="xclip -selection c"

gcsvim () { gzip -d -c $1 | csvtool readable - | vim - ; }

csvim () { csvtool readable $1 | vim - ; }


# export PATH="/anaconda3/bin:$PATH"

# for guake tab names
PROMPT_COMMAND='echo -ne "\033]0;${PWD}\007"'

alias open="xdg-open"
