export ZSH_DISABLE_COMPFIX=1   # ignore nonsense permission issue

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="spaceship"

COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  aws
  colored-man-pages
  colorize
  docker
  docker-compose
  git
  python
  themes
  vi-mode
  virtualenv
)

source $ZSH/oh-my-zsh.sh

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR="vim"
else
  export EDITOR="nvim"
fi

unsetopt beep

# reduce delay entering normal mode
KEYTIMEOUT=1  # 10ms

alias vim="nvim"
alias ep="sudo eopkg"

if [ "$(uname)" = "Darwin" ]; then
    alias cbcopy="pbcopy"
    alias cbpaste="pbpaste"
else
    alias open="xdg-open"
    alias cbcopy="xclip -in -selection clipboard"
    alias cbpaste="xclip -out -selection clipboard"
fi

alias dconf-dump="dconf dump / | vim -R -c 'set ft=dosini'"
alias dconf-edit="vim $HOME/.config/dconf.ini"
alias dconf-load="dconf load / < $HOME/.config/dconf.ini"

export PATH="$PATH:$HOME/.local/bin"

if [ "$(uname)" = "Darwin" ]; then
  export PATH="$PATH:/usr/local/sbin"
  export SPACESHIP_DOCKER_SHOW="false"
fi

# https://github.com/TheLocehiliosan/yadm/issues/33
export GPG_TTY=$(tty)
