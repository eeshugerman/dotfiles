export ZSH_DISABLE_COMPFIX=true   # ignore nonsense permission issue

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="refined"
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

if [[ "$INSIDE_EMACS" ]]; then
    export PAGER="cat"
else
    export PAGER="less"
fi

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


if [[ "$INSIDE_EMACS" ]]; then
    # use evil instead of zsh's vi emulation
    bindkey -e
fi

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    vterm_printf () {
        printf "\e]%s\e\\" "$1"
    }
    vterm_prompt_end() {
        vterm_printf "51;A$(pwd)";
    }

    # https://github.com/akermu/emacs-libvterm#vterm-clear-scrollback
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'

    # https://github.com/akermu/emacs-libvterm#vterm-buffer-name-string
    # (currently disabled)
    autoload -U add-zsh-hook
    add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%2~\a" }

    # https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
    setopt PROMPT_SUBST
    PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
fi

# ends emacs vterm stuff
