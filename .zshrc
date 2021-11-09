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

# https://github.com/TheLocehiliosan/yadm/issues/33
export GPG_TTY=$(tty)


if [[ "$INSIDE_EMACS" ]]; then
    # use evil instead of zsh's vi emulation
    bindkey -e
fi

export HOMEBREW_NO_AUTO_UPDATE=1

eval "$(direnv hook zsh)"

export JAVA_HOME=/usr/local/Cellar/openjdk@11/11.0.12/libexec/openjdk.jdk/Contents/Home

alias snowsql=/Applications/SnowSQL.app/Contents/MacOS/snowsql

export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# i think this is just for launching emacs from cli -- not worth poluting the PATH
# for emacs_app_path in "/Applications/Emacs.app" "$HOME/opt/Emacs.app"; do
#     emacs_bin_path="${emacs_app_path}/Contents/MacOS/bin"
#     if [ -d emacs_bin_path ]; then
#         export PATH="${emacs_bin_path}:$PATH"
#     fi
# done
