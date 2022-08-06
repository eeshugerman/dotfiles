if [ "$(yadm config --get local.class)" = "WORK" ]; then day_job=true; else day_job=false; fi

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

export EDITOR="vim"

unsetopt beep

# reduce delay entering normal mode
KEYTIMEOUT=1  # 10ms

if [ "$(uname)" = "Darwin" ]; then
    alias cbcopy="pbcopy"
    alias cbpaste="pbpaste"
else
    alias open="xdg-open"
    alias cbcopy="xclip -in -selection clipboard"
    alias cbpaste="xclip -out -selection clipboard"
fi

alias dconf-dump="dconf dump / | vim -R -c 'set ft=dosini'"
alias dconf-edit="vim $HOME/.config/dconf-user.conf"
alias dconf-load="dconf load / < $HOME/.config/dconf-user.conf"

# https://github.com/TheLocehiliosan/yadm/issues/33
export GPG_TTY=$(tty)


if [[ "$INSIDE_EMACS" ]]; then
    # use evil instead of zsh's vi emulation
    bindkey -e
fi


eval "$(direnv hook zsh)"

if [ "$(uname)" = "Darwin" ]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "/usr/local/opt/nvm/nvm.sh" ] && \. "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
    [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
else
    source /usr/share/nvm/init-nvm.sh
fi

# macos stuff
export HOMEBREW_NO_AUTO_UPDATE=1
alias fix-org-data-sync="launchctl unload -w Library/LaunchAgents/me.org-data-git-sync.plist && launchctl load -w Library/LaunchAgents/me.org-data-git-sync.plist"

if [ $day_job = true ]; then
    export JAVA_HOME=/usr/local/Cellar/openjdk@11/11.0.12/libexec/openjdk.jdk/Contents/Home
    alias snowsql=/Applications/SnowSQL.app/Contents/MacOS/snowsql
    # https://github.com/immuta/bodata/blob/master/README.md#getting-started
    sudo ifconfig lo0 alias 10.0.2.2
fi
