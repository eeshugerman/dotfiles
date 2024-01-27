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
	  alias upgrade-emacs="brew update && brew uninstall emacs-plus@29 && brew install emacs-plus@29 --with-poll --with-native-comp"
else
    alias open="xdg-open"
    alias cbcopy="xclip -in -selection clipboard"
    alias cbpaste="xclip -out -selection clipboard"

    alias sudo="sudo " # https://askubuntu.com/a/22043
    alias nixos-rebuild-test="sudo nixos-rebuild test --impure --flake ~/.config/nixos"
    alias nixos-rebuild-switch="sudo nixos-rebuild switch --impure --flake ~/.config/nixos"
    alias nixos-print-diffs="nix profile diff-closures --profile /nix/var/nix/profiles/system | tail -100"

    alias dconf-dump="dconf dump / | vim -R -c 'set ft=dosini'"
    alias dconf-edit="vim $HOME/.config/dconf-user.conf"
    alias dconf-load="dconf load / < $HOME/.config/dconf-user.conf"
fi

alias my-nix-gc="nix-collect-garbage --delete-older-than 15d"


function rsync-to-kodi {
    rsync --recursive --verbose --progress $1 kodi:/storage/tvshows
}

function rsync-backup {
    rsync --archive --recursive --verbose --progress $1 $2
}

# https://github.com/TheLocehiliosan/yadm/issues/33
export GPG_TTY=$(tty)


if [[ "$INSIDE_EMACS" ]]; then
    # use evil instead of zsh's vi emulation
    bindkey -e
fi


eval "$(direnv hook zsh)"

# misc macos stuff
if [ "$(uname)" = "Darwin" ]; then
    export HOMEBREW_NO_AUTO_UPDATE=1
    alias fix-org-data-sync="launchctl unload -w Library/LaunchAgents/me.org-data-git-sync.plist && launchctl load -w Library/LaunchAgents/me.org-data-git-sync.plist"
    alias fudns='sudo dscacheutil -flushcache; sleep 2; sudo killall -HUP mDNSResponder;'
    # i think this is added by the multi-user nix installer
    # it adds ~/.nix-profile to PATH (plus does other stuff, presumably)
    source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
