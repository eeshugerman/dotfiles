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
    alias nix-gc="nix-collect-garbage --delete-older-than 5d"

	  # TODO: switch to nix https://github.com/noctuid/dotfiles/blob/94c6f3e8a/nix/overlays/emacs.nix
    function upgrade-emacs {
        brew update \
            && brew uninstall emacs-plus@30 \
            && install emacs-plus@30 \
            && sudo rm -f /Applications/Emacs.app \
            && sudo osascript -e 'tell application "Finder" to make alias file to posix file "/usr/local/opt/emacs-plus@30/Emacs.app" at POSIX file "/Applications" with properties {name:"Emacs.app"}'

        # https://github.com/d12frosted/homebrew-emacs-plus/issues/742#issuecomment-2449092291
        sudo codesign --force --deep --sign - /usr/local/opt/emacs-plus@30/Emacs.app
    }
else
    alias open="xdg-open"
    alias cbcopy="xclip -in -selection clipboard"
    alias cbpaste="xclip -out -selection clipboard"

    alias sudo="sudo " # https://askubuntu.com/a/22043
    alias nixos-rebuild-test="sudo nixos-rebuild test --impure --flake ~/.config/nixos"
    alias nixos-rebuild-switch="sudo nixos-rebuild switch --impure --flake ~/.config/nixos"
    alias nixos-print-diffs="nix store diff-closures \$(ls -t1d /nix/var/nix/profiles/system-*-link | head -2 | tac)"
    alias nixos-gc="nix-collect-garbage --delete-older-than 5d && sudo nix-collect-garbage --delete-older-than 5d"

    alias dconf-dump="dconf dump / | vim -R -c 'set ft=dosini'"
    alias dconf-edit="vim $HOME/.config/dconf-user.conf"
    alias dconf-load="dconf load / < $HOME/.config/dconf-user.conf"
fi



function rsync-to-kodi {
    rsync --recursive --verbose --progress $1 "kodi:${2:-/media/kodi-media-usb/misc}"
}

function rsync-backup {
    rsync --archive --recursive --verbose --progress $1 $2
}

# https://github.com/TheLocehiliosan/yadm/issues/33
export GPG_TTY=$(tty)


if [[ "$INSIDE_EMACS" ]]; then
    # use evil instead of zsh's vi emulation
    bindkey -e
    # https://elpa.nongnu.org/nongnu-devel/doc/eat.html#Shell-Integration
    [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/zsh"
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
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

function git-cleanup {
    git fetch --prune --prune-tags
    git maintenance run --task gc # docs say to not run this at same time as loose-objects
    git maintenance run --task loose-objects --task incremental-repack --task pack-refs
}
