export PATH="$PATH:$HOME/.local/bin"

if [ "$(uname)" != "Darwin" ]; then
    # i don't think this is necessary because the same thing is in /etc/profile.d/guix.sh
    # but the docs say to add it :shrug:
    GUIX_PROFILE="$HOME/.guix-profile"
    if [[ -f "$GUIX_PROFILE" ]]; then
        . "$GUIX_PROFILE/etc/profile"
    fi

    # docs say to add this to. it's not present in /etc/profile.d/guix.sh.
    GUIX_PROFILE="$HOME/.config/guix/current"
    if [[ -f "$GUIX_PROFILE" ]]; then
        . "$GUIX_PROFILE/etc/profile"
    fi
fi
