if [ "$(yadm config --get local.class)" = "WORK" ]; then
    DAY_JOB=true
else
    DAY_JOB=false
fi

if [ $DAY_JOB = true ]; then
    # added by Snowflake SnowSQL installer v1.2
    export PATH=/Applications/SnowSQL.app/Contents/MacOS:$PATH
else
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

export PATH="$PATH:$HOME/.local/bin"

