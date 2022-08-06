if [ $DAY_JOB = true ]; then
    # added by Snowflake SnowSQL installer v1.2
    export PATH=/Applications/SnowSQL.app/Contents/MacOS:$PATH
else
    # i don't think this is necessary because the same thing is in /etc/profile.d/guix.sh
    # but the docs say to add it :shrug:
    GUIX_PROFILE="$HOME/.guix-profile"
    . "$GUIX_PROFILE/etc/profile"

    # docs say to add this to. it's not present in /etc/profile.d/guix.sh.
    GUIX_PROFILE="$HOME/.config/guix/current"
    . "$GUIX_PROFILE/etc/profile"
fi

export PATH="$PATH:$HOME/.local/bin"

