if [ "$(yadm config --get local.class)" = "WORK" ]; then
    day_job=true
else
    day_job=false
fi

if [ $day_job = true ]; then
    # added by Snowflake SnowSQL installer v1.2
    alias snowsql=/Applications/SnowSQL.app/Contents/MacOS/snowsql
    export JAVA_HOME=/usr/local/Cellar/openjdk@11/11.0.12/libexec/openjdk.jdk/Contents/Home
    # https://github.com/immuta/bodata/blob/master/README.md#getting-started
    # TODO: is this still necessary? if not, remove from here, sudoers, and bodata readme
    sudo ifconfig lo0 alias 10.0.2.2
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

