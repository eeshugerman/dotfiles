if [ "$(yadm config --get local.class)" = "WORK" ]; then
    DAY_JOB=true
else
    DAY_JOB=false
fi
