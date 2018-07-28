#!/usr/bin/env sh
ch=$1
timeout_sec=$2
tempfile=$3
if type recpt1; then
    echo "" > /dev/null
else
    exit 1; # not scaned scan was not done
fi
timeout ${timeout_sec} recpt1 --b25 --strip ${ch} 1 ${tempfile}
