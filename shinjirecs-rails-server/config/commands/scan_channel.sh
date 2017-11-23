#!/usr/bin/env sh
ch=$1
sec=$2
if [ "$sec" = "" ]
then
    sec=1
fi
recpt1 --b25 --strip ${ch} ${sec} /dev/null
