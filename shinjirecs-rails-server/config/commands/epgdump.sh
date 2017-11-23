#!/usr/bin/env sh
ch=$1
sec=$2
if [ "$sec" = "" ]
then
    sec=10
fi

recpt1 ${ch} ${sec} - | epgdump - -
