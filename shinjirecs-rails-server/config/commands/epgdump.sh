#!/usr/bin/env sh

# usage
# epgdump.sh ./in.ts ./temp.json
#   or
# epgdump.sh 13      ./temp.json 10
in_=$1
out=$2

if [ -f ${in_} ] ; then
    epgdump json ${in_} ${out}
else # ${in} is channel number
    ch=$1
    sec=$3
    if [ "$sec" = "" ]
    then
        sec=10
    fi
    recpt1 ${ch} ${sec} - | epgdump json - ${out}
fi           
