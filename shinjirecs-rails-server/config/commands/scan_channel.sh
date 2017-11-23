#!/usr/bin/env sh
ch=$1
sec=$2

if [ "$sec" = "" ]
then
    sec=1
fi
tmpfile=$(mktemp)
# echo ${tmpfile}
rm ${tmpfile}
recpt1 --b25 --strip ${ch} ${sec} ${tmpfile}

if [ -f ${tmpfile} ]; then
#    echo "success"
    exit 0 # success
else
#    echo "error"
    exit 1 # error
fi
