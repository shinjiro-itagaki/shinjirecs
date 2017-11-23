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
# sleep 100
if type recpt1 ; then
    echo "" > /dev/null; # dummy
else
    exit 1; # error
fi
recpt1 --b25 --strip ${ch} ${sec} ${tmpfile}

if [ -f ${tmpfile} ]; then
#    echo "success"
    exit 0 # success
else
#    echo "error"
    exit 1 # error
fi
