#!/usr/bin/env sh
ch=$1
sec=$2

if [ "$sec" = "" ]
then
    sec=1
fi

# sleep 100
# echo "recpt1 --b25 --strip ${ch} ${sec} ${tmpfile}"
if type recpt1 ; then
    echo "" > /dev/null; # dummy
else
    exit 1; # error
fi

tmpfile=$(mktemp)
# echo ${tmpfile}
rm ${tmpfile}

if [ -f ${tmpfile} ]; then
    #    echo "success"
    echo "recpt1 --b25 --strip ${ch} ${sec} ${tmpfile}"
    exit 0 # success
else
#    echo "error"
    exit 1 # error
fi
