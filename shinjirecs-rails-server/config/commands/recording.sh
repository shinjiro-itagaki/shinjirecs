#!/usr/bin/env sh
ch=$1
sec=$2
out=$3
(recpt1 --b25 --strip ${ch} ${sec} ${out} 1>&2) &
# (recpt1 --b25 --strip ${ch} ${sec} ${out}) &
echo $!
wait $!
