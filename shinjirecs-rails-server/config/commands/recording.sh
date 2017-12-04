#!/usr/bin/env sh
ch=$1
sec=$2
out=$3
sid=$4
opt_sid="--sid ${sid}"
ffmpeg_opts=$5

if [ "${sid}" = "" ] ; then
    opt_sid=""
fi

(recpt1 ${opt_sid} --b25 --strip ${ch} ${sec} ${out} 1>&2) &
pid=$!
echo $pid
wait $pid      
