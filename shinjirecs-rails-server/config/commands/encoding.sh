#!/usr/bin/env sh
inn=$1
options=$2
out=$3

if [ "${inn}" = "" ]; then
    $inn=/dev/stdin
fi

if [ "${out}" = "" ]; then
    $out=${inn}.mpeg
fi

ffmpeg -y ${options} -i ${inn} ${out}
