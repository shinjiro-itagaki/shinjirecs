#!/usr/bin/env sh
ch=$1
sec=$2
out=$3

echo $$
touch ${out}
echo "recording for test channel="${ch}
sleep ${sec}
