#!/usr/bin/env sh
ch=$1
timeout_sec=$2
tempfile=$3
timeout ${timeout_sec} recpt1 --b25 --strip ${ch} 1 ${tempfile}
