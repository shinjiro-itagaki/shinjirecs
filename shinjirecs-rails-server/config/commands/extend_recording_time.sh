#!/usr/bin/env sh
pid=$1
sec=$2
# recpt1ctl --pid pid [--channel channel] [--sid SID1,SID2] [--extend time_to_extend] [--time recording_time]
timeout 10 recpt1ctl --pid ${pid} --extend ${sec}
