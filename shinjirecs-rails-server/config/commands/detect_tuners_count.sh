#!/usr/bin/env sh
gr=0 
bs=0
cur_board=0
max_board=100
cur_tuner=0
devname="/dev/pt3video"
# if using driver is destributed by
#   https://github.com/m-tsudo/pt3.git
while [ $cur_board -lt $max_board ]
do
    old_bs=0
    old_gr=0
    cur_tuner=$((cur_board*4))
    if [ -f ${devname}$((cur_tuner)) ]; then
	$((bs+=1))
    fi
    if [ -f ${devname}$((cur_tuner+1)) ]; then
	$((bs+=1))
    fi
    if [ -f ${devname}$((cur_tuner+2)) ]; then
	$((gr+=1))
    fi
    if [ -f ${devname}$((cur_tuner+3)) ]; then
	$((gr+=1))
    fi
    if [ $old_bs = $bs -a $old_gr = $gr ]; then
	break;
    fi
    $((cur_board+=1));
done
echo "{\"gr\": "${gr}", \"bs\": "${bs}"}"
# bs /dev/pt3video0 /dev/pt3video1 /dev/pt3video4 /dev/pt3video5
# gr /dev/pt3video2 /dev/pt3video3 /dev/pt3video6 /dev/pt3video7
