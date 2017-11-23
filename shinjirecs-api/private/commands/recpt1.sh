#!bin/sh
# $1 : device path(ex. /dev/pt3video0)
# $2 : physical channel number
# $3 : seconds
# $4 : dest filepath
recpt1 --b25 --strip --device $1 $2 $3 $4
