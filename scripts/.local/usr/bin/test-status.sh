#!/bin/bash

INT="$(get-internet-connectivity)"
VOL="$(get-system-volume)"
IP=$(for i in $(ip r); do echo $i; done | grep -A 1 src | tail -n1) # can get confused if you use vmware
TEMP="$(get-system-temperature)"
LOCALTIME="$(get-local-time)"
BAT="$(get-battery)"

echo " $INT $IP | $BAT | $VOL | $TEMP | $LOCALTIME"

# 192.168.1.114
