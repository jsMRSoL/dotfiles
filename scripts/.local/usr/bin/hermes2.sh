#!/bin/bash
file="$1"
#echo "$1"
search='/^'"$2"'\t/{n = split($0, a, "{"); for (i = 1; ++i <= n;) print a[i]}'
#echo "$search"
awk "$search" $file | awk '{print $3}' | cut -d, -f2 | cut -d# -f1 | uniq
