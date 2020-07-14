#!/bin/bash
file="/home/simon/.vim/bundle/vim-clc-lookup/latin-analyses.txt"
#echo "$1"
search='/^'"$1"'\t/{n = split($0, a, "{"); for (i = 1; ++i <= n;) print a[i]}'
#echo "$search"
awk "$search" $file | awk '{print $3}' | cut -d, -f2 | cut -d# -f1 | uniq
