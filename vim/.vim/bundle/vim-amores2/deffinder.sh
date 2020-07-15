#!/bin/bash
word="$1"
dictionary="/home/simon/.config/vim/bundle/vim-amores2/amores2vocab.txt"

awk -F ': ' '/^'"$1[^a-z]"'/ {print $2}' "$dictionary"


