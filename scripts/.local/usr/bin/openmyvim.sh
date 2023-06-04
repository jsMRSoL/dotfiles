#!/bin/bash

alacritty -e tmux-sessionizer.sh home &
sleep 1
tmux send-key -t home: 'myvim' 'c-m'
