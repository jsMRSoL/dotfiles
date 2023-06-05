#!/bin/bash

alacritty -e tmux-sessionizer.sh home &

for i in {1..10}
do
  if tmux has-session -t home
  then
    tmux send-key -t home: 'myvim' 'c-m'
    break
  fi
  sleep 0.2
done

exit 0
