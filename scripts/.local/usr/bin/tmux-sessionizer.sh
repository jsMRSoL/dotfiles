#!/bin/bash

session="$1"
in_tmux=false

if !tmux has-session -t $session; then
  tmux new-session -s $session -d
fi

if [[ -z $TMUX ]]; then
  tmux switch-client -t $session
  return
else
  tmux attach -t $session
  return
fi


exit 0
