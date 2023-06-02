#!/bin/bash

session="$1"
in_tmux=false

if ! tmux has-session -t $session >/dev/null 2>&1; then
  tmux new-session -s $session -d
fi

if [[ -n $TMUX ]]; then
  tmux switch-client -t $session
else
  tmux attach -t $session
fi

exit 0
