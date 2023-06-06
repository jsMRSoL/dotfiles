#!/bin/bash

tmux_running() {
  if ! tmux run 2>/dev/null
  then
    exec alacritty -e tmux-sessionizer.sh home &
    for i in {1..10}
    do
      if tmux has-session -t home 2>/dev/null
      then
        # return true
        return 0
      fi
      sleep 0.2
    done
    # return false
    return 1
  fi
  # return true
  return 0
}
