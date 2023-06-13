#!/bin/bash

tmux_running() {
  if ! tmux run 2> /dev/null; then
    return 1
  fi
  # return true
  return 0
}

tmux_attached() {
  attached="$(tmux list-sessions -F \
    '#{session_attached} #{session_name}' | grep ^1)"
  if [[ -n $attached ]]; then
    # return true
    return 0
  else
    return 1
  fi
}

tmux_ready() {
  if tmux_running && tmux_attached; then
    return 0
  else
    exec alacritty -e tmux-sessionizer.sh home &
    for _i in {1..50}; do
      if tmux has-session -t home 2> /dev/null; then
        # good to go
        return 0
      fi
      sleep 0.1
    done
    return 1
  fi
}

tmux_window_exists() {
  exists="$(tmux list-windows -F \
    '#{session_attached} #{window_name}' | grep "$1")"
  if [[ -n $exists ]]; then
    return 0
  fi
  return 1
}
