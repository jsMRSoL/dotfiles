#!/bin/bash

source "$HOME/.config/bash/tmux_running.sh"

# Do we already know what session we want?
if [[ $# -eq 1 ]]; then
  session="$1"
fi

# Do we want an already running session?
if [[ -z $session ]] && tmux_running; then
  session=$(tmux list-sessions | cut -d: -f1 | fzf --prompt='Pick session or C-c for new session: ')
fi

# If not, then pick a folder for a new session.
if [[ -z $session ]]; then
  path=$(fd --type d -HL . ~/ | fzf --prompt="Pick folder or C-c to cancel: ")
  [[ -z $path ]] && exit 0
  session=$(basename "$path" | tr . _)
  [[ -z $path ]] && path=$dir
fi

# Create the session if it doesn't exist...
if ! tmux has-session -t $session > /dev/null 2>&1; then
  tmux new-session -s $session -d -c $path
fi

# ...and switch to it.
if tmux_attached; then
  # whenever a client is attached, we switch with switch-client.
  # this method allows this script to be called (with an argument)
  # from dmenu.
  tmux switch-client -t $session
else
  # This needs to be run in a terminal, and tmux will start in that terminal.
  # tmux-sessionizer is used in the tmux_ready function.
  # Will this code ever be reached in practice?
  tmux attach -t $session
fi

exit 0
