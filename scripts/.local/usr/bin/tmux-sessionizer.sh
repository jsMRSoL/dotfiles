#!/bin/bash

# Is tmux running?
running=false
if tmux run 2>/dev/null; then
  running=true
fi

# Do we already know what session we want?
if [[ $# -eq 1 ]]; then
    session="$1"
fi

# Do we want an already running session?
if [[ -z $session ]] && $running; then
  session=$( tmux list-sessions | cut -d: -f1 | fzf --prompt='Pick session or C-c for new session: ')
fi

# Then pick a folder for a new session.
if [[ -z $session ]]; then
  # dir=$( fd --type d -H -d 1 . ~/ | fzf --prompt='Pick branch: ')
  # [[ -z $dir ]] && exit 0
  # path=$( fd --type d -HL . $dir | fzf --prompt="Pick subfolder or C-c to use $dir: " )
  path=$( fd --type d -HL . ~/ | fzf --prompt="Pick folder or C-c to cancel: " )
  [[ -z $path ]] && exit 0
  # session=$(basename "${path:-$dir}" | tr . _ )
  session=$(basename "$path" | tr . _ )
  [[ -z $path ]] && path=$dir
fi

# Create the session if it doesn't exist...
if ! tmux has-session -t $session >/dev/null 2>&1; then
  tmux new-session -s $session -d -c $path
fi

# ...and switch to it.
attached=$( tmux list-sessions -F '#{session_attached} #{session_name}' | grep ^1 )
# if [[ -n $TMUX ]]; then 
# the above test works only if the script is called from within the attached client
if [[ -n $attached ]]; then
# whenever a client is attached, we switch with switch-client.
# this method allows this script to be called (with an argument)
# from dmenu.
  tmux switch-client -t $session
else
  tmux attach -t $session
  # If tmux is not attached to a session, that means
  # it's not open. So spawn a terminal, run tmux and attach.
  # exec alacritty -e tmux attach -t $session &
fi

exit 0
