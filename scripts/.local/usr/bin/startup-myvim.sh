#!/bin/bash

source "$HOME/.config/bash/tmux_running.sh"

launch() {
  tmux new-window -t home -n MyVim && \
    tmux send-keys -t :MyVim 'myvim && exit' Enter
}

tmux_running && launch
