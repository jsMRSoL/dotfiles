#!/bin/bash
#
j4-dmenu-desktop --dmenu="dmenu \
  -g 5 \
  -l 10 \
  -p 'Select application: ' \
  -fn 'Firacode Nerd Font-15' \
  -nf '#cdd6f4' \
  -nb '#1e1e2e' \
  -sf '#D8DEE9' \
  -sb '#626880' \
  " --no-generic --term="alacritty"