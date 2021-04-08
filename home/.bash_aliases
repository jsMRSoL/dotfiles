# Set vi mode 
set -o vi
bind -m vi-command ".":insert-last-argument
bind -m vi-command '"\e-":yank-nth-arg'
bind -m vi-insert '"\e=":edit-and-execute-command'

BASHFILES="/home/simon/.dotfiles/bash/.config/bash"
[ -f "$BASHFILES/shell_settings" ] && source "$BASHFILES/shell_settings"
[ -f "$BASHFILES/bash_functions" ] && source "$BASHFILES/bash_functions"

# api keys
[ -f "$HOME/.api_keys" ] && source "$HOME/.api_keys"

# python virtual environment functionality
PY_VE_WRAPPER="$HOME/.local/bin/virtualenvwrapper.sh"
[ -f "$PY_VE_WRAPPER" ] && source "$PY_VE_WRAPPER"
