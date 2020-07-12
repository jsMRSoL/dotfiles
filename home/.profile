# ~/profile: user-specific .profile file for the Bourne shell (sh(1))
# and Bourne compatible shells (bash(1), ksh(1), ash(1), ...).

PATH="/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games"

BASHFILES="/home/simon/.dotfiles/bash/.config/bash"
[ -f "$BASHFILES/shell_settings" ] && source "$BASHFILES/shell_settings"
