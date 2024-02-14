# ~/profile: user-specific .profile file for the Bourne shell (sh(1))
# and Bourne compatible shells (bash(1), ksh(1), ash(1), ...).

PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/local/games:/usr/games:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl"
# BASHFILES="/home/simon/.dotfiles/bash/.config/bash"
# [ -f "$BASHFILES/shell_settings" ] && source "$BASHFILES/shell_settings"
################################################################################
## PATH
################################################################################
## user's private bin
if [ -d "$HOME/.local/usr/bin" ] ; then
    PATH="$HOME/.local/usr/bin:$PATH"
fi
## set PATH so it includes .local/bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
## cargo
if [ -d "$HOME/.cargo/bin" ] ; then
    PATH="$HOME/.cargo/bin:$PATH"
fi
## luarocks
if [ -d "$HOME/.luarocks/bin" ] ; then
    PATH="$HOME/.luarocks/bin:$PATH"
fi
## surfraw. To remove use surfraw-update-path -remove
    PATH="$PATH:/usr/lib/surfraw"
## doom emacs
    PATH="$PATH:$HOME/.emacs.d/bin"
## npm
    PATH="$PATH:$HOME/.npm/bin"
export PATH
################################################################################
## End PATH
################################################################################
