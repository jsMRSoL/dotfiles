#!/bin/bash
MYHOME='/home/simon/.config'
GITVAR='/home/simon/.dotfiles'

# config files under home
    cp -p $HOME/.inputrc         $GITVAR/home/inputrc
    cp -p $HOME/.bash_aliases    $GITVAR/home/bash_aliases
    cp -p $HOME/.bashrc          $GITVAR/home/bashrc
    cp -p $HOME/.selected_editor $GITVAR/home/selected_editor
    cp -p $HOME/.profile         $GITVAR/home/profile

# configs to start window manager
    cp -p /usr/share/xsessions/dwm.desktop $GITVAR/various/dwm.desktop
    cp -p /usr/local/bin/dwmstart          $GITVAR/various/dwmstart

# customized source files
    cp -p $HOME/.dwmsrc/config.h         $GITVAR/dwm/
    cp -p $HOME/.dwmsrc/config.mk        $GITVAR/dwm/
    cp -p $HOME/.stsrc/config.h          $GITVAR/st/

# configs for terminal applications
    cp -p $MYHOME/mpd/mpd.conf          $GITVAR/mpd/
    cp -p $MYHOME/mps-youtube/transcode $GITVAR/mps-youtube/
    cp -p $MYHOME/ncmpcpp/config        $GITVAR/ncmpcpp/
    cp -p $MYHOME/ncmpcpp/bindings      $GITVAR/ncmpcpp/
    cp -p $MYHOME/newsbeuter/config     $GITVAR/newsbeuter/
    cp -p $MYHOME/newsbeuter/urls       $GITVAR/newsbeuter/
    cp -p $MYHOME/ranger/commands.py    $GITVAR/ranger/
    cp -p $MYHOME/ranger/rc.conf        $GITVAR/ranger/
    cp -p $MYHOME/ranger/rifle.conf     $GITVAR/ranger/
    cp -p $MYHOME/ranger/scope.sh       $GITVAR/ranger/
    cp -p $MYHOME/redshift.conf         $GITVAR/redshift/
    cp -p $MYHOME/rtv/rtv.cfg           $GITVAR/rtv/
    cp -p $MYHOME/surfraw/conf          $GITVAR/surfraw/
    cp -p $MYHOME/tmux/tmux.conf        $GITVAR/tmux/
    cp -p $MYHOME/urlview               $GITVAR/urlview/
    cp -p $MYHOME/w3m/config            $GITVAR/w3m/
    cp -p $MYHOME/w3m/keymap            $GITVAR/w3m/

# Get all vim files except downloadable ones
for file in $MYHOME/vim/*; do
    if [ -d "$file" ]; then
        if [[ ! "$file" == *"bundle"* ]]; then
            cp -pr "$file" $GITVAR/vim/
        fi
    else
        cp -pr "$file" $GITVAR/vim/
    fi
done

# Copy all my scripts
    cp -p $HOME/.local/usr/bin/*        $GITVAR/scripts/

# Finish
    echo "Backup finished..."
    exit 0
