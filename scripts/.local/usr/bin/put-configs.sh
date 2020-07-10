#!/bin/bash
MYHOME='/home/simon/.config'
GITVAR='/home/simon/.dotfiles'

# config files under home
    cp -p $GITVAR/home/bash_aliases    $HOME/.bash_aliases 
    cp -p $GITVAR/home/bashrc          $HOME/.bashrc 
    cp -p $GITVAR/home/inputrc         $HOME/.inputrc 
    cp -p $GITVAR/home/profile         $HOME/.profile 
    cp -p $GITVAR/home/selected_editor $HOME/.selected_editor 

# configs to start window manager
    cp -p $GITVAR/various/dwm.desktop /usr/share/xsessions/dwm.desktop 
    cp -p $GITVAR/various/dwmstart    /usr/local/bin/dwmstart  

# customized source files
    cp -p $GITVAR/dwm/config.h  $HOME/.dwmsrc/
    cp -p $GITVAR/dwm/config.mk $HOME/.dwmsrc/
    cp -p $GITVAR/st/config.h   $HOME/.stsrc/

# configs for terminal applications
    cp -p $GITVAR/mpd/mpd.conf           $MYHOME/mpd/
    cp -p $GITVAR/mps-youtube/transcode  $MYHOME/mps-youtube/
    cp -p $GITVAR/ncmpcpp/*              $MYHOME/ncmpcpp/
    cp -p $GITVAR/newsbeuter/*           $MYHOME/newsbeuter
    cp -p $GITVAR/ranger/*               $MYHOME/ranger/
    cp -p $GITVAR/redshift/redshift.conf $MYHOME/
    cp -p $GITVAR/rtv/rtv.cfg            $MYHOME/rtv/
    cp -p $GITVAR/surfraw/conf           $MYHOME/surfraw/
    cp -p $GITVAR/tmux/tmux.conf         $MYHOME/tmux/
    cp -p $GITVAR/urlview/urlview        $MYHOME/
    cp -pr $GITVAR/vim/*                 $MYHOME/vim/
    cp -p $GITVAR/w3m/*                  $MYHOME/w3m/

# myscripts
    cp -p $GITVAR/scripts/*              $HOME/.local/usr/bin/

# Create links 
    ln -sT $MYHOME/newsbeuter            $HOME/.newsbeuter
    ln -sT $MYHOME/tmux/tmux.conf        $HOME/.tmux.conf
    ln -sT $MYHOME/urlview               $HOME/.urlview
    ln -sT $MYHOME/vim                   $HOME/.vim
    ln -sT $MYHOME/w3m                   $HOME/.w3m

# Finish
    echo "All dotfiles copied to ~/.config/"
    echo "Links made to ~/"
    exit 0
