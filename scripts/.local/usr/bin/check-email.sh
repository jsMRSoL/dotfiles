#!/bin/sh
if /usr/local/bin/mbsync Yahoo
then
    echo "$(date): mbsync ran" >> $HOME/tmp/mbsync.log
    notify-send "Email updated!"
    # mu index -m $HOME/.mail/Yahoo
else
    echo "$(date): mbsync failed!" >> $HOME/tmp/mbsync.log
    notify-send "Email update failed!"
fi
