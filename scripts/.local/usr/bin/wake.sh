#!/bin/bash

#dmenu function
ask() {
x=$(echo -e "ant\nderek" | dmenu -i -p "Wake which machine?")

ping -q -c 1 "$x"
PING=$?

if [[ $PING -eq 0 ]]; then
    notify-send "$x is already online"
    exit 0
fi

case "$x" in
    ant) wakeonlan 00:1F:D0:69:B9:DD ;;
    derek) wakeonlan 5C:F9:DD:E6:E4:2D ;;
esac

for run in {1..10}
    do
        sleep 3
        ping -q -c 1 "$x"
        PING=$?
        if [[ $PING -eq 0 ]]; then
            notify-send "$x is now online"
            exit 0
        fi
    done

notify-send "$x has not come online"
}

ask
