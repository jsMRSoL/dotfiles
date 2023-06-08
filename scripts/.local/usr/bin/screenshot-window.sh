win_id="$( xwininfo -wm -int | grep 'Window id:' | awk -F' ' '{print $4}')"
scrot -w "$win_id" "%Y-%m-%d_%H:%M:%S.png" -e 'mv $f ~/Pictures/screenshots'
