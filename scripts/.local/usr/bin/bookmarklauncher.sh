#!/bin/bash
file="/home/simon/.bookmarks"
[ ! -e $file ] && notify-send "Cannot find bookmarks file" && exit
choice=$( awk '{print $1}' $file | dmenu -l 10 -p "Choose bookmark: ")
# echo "Choice was: $choice"
[ -z "$choice" ] && exit
search='/^'"$choice"'/{$1 = ""; print $0}'
# echo "Searching pattern is: $search"
target=$( awk "$search" "$file" )
# echo "Site is: $target"
# printf "%s" "$choice"
BROWSER="firefox"
[ -z "$target" ] && cmd=$(printf "%s \"https://duckduckgo.com/?q=%s\"" "$BROWSER" "$choice")
[ -n "$target" ] && cmd=$(printf "%s %s" "$BROWSER" "$target")
# echo "$cmd"
/bin/bash -c "$cmd" & disown
