#!/bin/bash

dl-it() {
	case "$1" in
	*"youtube"*) location=\'"$2/"'%(title)s-%(id)s.%(ext)s'\' ;;
	*) location="$2/$(basename $3)" ;;
	esac

	notify-send "Executing  \"$1 $location $3\""
	bash -c "tsp $1 $location $3"
}

# dmenu function
# The URL will be shown visually in 30 characters or less.
x=$(echo -e "wget\nyt-video\nyt-audio" | dmenu -i -p "How to download? ")

case "$x" in
wget) myfunc="wget -O" ;;
yt-video) myfunc="youtube-dl --config-location ~/.config/youtube-dl/ytv \
        --output " ;;
yt-audio) myfunc="youtube-dl --config-location ~/.config/youtube-dl/yta \
        --output " ;;
esac

choice=$(find "/home/simon" -maxdepth 3 -name ".*" -prune -o -type d -print | dmenu -i -p "Save location? ")
case "$choice" in
/home/simon/*) location="$choice" ;;
*) choice="/home/simon" ;;
esac

dl-it "$myfunc" "$location" "$1"
exit 0
