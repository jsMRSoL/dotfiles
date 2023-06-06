#!/bin/bash

webcli() { w3m "$1" ;}
webgui() { waterfox "$1" 2&>/dev/null & disown ;}
vidplay() { mpv --no-terminal "$1" 2&>/dev/null & disown ;}
gifview() { sxiv -a -s f -- "$1" 2&>/dev/null & disown ;}
pdfview() { mupdf "$1" 2&>/dev/null & disown ;}
filedl() { wget "$1" 2&>/dev/null & disown ;}
imageview() { feh -. "$1" & disown ;}
ytdl() { youtube-viewer "$1" & disown ;}
saveplay() { ytsp "$1" ;}

# dmenu function
# The URL will be shown visually in 30 characters or less.
ask() {
x=$(echo -e "clip\nw3m\nwaterfox\nmpv\nmupdf\nwget\nsaveplay\nsxiv\nytdl" | dmenu -p "How to open?")

case "$x" in
    clip) printf '%s' "$1" | xclip -selection clipboard ;;
	w3m) webcli "$1" ;;
	waterfox) webgui "$1" ;;
	mpv) vidplay "$1" ;;
	feh) imageview "$1" ;;
	mupdf) pdfview "$1" ;;
	wget) filedl "$1" ;;
	sxiv) gifview "$1" ;;
	ytdl) ytdl "$1" ;;
    saveplay) ytsp "$1" ;;
esac
}

# Get the extension and try to decide what to do.
# If all else fails, ask the user.
# ext="${1##*.}"
# mpvFiles="mkv mp4 webm"
# imageFiles="png jpg jpeg jpe"
# gifFiles="gif"
# wgetFiles="mp3 flac opus mp3?source=feed"
# pdfFiles="pdf"

# if echo $imageFiles | grep -w $ext > /dev/null; then
# 	imageview "$1"
# elif echo $gifFiles | grep -w $ext > /dev/null; then
# 	gifview "$1"
# elif echo $mpvFiles | grep -w $ext > /dev/null; then
# 	vidplay "$1"
# elif echo $wgetFiles | grep -w $ext > /dev/null; then
# 	filedl "$1"
# elif echo $pdfFiles | grep -w $ext > /dev/null; then
# 	pdfview "$1"
# elif [[ "$1" == *"youtu"* ]]; then
# 	ytdl "$1"
# else
# 	ask "$1" 
# fi
ask "$1"
