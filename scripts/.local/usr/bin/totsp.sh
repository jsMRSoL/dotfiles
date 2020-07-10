#!/bin/bash
notify-send "Sending $1 to tsp..."
tsp youtube-dl -f 18/21/medium/best -o '/home/simon/Videos/%(title)s_%(id)s.%(ext)s' -- "$1"


