#!/bin/bash

# step zero : check for args. If none, give help.
[[ $# -gt 0 ]] && echo -e "
dirtothumbs.sh does not take any arguments. It finds files in the current
working directory and makes thumbnails in a subdirectory 'thumbs'.

Example usage:
dirtothumbs.sh
" && exit 0

# step one : create new directory for thumbnails
[[ -d thumbs ]] || mkdir thumbs

# step two a: resize mp4s, etc.
for image in *.webp *.webm *.gif *.mp4; do
    convert $image[0] \
        -resize 200 \
        -crop 250x250 \
        -gravity center \
        -extent 250x250 \
        thumbs/$image-thumb.jpg 2>/dev/null
done

# step three : resize jpgs, etc.
for image in *.jpg *.jpeg *.png; do
    convert $image \
        -resize 200 \
        -crop 250x250 \
        -gravity center \
        -extent 250x250 \
        thumbs/$image-thumb.jpg 2>/dev/null
done

# step four : remove unwanted images
for image in thumbs/*-1.jpg; do
    rm $image 2>/dev/null
done
for image in thumbs/*-0.jpg; do
    mv $image ${image/-0.jpg/.jpg} 2>/dev/null
done
