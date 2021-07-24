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
for image in *.gif *.mp4; do
    convert $image[60] \
        -resize 200 \
        -crop 250x250 \
        -gravity center \
        -extent 250x250 \
        thumbs/$image-thumb.jpg 2>/dev/null
done

# step two b: handle webm files
for image in *.webm; do
    ffmpegthumbnailer -i $image \
        -s 200 \
        -o thumbs/$image-del.jpg 2>/dev/null
    convert thumbs/$image-del.jpg \
        -crop 250x250 \
        -gravity center \
        -extent 250x250 \
        thumbs/$image-thumb.jpg 2>/dev/null
    rm thumbs/$image-del.jpg 2>/dev/null
done

# step two c: handle both types of webp files
for image in *.webp; do
    if [[ $(grep -c 'ANMF' $image) -gt 0 ]]
    then
        webpmux -get frame 10 $image -o thumbs/$image-del.webp
    else
        cp $image thumbs/$image-del.webp
    fi

    convert thumbs/$image-del.webp \
        -resize 200 \
        -crop 250x250 \
        -gravity center \
        -extent 250x250 \
        thumbs/$image-thumb.jpg 2>/dev/null
    rm thumbs/$image-del.webp 2>/dev/null
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
for image in thumbs/*-0.jpg; do
    mv $image ${image/-0.jpg/.jpg} 2>/dev/null
done
for image in thumbs/*-thumb-*.jpg; do
    rm $image 2>/dev/null
done
