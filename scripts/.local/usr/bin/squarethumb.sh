#!/bin/bash

# step zero : check for args. If none, give help.
[[ "$#" -eq 0 ]] && echo -e "
squarethumb.sh takes a list of files and makes thumbnails in a
subdirectory 'thumbs'.

Example usage:
squarethumb.sh file1.jpg file2.png file3.mp4
" && exit 0

# step one : create new directory for thumbnails
[[ -d thumbs ]] || mkdir thumbs

for var in "$@"; do
    ext="${var##*.}"
    # echo "$var"
    # echo "$ext"

    case $ext in
        webp | webm | gif| mp4 )
            convert $var[0] \
                    -resize 200 \
                    -crop 250x250 \
                    -gravity center \
                    -extent 250x250 \
                    thumbs/$var-thumb.jpg 2>/dev/null;;
        jpg | jpeg | png )
            convert $var \
                    -resize 200 \
                    -crop 250x250 \
                    -gravity center \
                    -extent 250x250 \
                    thumbs/$var-thumb.jpg 2>/dev/null;;
     esac

done

# step four : remove unwanted images
for image in thumbs/*-1.jpg; do
    rm $image 2>/dev/null
done
for image in thumbs/*-0.jpg; do
    mv $image ${image/-0.jpg/.jpg} 2>/dev/null
done
