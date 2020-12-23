#!/bin/bash

# step one : create new directory for thumbnails
[[ -d thumbs ]] || mkdir thumbs

# step two a: resize mp4s
for image in *.webp *.webm *.gif *.mp4; do
    convert $image[0] \
        -resize 200 \
        -crop 250x250 \
        -gravity center \
        -extent 250x250 \
        thumbs/$image-thumb.jpg
done

# step two b: resize jpgs
for image in *.jpg *.jpeg *.png; do
    convert $image \
        -resize 200 \
        -crop 250x250 \
        -gravity center \
        -extent 250x250 \
        thumbs/$image-thumb.jpg
done

# step three : crop
# for image in thumbs/*-del.jpg; do
#     echo "image: $image"
#     shortside=1000
#     height=$(identify -format %h $image)
#     echo "height: $height"
#     width=$(identify -format %w $image)
#     echo "width: $width"
#     if [ $height -lt $width ]
#     then
#        shortside=$height
#     fi
#     if [ $width -lt $height ]
#     then
#         shortside=$width
#     fi
#     echo "shortside: $shortside"
#     convert $image -crop "$shortside"x"$shortside" -gravity center ${image/-del.jpg/-thumb.jpg}
# done

# step four : remove unwanted images
for image in thumbs/*-1.jpg; do
    rm $image
done
for image in thumbs/*-0.jpg; do
    mv $image ${image/-0.jpg/.jpg}
done
