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

  case $ext in
    gif)
      convert "$var"[5] \
        -thumbnail @62500 \
        -gravity center \
        -extent 250x250 \
        thumbs/"$var"-thumb.jpg 2> /dev/null
      ;;
    mp4 | MP4 | mov | MOV | mkv)
      ffmpegthumbnailer -i "$var" \
        -s 300 \
        -o thumbs/"$var"-del.jpg 2> /dev/null
      convert thumbs/"$var"-del.jpg \
        -thumbnail @62500 \
        -gravity center \
        -extent 250x250 \
        thumbs/"$var"-thumb.jpg 2> /dev/null
      rm thumbs/"$var"-del.jpg 2> /dev/null
      ;;
    jpg | jpeg | JPG | png)
      convert "$var" \
        -thumbnail @62500 \
        -gravity center \
        -extent 250x250 \
        thumbs/"$var"-thumb.jpg 2> /dev/null
      ;;
    webp)
      if [[ $(grep -c 'ANMF' "$var") -gt 0 ]]; then
        webpmux -get frame 10 "$var" -o thumbs/"$var"-del.webp
      else
        cp "$var" thumbs/"$var"-del.webp
      fi

      convert thumbs/"$var"-del.webp \
        -thumbnail @62500 \
        -gravity center \
        -extent 250x250 \
        thumbs/"$var"-thumb.jpg 2> /dev/null
      rm thumbs/"$var"-del.webp 2> /dev/null
      ;;
    webm)
      ffmpegthumbnailer -i "$var" \
        -s 300 \
        -o thumbs/"$var"-del.jpg 2> /dev/null
      convert thumbs/"$var"-del.jpg \
        -thumbnail @62500 \
        -gravity center \
        -extent 250x250 \
        thumbs/"$var"-thumb.jpg 2> /dev/null
      rm thumbs/"$var"-del.jpg 2> /dev/null
      ;;
  esac

done

# step four : remove unwanted images
for image in thumbs/*-0.jpg; do
  mv "$image" "${image/-0.jpg/.jpg}" 2> /dev/null
done
for image in thumbs/*-thumb-*.jpg; do
  rm "$image" 2> /dev/null
done

# exit
exit 0
