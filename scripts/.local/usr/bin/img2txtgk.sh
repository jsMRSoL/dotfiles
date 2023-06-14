#!/bin/bash

# step one : convert to tif
count=0
for image in *.png; do
  count=$((count + 1))
  no=$(printf "%02d" $count)
  convert "$image" "$no.tif"
done

# # step two : unpaper
# for image in *.tif; do
#   unpaper "$image" "${image/.tif/-unp.tif}"
# done

# step three : ocr
# for page in *-unp.tif; do
for page in *.tif; do
  tesseract "$page" "${page/.tif/}" -l grc+eng
  rm "$page"
done

# step four : combine
for file in *.txt; do
  cat "$file" >> binder.txt
  rm "$file"
done

# rm ./*.tif

notify-send "Greek OCR" "$count page(s) ready!"
